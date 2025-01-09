pacman::p_load(terra,data.table,tidyverse,sf, patchwork)

# config options
sf_use_s2(use = T)

## Load vector datasets using Terra =========================
# survey area
dsa <- vect("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
                   layer = "SURVEYED_AREAS_FLAT_AllYears_CONUS_Rgn3")
names(dsa) <- tolower(names(dsa))
dsa$area_ha <- terra::expanse(dsa, unit="ha")
dsa <- terra::project(dsa, terra::crs("epsg:4326"))

# damage area
dd <- vect("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
                  layer = "DAMAGE_AREAS_FLAT_Allyears_CONUS_Rgn3")
names(dd) <- tolower(names(dd))
dd$area_ha <- terra::expanse(dd, unit="ha")
dd <- terra::project(dd, terra::crs("epsg:4326"))



## Process Gridded datasets =====================
## list of all gridded data

fl_vi <- c("data/gee_data_AZ_tree_stress/mcd43a4_evi2_monthly_surveyArea_2000-6_2023-11.tif")

fl_gmd <- list.files("data/gee_data_AZ_tree_stress",
           pattern = "gridmetDrought",
           full.names = T)
fl_gmd <- fl_gmd[str_detect(fl_gmd, ".tif")]

fl_prism <- list.files("data/gee_data_AZ_tree_stress",
                     pattern = "PRISM_AN81m",
                     full.names = T)
fl_prism <- fl_prism[str_detect(fl_prism, ".tif")]


fn_format_rast <- function(fp){
  fp_csv <- str_replace(fp, pattern = ".tif", replacement = ".csv")
  r <- rast(fp)
  vec_dates <- fread(fp_csv)
  time(r) <- vec_dates$date
  names(r) <- time(r)
  return(r)
}

tmp_v <- fn_format_rast(fl_vi)

dd13 <- subset(dd, dd$survey_year >= 2013)
dd13$fobservation_id <- as.factor(dd13$observation_id)

# # tmp_v <- mask(tmp_v, dd13) ## slow!
# 
# ## hackish. to remove...
# if(is.na(all(time(tmp_v)))==TRUE){
#   time(tmp_v) <- as.Date(names(tmp_v))
# }

system.time(dd13_simp <- terra::simplifyGeom(dd13))

# rasterize from the Damage Area observation id
# slow!!!
system.time(grid_dd13 <- terra::rasterize(dd13_simp, 
                              tmp_v, 
                              field="fobservation_id") ## slow!
)

## combine "Damage Area" raster with MODIS VI to retain full VI time series
v <- c(grid_dd13, tmp_v) %>% 
  as.data.table(xy=T)

v2 <- melt(v, id.vars = 1:3, 
           variable.name = "time", 
           value.name = "evi2")
v2[is.na(fobservation_id)==F]
v2[,`:=`(id_pix = .GRP), by=.(x,y)]
v2[,`:=`(time = as.character(time))]
lut_time <- as.Date(unique(v2$time))
system.time(v2[,`:=`(time = lut_time[match(time, lut_time)])]) # Fast way to cast to date!
v2 <- v2[is.na(fobservation_id)==F]
dd_evi2 <- v2

coords <- unique(dd_evi2[,.(x,y,id_pix)])


fn_extract_gmd <- function(fp){
  ## function to extract GridMetDrought
  tmp_gmd <- fn_format_rast(fp)
  system.time(vv_gmd <- terra::extract(tmp_gmd, coords[,.(x,y)]))
  vv_gmd <- cbind(coords, vv_gmd[,-1])
  gmd_var <- str_extract(basename(fp),  "_([^_]+)_") %>%
    substr(., 2, nchar(.) - 1)
  vv_gmd2 <- melt(vv_gmd, id.vars = 1:3, 
             variable.name = "time", 
             value.name = gmd_var)
  lut_time <- as.Date(unique(vv_gmd2$time))
  vv_gmd2[,`:=`(time = as.character(time))]
  vv_gmd2[,`:=`(time = lut_time[match(time, lut_time)])] # Fast way to cast to date!
  return(vv_gmd2)
}

system.time(l_gmd <- lapply(fl_gmd, fn_extract_gmd))

fn_extract_prism <- function(fp){
  ## function to extract PRISM vars
  tmp_prism <- fn_format_rast(fp)
  tmp_prism <- tmp_prism[[month(time(tmp_prism)) %in% 6:10]] # subset to growing season
  # note: gmd was subset to growing season in GEE
  
  system.time(vv_prism <- terra::extract(tmp_prism, coords[,.(x,y)]))
  vv_prism <- cbind(coords, vv_prism[,-1])
  
  # get variable name 
  prism_var <- str_split(basename(fp),"_",simplify = T)[3]

  # cast to long
  vv_prism2 <- melt(vv_prism, id.vars = 1:3, 
                  variable.name = "time", 
                  value.name = prism_var)
  
  # fix time column
  lut_time <- as.Date(unique(vv_prism2$time))
  vv_prism2[,`:=`(time = as.character(time))]
  vv_prism2[,`:=`(time = lut_time[match(time, lut_time)])] # Fast way to cast to date!
  
  # speeds up joins
  setkeyv(vv_prism2, cols=c("x","y","id_pix","time"))
  
  return(vv_prism2)
}

system.time(l_prism <- lapply(fl_prism, fn_extract_prism))

gc(full=T)
mergeDTs <- function(dt_list, by = NULL, sort = FALSE) {
  Reduce(
    function(...) {
      merge(..., by = by, all = TRUE, sort = sort)
    }, dt_list)
}

system.time(d_gmd <- mergeDTs(l_gmd))
system.time(d_prism <- mergeDTs(l_prism))

setkeyv(d_gmd, cols=c("x","y","id_pix","time"))
setkeyv(d_prism, cols=c("x","y","id_pix","time"))

system.time(d_clim <- merge(d_gmd, d_prism, by=c("x","y","id_pix","time")))

## END extract gridded climate ===============================

setkeyv(dd_evi2, cols=c("x","y","id_pix","time"))
setkeyv(d_clim, cols=c("x","y","id_pix","time"))

system.time(cdat <- merge(dd_evi2, d_clim, by=c("x","y","id_pix","time")))
system.time(cdat2 <- merge(cdat, dd13, by=c("fobservation_id")))

arrow::write_parquet(cdat2, 
                     sink = "data/parquets/damageArea13_evi2_gmd_prism.parquet",
                     compression = "snappy")



# SCRATCH ############################################

# cdat2[sample(.N, 100)] %>% select(area_ha, shape_area) %>% cor
# 
# 
# vec_id <- unique(cdat$id_pix)
# sel_id <- sample(vec_id, 10)
# sel_id
# cdat[id_pix%in%sel_id][month(time)==7] %>% ggplot(aes(eddi1y, evi2))+geom_point()+geom_smooth()+
#   facet_wrap(~id_pix)
# 
# fn_extract_gmd <- function(fp){
#   fp_csv <- str_replace(fp, pattern = ".tif", replacement = ".csv")
#   r <- rast(fp)
#   vec_dates <- fread(fp_csv)
#   time(r) <- vec_dates$date
#   names(r) <- time(r)
#  
# }
# 
# gmd_var <- str_extract(basename(fp),  "_([^_]+)_") %>% 
#   substr(., 2, nchar(.) - 1)
# 
# mask()




v2[id_pix==1] %>% 
  ggplot(aes(time, evi2))+geom_point()

lut_time <- as.Date(unique(v2$time))
lut_time[match(v2$time[1:100], lut_time)] %>% class

microbenchmark::microbenchmark(
  base::as.Date(v2$time[1:100]), 
  lubridate::date(v2$time[1:100]), 
  lut_time[match(v2$time[1:100], lut_time)])



lubridate::date(v2$time[1:100])


# v2[id_pix == 1] %>% 
#   ggplot(aes(time))
# 
# class(dd)
# dd[,"survey_year" < 2013] %>% dim
# dd[,"survey_year" >= 2013] %>% dim
# dd %>% filter(survey_year >= 2013)
# 
# ##!!!!!!!!!!!!!!!!!!!!!
# dd_grid <- terra::rasterize(dd, tmp_v, field = "observation_id")
# 
# dd %>% 
#   as.data.table() %>% 
#   select(survey_year, observation_id)
# 
# 
# vv_bath <- terra::extract(bath, xy[,.(x,y)], xy=T) %>% 
#   as.data.table()
# vv_amc <- terra::extract(amc, xy[,.(x,y)], xy=T) %>% 
#   as.data.table()
# 
# tmp_red <- terra::extract(ic, xy)
# tmp_red <- cbind(xy,tmp_red)
# 
# dat <- cbind(tmp_red, vv_bath %>% select(depth),
#              vv_amc %>% select(zone))
# 
# 
# cdat <- dat %>% 
#   as_tibble() %>% 
#   pivot_longer(-c("x","y","ID","depth","zone"), 
#                names_to = 'date',
#                values_to = "kd490") %>% 
#   as.data.table()
# 
# 
# 
# 
# tmp_v
# names(tmp_v)
# 
# tmp_v <- terra::mask(tmp_v, dd)
# 
# 
# 
# tmp_dd <- dd[sample(1:nrow(dd), 1000)]
# tmp3 <- terra::extract(tmp_v, tmp_dd)
# 
# 
# microbenchmark::microbenchmark(terra::extract(tmp, dd_v), times = 5)
# 
# 
# rs <- rast("data/gee_data_AZ_tree_stress/mcd43a4_evi2_monthly_surveyArea_2000-6_2023-11.tif")
# vec_dates <- fread('data/gee_data_AZ_tree_stress/mcd43a4_evi2_monthly_surveyArea_2000-6_2023-11.csv')
# time(rs) <- vec_dates$date
# names(rs)
# 
# gmd_pdsi <- rast("data/gee_data_AZ_tree_stress/gridmetDrought_pdsi_2000-6_2023-11.tif")
# vec_dates <- fread('data/gee_data_AZ_tree_stress/mcd43a4_evi2_monthly_surveyArea_2000-6_2023-11.csv')
# time(rs) <- vec_dates$date
# 
# 
# 
# 
# par(mfrow=c(2,2))
# plot(rs[[time(rs)==ymd("2001-07-01")]]);
# plot(rs[[time(rs)==ymd("2005-07-01")]]);
# plot(rs[[time(rs)==ymd("2011-07-01")]]);
# plot(rs[[time(rs)==ymd("2015-07-01")]]);
# 
# 
# plot(rs[[year(time(rs))==2005]]);
# 
# dsa <- st_read("data/gee_tables/surveyed_areas_epsg4326_allyears_conus_rgn3.zip/")
# dsa %>% 
#   select(srvy_yr) %>% 
#   filter(srvy_yr==2005) %>% plot
# l_out <- 2001:2023 %>% 
#   lapply(., FUN = function(x) plot(dsa %>% 
#                                      select(srvy_yr) %>% 
#                                      filter(srvy_yr==x)))
# 
# dsa %>% filter(srvy_yr%in% 2005) %>% 
#   ggplot() + 
#   geom_sf() + 
#   facet_wrap(~srvy_yr)
