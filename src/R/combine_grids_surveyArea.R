pacman::p_load(terra,data.table,tidyverse,sf, patchwork)

# config options
sf_use_s2(use = T)

## Load reference MODIS grid ===============================
grid_ref <- rast("data/gee_data_AZ_tree_stress/mcd43a4_evi2_monthly_surveyArea_2000-6_2023-11.tif")[[1]]


## Load vector datasets using Terra =========================
# survey area
dsa <- vect("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
            layer = "SURVEYED_AREAS_FLAT_AllYears_CONUS_Rgn3")
names(dsa) <- tolower(names(dsa))
dsa$area_ha <- terra::expanse(dsa, unit="ha")
dsa <- terra::project(dsa, terra::crs("epsg:4326"))
dsa <- terra::makeValid(dsa)
table(terra::is.valid(dsa))
dsa <- terra::simplifyGeom(dsa, tolerance = 0.01)

# damage area
dd <- vect("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
           layer = "DAMAGE_AREAS_FLAT_Allyears_CONUS_Rgn3")
names(dd) <- tolower(names(dd))
dd$area_ha <- terra::expanse(dd, unit="ha")
dd <- terra::project(dd, terra::crs("epsg:4326"))
dd <- terra::makeValid(dd)
table(terra::is.valid(dd))
# dd_buff <- terra::buffer(dd, width = 500) # too slow!
dd <- terra::simplifyGeom(dd, tolerance = 0.01)


## Rasterize vectors
r_sa <- terra::rasterize(dsa, grid_ref, field = "survey_year", fun='max')
r_sa %>% plot

r_da <- terra::rasterize(dd, grid_ref, field = "survey_year")
r_da %>% plot


r_sa2 <- terra::mask(r_sa, r_da, inverse=TRUE)
r_sa3 <- terra::focal(r_sa2, fun='min', w = 3, na.rm=FALSE)

r_sa3[r_sa3 <= 2013] <- NA
plot(r_sa3)

# convert masked survey area back to points
d_sa3 <- terra::as.points(r_sa3)
names(d_sa3) <- "survey_year"

# subsample
dsa13 <- d_sa3[sample(nrow(d_sa3),  floor(nrow(d_sa3)/2) ), ]


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







## combine "Survey Area" points with MODIS VI to retain full VI time series
vv_vi <- terra::extract(tmp_v, dsa13, xy=T) %>% setDT()
vv_vi <- vv_vi[,-c("ID")]
vv_vi$survey_year <- (as.data.table(dsa13,geom="XY"))$survey_year


v2 <- melt(vv_vi, id.vars = c('x','y','survey_year'), 
           variable.name = "time", 
           value.name = "evi2")
v2[,`:=`(id_pix = .GRP), by=.(x,y)] # caution! this will not correspond w/damage area id_pix
v2[,`:=`(time = as.character(time))]
lut_time <- as.Date(unique(v2$time))
system.time(v2[,`:=`(time = lut_time[match(time, lut_time)])]) # Fast way to cast to date!
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
#!!!system.time(cdat2 <- merge(cdat, dd13, by=c("fobservation_id")))
cdat2 <- cdat %>% select(-id_pix)
arrow::write_parquet(cdat2, 
                     sink = "data/parquets/surveyArea13_evi2_gmd_prism.parquet",
                     compression = "snappy")


