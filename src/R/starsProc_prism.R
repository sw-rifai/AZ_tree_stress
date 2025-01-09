pacman::p_load(stars, data.table, tidyverse, lubridate 
               # future.apply
               )

# library(foreach)
# library(doParallel)
# cl <- makeCluster(12L)
# reg1isterDoParallel(cl)


## prism met data =====================
p_time <- fread("data/gee_data_AZ_tree_stress/DATES_PRISM_AN81m_ppt_2000-07-01_2024-10-30.csv")
p_time <- ymd_hms(p_time$date) %>% as.Date()
fl <- list.files("data/gee_data_AZ_tree_stress",pattern = "PRISM",full.names = T)
fl <- fl[!str_detect(fl, "DATES")]

str_extract(str_remove(basename(fl), "PRISM_AN81m_"), "^[a-zA-Z]{3,6}")

d <- arrow::read_parquet("data/parquets/mcd43a4_evi2_growingSeason_2002_2024.parquet") %>% 
  setDT()

coords <- unique(d[,.(x,y)])


# crs("+proj=longlat +datum=WGS84")
coords <- st_as_sf(coords, coords=c("x","y"), 
                   crs=st_crs("+init=EPSG:4326"))

s <- stars::read_stars(fl[1])
s <- st_set_dimensions(s, 3, values=p_time, names = 'time')
names(s) <- 'ppt'

s2 <- stars::read_stars(fl[2])
s2 <- st_set_dimensions(s2, 3, values=p_time, names = 'time')
names(s2) <- 'tdmean'

s <- c(s,s2)

vv <- st_extract(s, coords) %>% 
  st_as_sf(xy=T) %>% 
  mutate(x = st_coordinates(vv)['X'],
         y =st_coordinates(vv)['Y'])

data.table::melt(v, id.vars = c("ID","x","y"), 
                 variable.name='ftime',
                 value.name=var_name)

vv <- st_as_sf(vv)
vv
vv %>% as.data.table(xy=T)

# tmp <- fn(fl[1])
# x <- fl[1]

fn <- function(x){
  var_name <- stringr::str_extract(stringr::str_remove(basename(x), "PRISM_AN81m_"), "^[a-zA-Z]{3,6}")
  grid <- terra::rast("data/gee_data_AZ_tree_stress/mcd43a4_annualGrowingSeasonMean_evi2_2000_2024.tif")
  grid <- grid[[1]]
  r <- terra::rast(x)
  terra:::readAll(r)
  terra::time(r) <- p_time
  names(r) <- p_time
  r <- terra::project(r, grid)
  v <- terra::extract(r, coords, xy=T) %>% setDT()
  names(v)
  vv <- data.table::melt(v, id.vars = c("ID","x","y"), 
                         variable.name='ftime',
                         value.name=var_name)
  time_dict <- unique(vv[,.(ftime)]) %>% 
    .[,`:=`(time = as.Date(ftime))]
  vv <- merge(vv, time_dict, by='ftime')
  out <- vv %>% select(-'ftime')
  return(out)
}


out_list <- foreach(i = 1:length(fl), 
                    .packages = c("tidyverse","terra","data.table")) %dopar%
  {
    fn(fl[i])
  }
stopCluster(cl)

tmp <- Reduce(merge, out_list, list(by=c("ID","x","y","time")))
tmp <- rbindlist(out_list, use.names = F)
out_list

plan(multisession)
d_clim <- future_lapply(fl, FUN = fn)
plan(sequential)

tmp <- merge(d_clim[1],d_clim[2],by=c("x","y","time"))
for(i in 3:7){
  
}

tmp <- merge(d_clim, by=c("x","y","time"))
tmp <- rbindlist(d_clim,fill = F)
tmp <- rbindlist(d_clim)

d_clim <- d_clim %>% rbindlist()
arrow::write_parquet(d_clim,
                     sink="data/met/prism.parquet",
                     compression='snappy')
