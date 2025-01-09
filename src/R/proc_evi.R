pacman::p_load(terra, data.table, tidyverse, lubridate, future.apply)

vec_time <- seq(ymd("2000-07-01"),by='1 year',length.out=25)
evi <- rast("data/gee_data_AZ_tree_stress/mcd43a4_annualGrowingSeasonMean_evi2_2000_2024.tif")
terra:::readAll(evi)
time(evi) <- vec_time
names(evi) <- vec_time
terra::inMemory(evi)

o_krig <- rast("data/gee_data_AZ_tree_stress/krig_max_mortRangeMax.tif")
krig <- terra::project(o_krig, evi)


bd <- rast("data/gee_data_AZ_tree_stress/mcd64_lastBurnDate_2000-7_2024-10.tif")
bd <- terra::project(bd, evi)
bd <- terra::mask(bd, krig)


tmp <- terra::mask(evi, krig)


v <- c(bd, krig) %>% 
  as.data.table(xy=T)
v2 <- tmp %>% 
  as.data.table(xy = T)

v3 <- melt(v2, id.vars=1:2, 
     variable.name='time',
     value.name='evi2')

v3[,`:=`(time = ymd(time))]


d <- merge(v3, v, by=c("x","y"))

d[,`:=`(evi2_p50 = median(evi2,na.rm=T), 
        evi2_min = min(evi2,na.rm=T),
        evi2_max = max(evi2,na.rm=T),
        evi2_sd = sd(evi2,na.rm=T)),by=.(x,y)]

arrow::write_parquet(d, 
                     sink = "data/parquets/mcd43a4_evi2_growingSeason_2002_2024.parquet",
                     compression = 'snappy')


# merge(d, d_clim, by=c("x","y"))

# # SCRAPT ==================================================
# 
# 
# library(mgcv)
# 
# 
# d[is.na(bd)==T][sample(.N, 1e5)] %>% 
#   ggplot(aes(evi2-evi2_p50, range_max))+
#   # geom_point()+
#   geom_smooth()
# 
# 
# tmp[[2]] %>% 
#   plot(range=c(0, 0.55))
# 
# evi[[2]] %>% 
#   plot(range=c(0, 0.55))
