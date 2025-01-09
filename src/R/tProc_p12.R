pacman::p_load(terra, data.table, tidyverse, lubridate 
)

# library(foreach)
# library(doParallel)
# cl <- makeCluster(12L)
# reg1isterDoParallel(cl)


## prism met data =====================
p_time <- fread("data/gee_data_AZ_tree_stress/DATES_PRISM_AN81m_ppt_2000-07-01_2024-10-30.csv")
p_time <- ymd_hms(p_time$date) %>% as.Date()
min_time <- min(p_time)
max_time <- max(p_time)
fl <- list.files("data/gee_data_AZ_tree_stress",pattern = "PRISM",full.names = T)
fl <- fl[!str_detect(fl, "DATES")]

# load precip
r <- rast(fl[str_detect(fl,"ppt")])
time(r) <- p_time

# load mask 
m <- rast("data/gee_data_AZ_tree_stress/krig_max_mortRangeMax.tif")
m <- terra::project(m, r)

# mask prism
r <- terra::mask(r, m)


# construct time vec for rolling calcs
vec_time <- seq(min_time+months(12), max_time, by='1 month')

# func to calc rolling 12 month p sum
fn_p12 <- function(x){
  t2 <- x
  t1 <- x-months(12)
  out <- r[[(time(r) >= t1) & (time(r) <= t2)]] %>% 
    sum(.,na.rm=T)
  return(out)
}

# calc p12, set attributes
tmp1 <- vec_time %>% lapply(., fn_p12)
r2 <- rast(tmp1)
names(r2) <- rep("p12",dim(r2)[3])
time(r2) <- vec_time

vec_time

str_extract(str_remove(basename(fl), "PRISM_AN81m_"), "^[a-zA-Z]{3,6}")

d <- arrow::read_parquet("data/parquets/mcd43a4_evi2_growingSeason_2002_2024.parquet") %>% 
  setDT()

coords <- unique(d[,.(x,y)])
