pacman::p_load(data.table, 
  tidyverse,
  sf,
  gstat)

nsamp <- 30000
tab2 <- fread("outputs/AZ_tree_mort_coords.csv")
tab2 %>% glimpse()

# tab2$damage_type %>% unique
# 
# tab2[damage_type=="Mortality"]$number_of_trees_code %>% unique
# 
# tab2[damage_type=="Mortality"]$range_max %>% unique


tmp <- tab2[damage_type=="Mortality"][(sample(.N, nsamp))]

tmp <- st_as_sf(tmp, coords = c("longitude","latitude"))
st_crs(tmp) <- st_crs("EPSG:4326")

tmp2 <- tmp %>% st_transform(., st_crs("EPSG:26912"))

# st_coordinates(tmp) <- cbind(tmp[,.(longitude,latitude)])


v1 <- variogram(range_mid ~ 1, tmp2, cutoff = 5000, 
  width = 50)
plot(v1)

m1 <- fit.variogram(v1, vgm(1,"Exp"))
m2 <- fit.variogram(v1, vgm("Sph"))
m3 <- fit.variogram(v1, vgm("Nug"))
m4 <- fit.variogram(v1, vgm("Mat"))


plot(m1, cutoff=5000)
plot(m2, cutoff=5000)
plot(m3, cutoff=5000)
plot(m4, cutoff=5000)

m4
