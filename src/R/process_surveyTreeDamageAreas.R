pacman::p_load(data.table, tidyverse, sf)

# survey area
dsa <- sf::read_sf("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
                  layer = "SURVEYED_AREAS_FLAT_AllYears_CONUS_Rgn3")
names(dsa) <- tolower(names(dsa))
st_geometry(dsa) <- "shape"
dsa <- dsa %>% 
  mutate(area_ha = st_area(.)/(100**2))

sf_use_s2(use = T)
dsa2 <- dsa %>% 
  st_simplify(preserveTopology = F, dTolerance = 50) %>% 
  st_transform(., crs = st_crs(4326))

dsa2 %>% 
  st_write(., dsn = "data/gee_tables/surveyed_areas_epsg4326_allyears_conus_rgn3.zip", 
           driver = "ESRI Shapefile")

tmp <- st_read("data/gee_tables/surveyed_areas_epsg4326_allyears_conus_rgn3.zip/")
tmp %>% 
  select(srvy_yr) %>% 
  filter(srvy_yr==2003) %>% plot


# damage area
dd <- sf::read_sf("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
                  layer = "DAMAGE_AREAS_FLAT_Allyears_CONUS_Rgn3")
names(dd) <- tolower(names(dd))
st_geometry(dd) <- "shape"
dd <- dd %>% 
  mutate(area_ha = st_area(.)/(100**2))


dsa %>% 
  st_drop_geometry() %>% 
  units::drop_units() %>% 
  group_by(survey_year) %>% 
  summarize(val_km2 = sum(area_ha/100)) %>% 
  ggplot(aes(survey_year, val_km2))+
  geom_point()

1e05/200**2


merge(
 dsa %>% 
  st_drop_geometry() %>% 
  group_by(survey_year) %>% 
  summarize(s_area = sum(area_ha,na.rm=T)), 
 dd %>% 
  st_drop_geometry() %>% 
  group_by(survey_year) %>% 
  summarize(d_area = sum(area_ha,na.rm=T)), 
 by='survey_year') %>% 
  units::drop_units() %>% 
  ggplot(aes(survey_year, d_area/s_area))+
  geom_line()




dsa %>% names %>% sort
dsa$shape_area


db <- sf::read_sf("../data_general/AZ_tree_stress_data/forest_health_monitoring_usda/CONUS_Region3_AllYears.gdb/CONUS_Region3_AllYears.gdb", 
                  layer = "DAMAGE_POINTS_FLAT_Allyears_CONUS_Rgn3")
names(db) <- tolower(names(db))
st_geometry(db) <- "shape"
db
db$damage_type %>% table

dat <- db %>% 
  st_transform(., st_crs(4326))

names(dat) %>% sort

sel <- c("acres", "collection_mode", "created_date", 
         # "damage_point_id", 
         "damage_type", "damage_type_code", "data_source_name", "dca_code", 
         "dca_common_name", "feature_user_id", "grp", "host", "host_code", 
         "host_group", "host_group_code", "ids_data_source", "label", 
         "modified_date",
         # "notes",
         "number_of_trees_code", "number_of_trees_count_range", 
         "observation_count", 
         # "observation_id", "observation_user_id", 
         "range_max", "range_mid", "range_min", "region_id", 
         # "shape", 
         "status", "survey_year", "tree_count" 
         # "us_area"
)
dat %>% select(shape)

tab <- dat %>% 
  mutate(longitude = st_coordinates(dat)[,'X'],
         latitude = st_coordinates(dat)[,'Y']) %>% 
  st_drop_geometry() %>% 
  as.data.table()


tab2 <- tab %>% 
  select(all_of(sel), longitude, latitude)

# fwrite(tab2, file = "outputs/AZ_tree***")
