#install and load the necessary packages
packs <- c('osmdata', 'dplyr', 'sf','rmapshaper','giscoR', 'readxl', 'stringi')

for (i in 1:length(packs))  {
  installed <- require(packs[i], character.only = TRUE)
  if (installed == F) {
      install.packages(packs[i], ask = FALSE)
  }
  library(packs[i], character.only = T)
}


#set the working directory to be the folder where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

#------------outline of GB for clipping-------------
countries <- gisco_get_nuts(country = 'UK', resolution = "01") %>%
  subset(LEVL_CODE == 1 & NAME_LATN != 'NORTHERN IRELAND') %>%
  mutate(country = ifelse(NAME_LATN %in% c('SCOTLAND', 'WALES'), NAME_LATN, 'ENGLAND')) %>%
  group_by(country) %>%
  summarize(geometry = st_union(geometry)) %>% st_transform(27700)

gb <- st_union(countries)

bb <- getbb("UK")
sf_use_s2(F)


#------------power plants-------------
#https://www.gov.uk/government/statistics/electricity-chapter-5-digest-of-united-kingdom-energy-statistics-dukes
#sheet 5.11 
download.file('https://assets.publishing.service.gov.uk/media/688cb64ee8ba9507fc1b0953/DUKES_5.11.xlsx',
              "./data/power_plants.xlsx")

plants <- read_xlsx("./data/power_plants.xlsx", sheet = 5, skip = 5) %>%
  rename('company' = 1, 'name'=2, 'technology'=3, 'type' = 4, 'fuel'=6, 'capacity'=8, 'country' = 10, 'x'=14, 'y'=15) %>%
  select(company, name, technology, type, fuel, capacity, country, x, y) %>%
  st_as_sf(coords = c(x='x',y='y'), crs = 27700) %>%
  st_transform(4326) %>%
  subset(country != "Northern Ireland" & capacity >= 100)
    
write_sf(plants, "./data/power_plants.geojson")


#------------overhead lines-------------
power_lines <- bb %>%
  opq(timeout = 999) %>%
  add_osm_feature(key = "power", value = "line") %>%
  osmdata_sf()

#one small section currently under construction
bb <- matrix(data = c(-2.88280, 51.33240, -2.63377,  51.53901),nrow = 2,ncol = 2,dimnames = list(c('x','y'), c('min', 'max')))
under_construction <- bb %>%
  opq(timeout = 999) %>%
  add_osm_feature(key = "construction:power") %>%
  osmdata_sf() %>%

#combine and clean the data
power <- power_lines$osm_lines %>% 
  bind_rows(under_construction$osm_lines) %>%
  select(osm_id, name, brand,cables, circuits, location,operator,owner,phases,ref,voltage, source) %>%
  #convert voltage lists to numbers
  mutate(max_voltage = stri_split(voltage, fixed = ";"),
          max_voltage = sapply(max_voltage, as.numeric),
          max_voltage = sapply(max_voltage, max)) %>%
  #keep only lines above 132k
  subset(max_voltage >= 132000) %>%
  #simplify
  ms_simplify(.1) %>%
  #british national grid
  st_transform(27700) %>%
  #keep all lines in scotland, only 400k + 275k + a few manual exceptions in eng+wales
  #manual exceptions established by hand in qgis to see which 132k lines appear in official maps
  st_join(countries, join = st_intersects) %>%
  subset( (country == 'SCOTLAND') | 
          (country %in% c('ENGLAND', 'WALES') & max_voltage != 132000) |
          (osm_id %in% c('90986942', '291507467', '1411600575', '87213645', '1376328853', '89427637', '358551363', 
                         '69156353', '69156590', '108600751', '1087785228', '108338688'))) %>%
  #remove short stubs, keep only those at least 1km long
  mutate(len = as.numeric(st_length(.))) %>%
  subset(len >= 1000) %>%
  select(-country) %>%
  unique()

write_sf(power, './data/power.geojson', delete_dsn = T)


#------------subsurface cables-------------
power_cables <- bb %>%
  opq(timeout = 999) %>%
  add_osm_feature(key = "power", value = "cable") %>%
  osmdata_sf()

cable_multilines <- power_cables$osm_multilines %>% st_cast('LINESTRING')
cable_lines <- bind_rows(power_cables$osm_lines, cable_multilines)
cables <- bind_rows(cable_lines, cable_multilines)

cables <- cables %>%
  select(osm_id, name, brand,cables, circuits, location,operator,owner,phases,ref,voltage, source) %>%
  #convert voltage lists to numbers
  mutate(max_voltage = stri_split(voltage, fixed = ";"),
        max_voltage = sapply(max_voltage, as.numeric),
         max_voltage = sapply(max_voltage, max)) %>%
  #keep only those above 132k voltage
  #also remove two weird stragglers that don't get caught later
  subset(max_voltage >= 132000 &
           !osm_id %in% c('1199410251', '1199410249')) %>%
  #simplify
  ms_simplify(.1) %>%
  #british national grid
  st_transform(27700) %>%
  #merge touching lines together
  group_by(max_voltage) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast('MULTILINESTRING') %>%
  st_line_merge() %>%
  ms_explode() %>%
  #only keep those lines at least 1km in length
  mutate(len = as.numeric(st_length(.))) %>%
  subset(len >= 1000) %>%
  mutate(id = row_number())
  

#only keep cables where both ends lie within great britain
coords <- cables %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename('id' = 3) %>%
  group_by(id) %>%
  mutate(row = row_number(),
         max_row = n(),
         type = ifelse(row == 1, "start", ifelse(row == max_row, "end", NA))) %>%
  subset(row == 1 | row == max_row) %>%
  st_as_sf(coords = c(x='X',y='Y'), crs = 27700) %>%
  st_intersection(gb) %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  tally() %>%
  subset(n >= 2)

cables <- cables %>% subset(id %in% coords$id) 
 
write_sf(cables, "./data/cables.geojson", delete_dsn = T)

#------------substations-------------
substations <- bb %>%
  opq(timeout = 999) %>%
  add_osm_feature(key = "power", value = "substation") %>%
  add_osm_feature(key = "substation", value = "transmission") %>%
  osmdata_sf()

#combine points and polygons into just single points 
pts <- substations$osm_points
poly <- substations$osm_polygons 
  
if (!is.null(substations$osm_multipolygons)) {
  poly <- poly %>%   bind_rows(substations$osm_multipolygons)
}

diff <- st_intersection(pts, poly)
single_pts <- pts %>%subset(!osm_id %in% diff$osm_id)
poly_pts <- poly %>%st_centroid()
  
substations <- single_pts %>%
  bind_rows(poly_pts) %>%
  select(osm_id, name, voltage, operator,  owner) %>%
  st_transform(27700)

#clip substations to just those within 200m of a power line
lines <- bind_rows(power, cables) %>%
  st_buffer(200) %>%
  select(geometry)

substations <- substations %>%
  st_intersection(lines) 

write_sf(substations, "./data/substations.geojson", delete_dsn = T)



  
#------------plot------------
power_plants <- read_sf("./data/power_plants.geojson") %>% st_transform(27700) %>% st_intersection(gb)
substations <- read_sf("./data/osm/substations.geojson")

ggplot() + theme_void() +
  geom_sf(data = gb, fill = '#FDF8E3', color = '#3d3c38') +
  geom_sf(data = power, aes(color = as.character(max_voltage), linewidth =voltage_width)) +
  geom_sf(data = substations, color = '#b7b4a9', fill = '#b7b4a9', size = .1) +
    geom_sf(data = power_plants, color = '#3d3c38', fill = '#3d3c38', size = .75) +
  scale_linewidth(range = c( .4, .8)) +
  scale_color_manual(values = c('400K'='#dd7a10', '275K' = '#F6952D', '132K' = '#F8C371')) +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = alpha("#BAD6D3", 1)))

ggsave("test.svg", width = 4000, height = 4000*1.89, units = 'px')





