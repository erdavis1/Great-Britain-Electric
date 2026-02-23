#install and load the necessary packages
packs <- c('ggplot2', 'sf','dplyr','giscoR', 'rmapshaper')

for (i in 1:length(packs))  {
  installed <- require(packs[i], character.only = TRUE)
  if (installed == F) {
      install.packages(packs[i], ask = FALSE)
  }
  library(packs[i], character.only = T)
}

#set the working directory to be the folder where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))



#------------Country borders-------------
countries <- gisco_get_nuts(country = 'UK', resolution = "03") %>%
  subset(LEVL_CODE == 1 & NAME_LATN != 'NORTHERN IRELAND') %>%
  mutate(country = ifelse(NAME_LATN %in% c('SCOTLAND', 'WALES'), NAME_LATN, 'ENGLAND')) %>%
  group_by(country) %>%
  summarize(geometry = st_union(geometry)) %>% st_transform(27700)

gb <- st_union(countries)
borders <- ms_innerlines(countries)

#------------load preprocessed data-------------
power <- read_sf("./data/power.geojson") %>%
  mutate(plot_voltage = ifelse(max_voltage >= 400000, "400k+", ifelse(max_voltage >= 275000, "275k+", "132k"))) 
cables <- read_sf("./data/cables.geojson") %>%
    mutate(plot_voltage = ifelse(max_voltage >= 400000, "400k+", ifelse(max_voltage >= 275000, "275k+", "132k"))) 
substations <- read_sf("./data/substations.geojson")
power_plants <- read_sf("./data/power_plants.geojson") %>% st_intersection(gb)


#------------plot-------------
voltage <- c("400k+" = "#C22A1F", "275k+" = "#C05A2B", "132k" = "#C89B3C")
land <- '#E8DCC2'
outlines <- '#1F2A33'
water <- '#A7C0C8'
substation <- '#96928a'
power_plant <- '#5A4E6D'


ggplot() + theme_void() +
  geom_sf(data = gb, fill = land, color = outlines) +
  geom_sf(data = borders, color = alpha(outlines, 0.5), linetype = "12") +
  geom_sf(data = power, aes(color = plot_voltage, linewidth = max_voltage)) +
  geom_sf(data = cables, aes(color = plot_voltage, linewidth = max_voltage), linetype = "11") +
  geom_sf(data = substations, color = substation, size = .1) +
  geom_sf(data = power_plants, color = power_plant, size = .8) +
  scale_color_manual(values = voltage) +
  scale_linewidth(range = c(0.1, 0.4)) +
  theme(plot.background = element_rect(fill = water, color = NA),
        legend.position = 'none')

ggsave("./plots/map.svg", height = 9, units = "in")
