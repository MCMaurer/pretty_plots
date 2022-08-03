library(tidyverse)
library(sf)
library(MCMsBasics)



# mapping tracks ----------------------------------------------------------


storms <- storms %>% 
  mutate(id = paste(name, year, sep = "_"),
         decade = paste0(year - (year %% 10), "s"))

ssf <- st_as_sf(storms, coords = c("long", "lat"), crs = 4326)

world1 <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE, 
                           xlim = c(-95,0), ylim = c(10, 55)))
ggplot() + 
  geom_sf(data = world1, fill = "grey95") +
  geom_path(data = storms, aes(x = long, y = lat, 
                               group = id, color = category, 
                               alpha = category,
                               size = category), 
            lineend = "round", linejoin = "round") +
  scale_color_viridis_d(option = "A", direction = -1, end = 0.9,
                        guide = ggh4x::guide_stringlegend()) +
  scale_alpha_discrete(guide = NULL,
                       range = c(0.2, 1)) +
  scale_size_manual(values = seq(from = 0.3, to = 0.9, by = 0.1),
                    guide = NULL) +
  xlim(c(-95,0)) +
  ylim(c(10,55)) +
  theme_mcm() +
  facet_wrap(vars(decade)) +
  theme(axis.text = element_blank(), axis.title = element_blank())


storms %>% 
  filter(id == "Amy_1975") %>% 
  ggplot(aes(x = long, y = lat)) +
  ggh4x::geom_pointpath()


names_1980 <- 
  storms %>% filter(year == 1980) %>% pull(name) %>% unique()

names_1980

storms %>% 
  filter(year == 1980, name == "Hermine") %>%
  slice_head(n = -2) %>% 
  print(n=Inf)

storms %>% 
  filter(year == 1980, name == "Hermine") %>%
  slice_head(n = -2) %>% 
  ggplot(aes(x = long, y = lat)) +
  ggh4x::geom_pointpath()

# if there are repeated lat/long coords, pointpath gets mad because it can't draw a line between points in the exact same spot

storms_d <- storms %>% 
  distinct(name, year, lat, long, .keep_all = T)

ggplot() + 
  geom_sf(data = world1, fill = "grey95") +
  ggh4x::geom_pointpath(data = storms_d, aes(x = long, y = lat, 
                                           group = id, color = category, 
                                           alpha = category,
                                           size = category)) +
  scale_color_viridis_d(option = "A", direction = -1, end = 0.9,
                        guide = ggh4x::guide_stringlegend(nrow = 1)) +
  scale_alpha_discrete(guide = NULL,
                       range = c(0.2, 1)) +
  scale_size_manual(values = seq(from = 0.3, to = 0.9, by = 0.1),
                    guide = NULL) +
  xlim(c(-95,-15)) +
  ylim(c(10,50)) +
  theme_mcm() +
  facet_wrap(vars(decade)) +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(5,0,-5,0),
        legend.direction = "horizontal",
        legend.justification = "left") +
  labs(title = "Hurricane Tracks",
       subtitle = "Data from NOAA Atlantic storm tracks. Data taken <br> every 6 hours for the duration of named storms.",
       color = "Category")
  

ggsave("images/storms/storm_tracks_pointpath_facet_decades.jpg",
       device = grDevices::jpeg, width = 10, height = 8)
  

ssf %>% 
  group_nest(name, year) %>% 
  mutate(track = map(data, ~st_cast(st_combine(.x), "LINESTRING") %>% 
                              .[[1]] %>% 
                              st_sfc(crs = 4326)))


  
storms.nest <- ssf %>% group_by(name, year) %>% nest

to_line <- function(tr) st_cast(st_combine(tr), "LINESTRING") %>% .[[1]] 

tracks <- storms.nest %>% pull(data) %>% map(to_line) %>% st_sfc(crs = 4326)
tracks

    