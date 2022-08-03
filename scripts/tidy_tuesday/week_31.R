library(tidyverse)
library(MCMsBasics)
library(sf)
library(osmdata)
library(patchwork)

d <- tidytuesdayR::tt_load("2022-08-02")
frogs <- d$frogs
frogs <- frogs %>% 
  janitor::clean_names() %>% 
  mutate(survey_date = lubridate::mdy(survey_date),
         month = lubridate::month(survey_date),
         female = as.logical(female),
         sex = ifelse(female, "female", "male")) %>% 
  arrange(survey_date)

frogs <- st_as_sf(frogs, coords = c("utme_83", "utmn_83"), 
                  crs = "+proj=utm +zone=10")

frog_hulls <- frogs %>% 
  group_by(frequency, sex) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_convex_hull()

water <- opq(bbox = c(-121.824775, 43.764375, -121.764923, 43.814821)) %>% 
  add_osm_features(c("\"natural\"=\"water\"",
                     "\"waterway\"=\"river\"")) %>% 
  osmdata_sf()

p1 <- ggplot() +
  geom_sf(data = water$osm_polygons, color = NA, fill = "lightblue") +
  geom_sf(data = water$osm_lines, color = "lightblue", fill = "lightblue", inherit.aes = F) +
  geom_sf(data = st_transform(frog_hulls, "+proj=longlat +datum=WGS84"), 
          inherit.aes = F, 
          mapping = aes(fill = sex), 
          color = NA, alpha = 0.5) +
  coord_sf(xlim = c(-121.835, -121.755),
           ylim = c(43.764375, 43.814821)) +
  theme_mcm(large_lineheight = T) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Oh how the frogs roam",
       subtitle = "Convex hulls containing telemetry locations for individual <span style = 'color:#1B9E77;font-family:FrauncesSuperSoftWonky-MediumItalic;'>female</span> and <span style = 'color:#D95F02;font-family:FrauncesSuperSoftWonky-MediumItalic;'>male</span><br>Oregon spotted frogs at Crane Prairie Reservoir in Oregon.<br>Data courtesy of the USGS.")

p2 <- frog_hulls %>% 
  mutate(area = st_area(geometry) %>% 
           as.numeric()) %>% 
  ggplot(aes(x = area, y = sex, fill = sex)) +
  #geom_histogram()
  #ggridges::stat_binline(color = NA, alpha = 0.8) +
  ggridges::geom_density_ridges(color = NA, alpha = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  theme_mcm() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = ggtext::element_markdown()) +
  scale_x_log10() +
  xlab("Hull area (*m*<sup>2</sup>)")


p1 / p2 + plot_layout(heights = c(3,1))

ggsave("images/tidy_tuesday/week_31_frogs_convex_hulls.jpg", width = 8, height = 6,
       device = grDevices::jpeg)
