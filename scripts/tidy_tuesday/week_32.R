library(tidyverse)
library(MCMsBasics)

d <- tidytuesdayR::tt_load("2022-08-09")
d

d <- d$wheels %>% 
  select(-...1)

d %>% 
  group_by(country) %>% 
  summarise(across(where(is.numeric), ~median(.x, na.rm=T)),
            country = paste0(country, " (n=", n(), ")")) %>% 
  pivot_longer(-country, names_to = "var") %>% 
  filter(!is.na(value)) %>% 
  mutate(var = factor(var) %>% 
           fct_recode("Diameter" = "diameter",
                      "Height" = "height",
                      "Hourly Capacity" = "hourly_capacity",
                      "\\# of Cabins" = "number_of_cabins",
                      "Passengers per Cabin" = "passengers_per_cabin",
                      "Ride Duration (mins)" = "ride_duration_minutes",
                      "Seating Capacity" = "seating_capacity",
                      "Turns" = "turns")) %>% 
  ggplot(aes(x = value, 
             y = drlib::reorder_within(country, by = value, within = var),
             yend = drlib::reorder_within(country, by = value, within = var))) +
  geom_point(color = "grey90") +
  geom_segment(xend = 0, color = "grey90") +
  drlib::scale_y_reordered() +
  facet_wrap(vars(var), scales = "free", nrow = 3) +
  theme_mcm_dark() +
  theme(axis.title = element_blank()) +
  labs(title = "Big wheel keep on turning",
       subtitle = "Median values of various metrics for ferris wheels in each country")

ggsave("images/tidy_tuesday/week_32_ferris_wheel_summaries.jpg", width = 10, height = 13, device = grDevices::jpeg)
