library(tidyverse)
library(MCMsBasics)

d <- tidytuesdayR::tt_load("2022-08-09")
d

d <- d$wheels %>% 
  select(-...1)

d %>% 
  filter(status == "Operating") %>% 
  select(country, where(is.numeric)) %>% 
  pivot_longer(-country) %>% 
  filter(!is.na(value)) %>% 
  group_by(name, country) %>% 
  summarise(med_value = median(value),
            n = n()) %>% 
  ungroup() %>% 
  mutate(country = paste0(country, " (n=", n, ")"),
         name = factor(name) %>% 
           fct_recode("Diameter" = "diameter",
                      "Height" = "height",
                      "Hourly Capacity" = "hourly_capacity",
                      "\\# of Cabins" = "number_of_cabins",
                      "Passengers per Cabin" = "passengers_per_cabin",
                      "Ride Duration (mins)" = "ride_duration_minutes",
                      "Seating Capacity" = "seating_capacity",
                      "Turns" = "turns")) %>% 
  ggplot(aes(x = med_value, 
             y = drlib::reorder_within(country, by = med_value, within = name),
             yend = drlib::reorder_within(country, by = med_value, within = name))) +
  geom_point(color = "grey90") +
  geom_segment(xend = 0, color = "grey90") +
  drlib::scale_y_reordered() +
  facet_wrap(vars(name), scales = "free", nrow = 3) +
  theme_mcm_dark() +
  theme(axis.title = element_blank()) +
  labs(title = "Big wheel keep on turning",
       subtitle = "Median values of various metrics for ferris wheels in each country")

ggsave("images/tidy_tuesday/week_32_ferris_wheel_summaries.jpg", width = 10, height = 12, device = grDevices::jpeg)


