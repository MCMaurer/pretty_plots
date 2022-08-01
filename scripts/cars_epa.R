library(tidyverse)
library(ggtext)
library(ggh4x)
library(MCMsBasics)


# data setup --------------------------------------------------------------


cars_mpg <- cars_mpg %>% 
  mutate(VClass_s = case_when(
    str_detect(VClass, "Sport Utility Vehicle -|Standard Sport") ~ "SUVs",
    str_detect(VClass, "Small Sport") ~ "Small SUVs",
    str_detect(VClass, "Standard Pickup") ~ "Standard Pickup Trucks",
    str_detect(VClass, "Small Pickup") ~ "Small Pickup Trucks",
    str_detect(VClass, "Special Purpose") ~ "Special Purpose Vehicles",
    str_detect(VClass, "Minivan") ~ "Minivans",
    str_detect(VClass, "Passenger") ~ "Passenger Vans",
    str_detect(VClass, "Cargo") ~ "Cargo Vans",
    TRUE ~ VClass
  ))


# mpg by make -------------------------------------------------------------


c2 <- cars_mpg %>% 
  select(where(~sum(is.na(.x))/length(.x) < 0.1)) %>% 
  mutate(make = factor(make) %>% 
           fct_lump_n(47)) %>%
  mutate(make = fct_recode(make, Other = "Eagle", Other = "Geo") %>% 
           fct_relevel("Other", after = Inf)) %>% 
  group_by(make, year) %>% 
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = T),
                                           med = ~median(.x, na.rm = T)))) 

c2 %>% 
  ggplot(aes(x = year, y = comb08_med, label = round(comb08_med, 1))) +
  geom_line() +
  geom_label(data = c2 %>% 
               filter(year == max(year) | year == min(year)),
             size = 3, label.padding = unit(0.15, "lines"),
             family = "FrauncesSuperSoftWonky-Light") +
  facet_wrap(vars(make)) +
  scale_y_log10() +
  theme_mcm(facet_outlines = F, base_size = 7) +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = "Median combined MPG",
       title = "Good for you, Rolls-Royce",
       subtitle = "Changes in median MPG by manufacturer")

ggsave("images/cars_epa/med_mpg_by_make.jpg", device = grDevices::jpeg, width = 10, height = 8)


# mpg by vehicle class ----------------------------------------------------



c3 <- cars_mpg %>% 
  select(where(~sum(is.na(.x))/length(.x) < 0.1)) %>% 
  mutate(VClass_s = factor(VClass_s) %>% 
           fct_reorder(comb08) %>% 
           fct_lump_n(20)) %>% 
  group_by(VClass_s, year) %>% 
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = T),
                                           med = ~median(.x, na.rm = T))))


c3 %>% 
  ggplot(aes(x = year, y = comb08_med, label = round(comb08_med, 1))) +
  geom_line() +
  geom_label(data = c3 %>% 
               filter(year == max(year) | year == min(year)),
             size = 3, label.padding = unit(0.15, "lines"),
             family = "FrauncesSuperSoftWonky-Light") +
  facet_wrap(vars(VClass_s)) +
  scale_y_log10() +
  theme_mcm(facet_outlines = T, base_size = 7) +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = "Median combined MPG",
       title = "Uneven MPG Improvements",
       subtitle = "Changes in median MPG by EPA vehicle category")

ggsave("images/cars_epa/med_mpg_by_class.jpg", device = grDevices::jpeg, width = 10, height = 8)

cars_mpg %>% 
  filter(year > max(year)-10) %>% 
  mutate(VClass_s = factor(VClass_s) %>% 
           fct_reorder(comb08)) %>% 
  ggplot(aes(y = VClass_s, x = comb08)) +
  ggridges::geom_density_ridges(color = NA, alpha = 0.8) +
  theme_mcm_dark() +
  theme(axis.title.y = element_blank()) +
  scale_x_log10() +
  labs(x = "Combined MPG",
       title = "MPG by EPA vehicle class",
       subtitle = "All vehicles from last 10 years of EPA data")

ggsave("images/cars_epa/mpg_ridges_byclass_last10years.jpg", device = grDevices::jpeg, width = 6, height = 4)
