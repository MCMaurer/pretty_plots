library(tidyverse)
library(MCMsBasics)
library(ggtext)
library(drlib)

d <- tidytuesdayR::tt_load("2022-08-16")
d

sop_p <- d$psych_stats %>% 
  filter(uni_name == "The Sopranos")

sop_emo <- sop_p %>% 
  filter(str_detect(question, emoji::emoji_rx))

d$psych_stats %>% 
  filter(str_detect(question, emoji::emoji_rx)) %>% 
  group_by(uni_name, question) %>% 
  summarise(min = min(avg_rating),
            max = max(avg_rating)) %>% 
  print(n=Inf)

p <- sop_emo %>% 
  mutate(char_name = str_remove(char_name, 
                                "Soprano|'Walnuts' Gualtieri|Moltisanti|Dante|Jennifer")) %>% 
  separate(question, into = c("opt1", "opt2"), sep = "/", remove = F) %>% 
  mutate(avg_rating = ifelse(personality == opt1, -avg_rating, avg_rating)) %>% 
  ggplot(aes(x = avg_rating, y = reorder_within(char_name, avg_rating, question))) +
  geom_point(color = "grey20") +
  geom_segment(color = "grey20", x = 0, 
               aes(xend = avg_rating, yend = reorder_within(char_name, avg_rating, question))) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  scale_y_reordered() +
  facet_wrap(vars(question), scales = "free_y") +
  theme_mcm(mult2 = 2.5, mult4 = 4) +
  theme(axis.title.y = element_blank()) +
  labs(title = "This emoji thing of ours",
       subtitle = '"Listen Tone, they\'re called emojis. They\'re the hot new thing, but check this out:\nthe Egyptians did em hundred of years ago or whatever. Called em hairy gliffs."',
       x = "Average Rating")

ggsave("images/tidy_tuesday/week_33_sopranos_emojis.jpg", plot = p, width = 20, height = 12)
