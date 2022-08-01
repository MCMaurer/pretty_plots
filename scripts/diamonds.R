library(tidyverse)
library(ggtext)
library(ggh4x)
library(MCMsBasics)


# price vs. carat with clarity, cut, and color ----------------------------------

p <- diamonds %>% 
  mutate(cutname = "Cut", clarityname = "Clarity",
         color = fct_rev(color), 
         clarity = fct_rev(clarity)) %>% 
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point(size = 0.2, alpha = 0.8) +
  scale_color_viridis_d(option = "A", begin = 0.25, 
                        guide = guide_stringlegend(nrow = 1, title.vjust = 1.1)) +
  theme_mcm_dark(facet_outlines = T) +
  theme(legend.position = "top",
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(10,0,-20,0),
        legend.direction = "horizontal",
        legend.justification = "left",
        axis.ticks = element_line(), 
        axis.title.x = element_text(hjust = 0.065),
        axis.title.y = element_text(hjust = 0.015)) +
  facet_nested(rows = vars(clarityname, clarity), cols = vars(cutname, cut),
               strip = strip_nested(text_x = 
                                      elem_list_text(family = c("FrauncesSuperSoftWonky-Medium", 
                                                                rep(NA, 100))),
                                    text_y = 
                                      elem_list_text(family = c("FrauncesSuperSoftWonky-Medium", 
                                                                rep(NA, 100)))),
               scales = "free") +
  facetted_pos_scales(
    x = list(
      cut == "Fair" ~ scale_x_log10(limits = range(diamonds$carat)),
      cut != "Fair" ~ scale_x_log10(limits = range(diamonds$carat), 
                                    labels = NULL)
    ),
    y = list(
      clarity == "I1" ~ scale_y_continuous(limits = range(diamonds$price), 
                                           labels = scales::label_dollar()),
      clarity != "I1" ~ scale_y_continuous(limits = range(diamonds$price), 
                                           labels = NULL)
    )
  ) +
  labs(title = "Diamond prices are affected by multiple variables",
       subtitle = "Size matters, but so do cut, clarity, and <span style = 'color:#FB8861FF;'>color</span>.<br>The best clarity is <span style = 'font-family:FrauncesSuperSoftWonky-MediumItalic;'>IF</span>, while the worst is <span style = 'font-family:FrauncesSuperSoftWonky-MediumItalic;'>I1</span>.<br>The best color is <span style = 'color:#FCFDBFFF;font-family:FrauncesSuperSoftWonky-MediumItalic;'>D</span>, while the worst is <span style = 'color:#51127CFF;font-family:FrauncesSuperSoftWonky-MediumItalic;'>J</span>.",
       x = "Carats",
       y = "Price",
       color = "Color:")

p



ggsave(plot = p, "images/diamonds/diamond_price_facets.jpg", device = grDevices::jpeg, width = 10, height = 8)

g <- ggplot_build(p)

cols <- unique(g$data[[1]]$colour)

cols
colorspace::swatchplot(cols)