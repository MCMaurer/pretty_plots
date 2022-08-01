library(tidyverse)
library(MCMsBasics)
library(palmerpenguins)
library(patchwork)

p <- penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_quantile(val = penguins$bill_length_mm) +
  scale_y_quantile(val = penguins$bill_depth_mm) +
  labs(title = "Penguin Bills",
       subtitle = "Bill size varies by species, with <span style = 'color:#1B9E77;font-family:FrauncesSuperSoftWonky-MediumItalic;'>Adelies</span> having shorter,<br>deeper bills compared to <span style = 'color:#D95F02;font-family:FrauncesSuperSoftWonky-MediumItalic;'>Chinstraps</span> and <span style = 'color:#7570B3;font-family:FrauncesSuperSoftWonky-MediumItalic;'>Gentoos</span>. <br> Axis labels denote 0%, 25%, 50%, 75%, and 100% quantiles.",
       x = "Bill length (mm)",
       y = "Bill depth (mm)")

p1 <- p + theme_mcm() + theme(legend.position = "none")
p1

ggsave("images/penguins/penguin_bills_light.jpg", device = grDevices::jpeg, width = 6, height = 4)

p2 <- p + theme_mcm_dark() + theme(legend.position = "none")
p2

ggsave("images/penguins/penguin_bills_dark.jpg", device = grDevices::jpeg, width = 6, height = 4)


RColorBrewer::brewer.pal(3, "Dark2")

p1 + p2

ggsave("images/penguins/penguin_bills_both.jpg", device = grDevices::jpeg, width = 12, height = 4)
