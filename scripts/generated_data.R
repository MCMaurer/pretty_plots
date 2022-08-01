library(tidyverse)
library(ggside)
library(slider)
library(patchwork)
library(MCMsBasics)


# gaussian by addition ----------------------------------------------------

nsteps <- 50
nreps <- 1000

c(rep(0, nreps), runif(nsteps*nreps, -0.2, 0.2)) %>% 
  matrix(nrow = nreps) %>% 
  t() %>% 
  as_tibble() %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  mutate(cumu_sum = slide_dbl(value, sum, .before = Inf),
         step = seq_len(n()),
         highlight = name == paste0("V", nreps)) %>% 
  ggplot(aes(x = step, y = cumu_sum, group = name)) +
  geom_line(data = . %>% 
              filter(!highlight), alpha = 100/nreps) +
  geom_line(data = . %>% 
              filter(highlight), color = "red") +
  geom_ysidehistogram(data = . %>% filter(step == nsteps), 
                      group = 1, bins = 100, fill = "grey15") +
  scale_ysidex_continuous(breaks = NULL) +
  theme_mcm() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Gaussian distribution by addition",
       subtitle = "At each step, a random uniform value between -0.2 and 0.2 <br> is added to the value. The histogram on the right represents <br> the endpoint of each path. The <span style = 'color:#FF0000;font-family:FrauncesSuperSoftWonky-MediumItalic;'>red line</span> is a single, randomly <br> chosen path.",
       x = "Step",
       y = "Value")

ggsave("images/generated_data/gaussian_dist_by_addition.jpg", width = 6, height = 4, device = grDevices::jpeg)
