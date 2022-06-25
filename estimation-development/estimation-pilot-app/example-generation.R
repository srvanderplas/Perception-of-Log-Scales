library(tidyverse)
library(scales)
library(patchwork)

# squirrels
set.seed(56156)
example_data1 <- tibble(x  = seq(2010, 2030, 1),
                       y  = 30*exp(0.10*(x - 2015) + rnorm(length(x), mean = 0, sd = 0.1)) - 5)

linear_plot <- example_data1 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous("Year") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Squirrel Population \n (Linear Scale)", 
                     labels = comma, 
                     limits = c(0, 160), 
                     breaks = seq(0,160,20),
                     minor_breaks = c()
                     )

linear_plot
ggsave(linear_plot, filename = "estimation-development/estimation-pilot-app/www/example-linear.png", width = 4, height = 4)

# bunnies
set.seed(56156)
example_data2 <- tibble(x  = seq(1980, 2000, 1),
                       y  = 30*exp(0.15*(x - 1980) + rnorm(length(x), mean = 0, sd = 0.1)) - 5)

log_plot <- example_data2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous("Year") +
  theme_bw() +
  theme(aspect.ratio = 1) + 
  scale_y_continuous("Bunny Population \n (Log Scale)", 
                     labels = comma, 
                     trans = "log2", 
                     breaks = 2^seq(0,100,1),
                     minor_breaks = c()
                     )

log_plot
ggsave(log_plot, filename = "estimation-development/estimation-pilot-app/www/example-log.png", width = 4, height = 4)
