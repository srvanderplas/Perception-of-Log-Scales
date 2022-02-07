library(tidyverse)
library(scales)
library(patchwork)

set.seed(56156)
example_data <- tibble(x  = seq(2010, 2030, 1),
                       y  = 30*exp(0.10*(x - 2015) + rnorm(length(x), mean = 0, sd = 0.1)) - 5)

base_plot <- example_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous("Year") +
  theme_bw() +
  theme(aspect.ratio = 1)

linear_plot <- base_plot + 
  scale_y_continuous("Squirrel Population", labels = comma, limits = c(0, 160), breaks = seq(0,160,20))
ggsave(linear_plot, filename = "estimation-development/estimation-pilot-app/www/example-linear.png", width = 4, height = 4)


log_plot <- base_plot + 
  scale_y_continuous("Squirrel Population", labels = comma, trans = "log2", limits = c(8, 160), breaks = 2^seq(0,100,1))
ggsave(log_plot, filename = "estimation-development/estimation-pilot-app/www/example-log.png", width = 4, height = 4)

linear_plot + log_plot
