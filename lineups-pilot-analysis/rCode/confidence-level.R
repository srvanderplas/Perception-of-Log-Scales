# LOAD LIBRARIES
library(readr)
library(tidyverse)
library(ggwordcloud)

# IMPORT DATA
lineup_results_data <- read.csv("lineups-pilot-analysis/data/jsm-student-paper-11302020.csv")
model_data <- lineup_results_data %>%
  filter(participant_count > 5) %>%
  mutate(test_param = factor(test_param, levels = c("log", "linear")))
summary(lineup_results_data)

# PARTICIPANT REAONSING
conf_data <- lineup_results_data  %>%
  select(test_param, conf_level) %>%
  group_by(test_param, conf_level) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))


scale_labels <- c(log = "Log Scale", linear = "Linear Scale")
# set.seed(42)
conf_data %>%
  mutate(test_param = factor(test_param, levels = c("log", "linear"))) %>%
  ggplot(aes(label = conf_level, size = prop, color = conf_level, angle = 90*round(runif(nrow(conf_data),0,1)))) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 28) +
  theme_test() +
  theme(strip.text.x = element_text(size = 12)) +
  facet_wrap(~test_param, labeller = labeller(test_param = scale_labels)) +
  scale_color_viridis_d()


conf_data %>%
  mutate(test_param = factor(test_param, levels = c("log", "linear"))) %>%
  mutate(conf_level = factor(conf_level, levels = c("Very Certain", "Certain", "Neutral", "Uncertain", "Very Uncertain"))) %>%
  ggplot(aes(x = conf_level, y = prop, group = test_param, fill = test_param)) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(size = 0.5),
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 12),
        legend.key.size = unit(1, "line")
  ) +
  theme(strip.text.x = element_text(size = 12)) +
  scale_fill_manual("Scale", values = c("#116611", "#55aa55"), labels = c("Log", "Linear")) +
  scale_y_continuous("Proportion within Scale", limits = c(0,1),breaks = seq(0,1,0.1)) +
  scale_x_discrete("Confidence Level")

