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
reason_data <- model_data %>%
  separate(choice_reason, c("reason1", "reason2", "reason3", "reason4", "reason5", "reason6", "reason7"), sep = "[,]") %>%
  pivot_longer(cols = c("reason1", "reason2", "reason3", "reason4", "reason5", "reason6", "reason7"),
               names_to = "reason_number",
               values_to = "reasoning") %>%
  na.omit() %>%
  select(test_param, reasoning) %>%
  mutate(reasoning = str_trim(reasoning)) %>%
  filter(reasoning != "Other") %>%
  group_by(test_param, reasoning) %>%
  summarize(count = n()) %>%
  arrange(reasoning, test_param) %>%
  group_by(test_param) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(test_param, reasoning) %>%
  mutate(propdelta = (prop - mean(prop)) / sqrt(mean(prop)))


scale_labels <- c(log = "Log Scale", linear = "Linear Scale")

reason_data %>%
  ggplot(aes(label = reasoning, size = prop, color = reasoning)) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 28) +
  theme_test() +
  theme(strip.text.x = element_text(size = 12)) +
  facet_wrap(~test_param, labeller = labeller(test_param = scale_labels)) +
  scale_color_viridis_d()

plot_data %>%
  filter(count < 10) %>%
  ggplot(aes(label = reasoning, size = prop, color = reasoning)) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 8) +
  theme_test() +
  theme(strip.text.x = element_text(size = 12)) +
  facet_wrap(~test_param, labeller = labeller(test_param = scale_labels)) +
  scale_color_viridis_d()

