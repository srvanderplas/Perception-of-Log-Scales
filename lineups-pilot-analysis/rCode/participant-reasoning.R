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
  group_by(test_param) %>%
  mutate(total_evaluations = n()) %>%
  separate(choice_reason, c("reason1", "reason2", "reason3", "reason4", "reason5", "reason6", "reason7"), sep = "[,]") %>%
  pivot_longer(cols = c("reason1", "reason2", "reason3", "reason4", "reason5", "reason6", "reason7"),
               names_to = "reason_number",
               values_to = "reasoning") %>%
  na.omit() %>%
  select(test_param, reasoning, total_evaluations) %>%
  mutate(reasoning = str_trim(reasoning)) %>%
  # filter(reasoning %in% c("Outlier(s)","Different range", "Different slope", "Different shape", "Clustering", "Other")) %>%
  group_by(test_param, total_evaluations, reasoning) %>%
  summarize(count = n()) %>%
  arrange(reasoning, test_param, total_evaluations) %>%
  group_by(test_param, total_evaluations) %>%
  mutate(prop = count / total_evaluations) %>%
  group_by(test_param, total_evaluations) %>%
  mutate(propdelta = (prop - mean(prop)) / sqrt(mean(prop)))


scale_labels <- c(log = "Log Scale", linear = "Linear Scale")
reason_data %>%
  filter(count > 2) %>%
  mutate(test_param = factor(test_param, levels = c("linear", "log"))) %>%
  mutate(reasoning = factor(reasoning, levels = c("Different shape", "Different slope", "Outlier(s)", "Different range", "Clustering", "Other"))) %>%
  ggplot(aes(x = reasoning, y = prop, group = test_param, fill = test_param)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_fill_manual("Scale", values = c("#116611", "#55aa55"), labels = c("Linear", "Log")) +
  scale_y_continuous("Proportion \n (relative to total evaluations per scale)", limits = c(0,1),breaks = seq(0,1,0.1)) +
  scale_x_discrete("Choice Reason")


# reason_data %>%
#   mutate(test_param = factor(test_param, levels = c("linear", "log"))) %>%
#   ggplot(aes(label = reasoning, size = prop, color = reasoning)) +
#   geom_text_wordcloud_area(shape = "circle") +
#   scale_size_area(max_size = 28) +
#   theme_test() +
#   theme(strip.text.x = element_text(size = 12)) +
#   facet_wrap(~test_param, labeller = labeller(test_param = scale_labels)) +
#   scale_color_viridis_d()

other_log_reasons <- reason_data %>%
  filter(test_param == "log", count < 2) %>%
  rename(Log = reasoning) %>%
  mutate(id = row_number()) %>%
  select(id, Log)

other_linear_reasons <- reason_data %>%
  filter(test_param == "linear", count < 2) %>%
  rename(Linear = reasoning) %>%
  mutate(id = row_number()) %>%
  select(id, Linear)

other_reasons <- other_linear_reasons %>%
  full_join(other_log_reasons, by = "id") %>%
  select(Linear, Log)
write.csv(other_reasons, file = "lineups-pilot-analysis/results/other-reasons.csv", row.names = F, na = "")
