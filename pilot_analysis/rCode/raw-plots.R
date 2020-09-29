source("pilot_analysis/rCode/data-management.R")
names(results_data2)

results_data2 %>%
  ggplot(aes(x = test_param, y = correct, color = test_param)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.9) +
  facet_wrap(~param_value) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Paired")


results_data2 %>% 
  group_by(nick_name, test_param) %>%
  filter(rorschach == 0) %>%
  summarise(plots_correct = sum(correct),
            plot_count = n()) %>%
  mutate(prop_correct = plots_correct/plot_count) %>%
  filter(plot_count > 6) %>%
ggplot(aes(x = test_param, y = prop_correct, fill = test_param)) +
  geom_bar(stat = "identity") +
  facet_wrap(~nick_name, ncol = 4) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))

results_data2 %>% 
  group_by(param_value, test_param) %>%
  summarise(plots_correct = sum(correct),
            participant_count = n()) %>%
  mutate(prop_correct = plots_correct/participant_count) %>%
  filter(participant_count > 3) %>%
ggplot(aes(x = test_param, y = prop_correct, fill = test_param)) +
  geom_bar(stat = "identity") +
  facet_wrap(~param_value, ncol = 5) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))

