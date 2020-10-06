source("pilot_analysis/rCode/data-management.R")
names(results_data2)

target_curvature.labs <- c("Target: E", "Target: M", "Target: H")
names(target_curvature.labs) <- c("E", "M", "H")

null_curvature.labs <- c("Null: E", "Null: M", "Null: H")
names(null_curvature.labs) <- c("E", "M", "H")

target_variability.labs <- c("Target: Lv", "Target: Hv")
names(target_variability.labs) <- c("Lv", "Hv")

null_variability.labs <- c("Null: Lv", "Null: Hv")
names(null_variability.labs) <- c("Lv", "Hv")

global_labeller <- labeller(Target_Curvature = target_curvature.labs,
                            Null_Curvature = null_curvature.labs,
                            Target_Variability = target_variability.labs,
                            Null_Variability = null_variability.labs)

p_curvature <- results_data2 %>%
  filter(Null_Variability == Target_Variability, Rorschach_Plot == "r0") %>%
  ggplot(aes(x = test_param, y = correct, color = test_param)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.9) +
  facet_grid(
    Target_Variability + Null_Variability ~ Target_Curvature + Null_Curvature,
    labeller = global_labeller
  )+
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Accuracy within Variability \n (Between Curvatures)")
p_curvature
ggsave(plot = p_curvature, filename = "p_curvature_raw.svg", path = "presentations/eskridge-PhD-seminars/oct_8_2020/images", device = "svg")


p_variability <- results_data2 %>%
  filter(Null_Variability != Target_Variability, Rorschach_Plot == "r0") %>%
  ggplot(aes(x = test_param, y = correct, color = test_param)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.9) +
  facet_grid(
    Target_Variability + Null_Variability ~ Target_Curvature + Null_Curvature,
    labeller = global_labeller
  )+
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Accuracy within Curvature \n (Between Variabilities)")
p_variability
ggsave(plot = p_variability, filename = "p_variability_raw.svg", path = "presentations/eskridge-PhD-seminars/oct_8_2020/images", device = "svg")


p_rorschach <- results_data2 %>%
  filter(Rorschach_Plot == "r1") %>%
  ggplot(aes(x = test_param, y = correct, color = test_param)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.9) +
  facet_grid(
    Target_Variability + Null_Variability ~ Target_Curvature + Null_Curvature,
    labeller = global_labeller
  )+
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Accuracy within Rorschach")
p_rorschach
ggsave(plot = p_rorschach, filename = "p_rorschach_raw.svg", path = "presentations/eskridge-PhD-seminars/oct_8_2020/images", device = "svg")


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

