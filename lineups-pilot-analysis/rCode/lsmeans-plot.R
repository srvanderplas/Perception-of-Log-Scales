lsmeans <- read.csv("manuscripts/jsm-2021-student-paper-submission/results/jsm-student-paper-lsmeans.csv") %>%
  extract(curvature, into = c("Target", "Null"), "t-([MEH])_n-([EMH])", remove = F) %>%
  mutate(Target = factor(Target, levels = c("E", "M", "H"), labels = c("Easy", "Medium", "Hard")),
         Null = factor(Null, levels = c("E", "M", "H"), labels = c("Easy", "Medium", "Hard")))

dodge <- position_dodge(width=0.9)
lsmeans_plot <- lsmeans %>%
  ggplot(aes(x = Null, y = Mu, group = Target, fill = Target)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = LowerMu, ymax = UpperMu), width = 0.1, position = dodge) +
  # geom_text(aes(y = UpperMu + 0.05), size = 2, position = dodge) +
  facet_wrap(~test_param, ncol = 2) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(size = 0.5),
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 12),
        legend.key.size = unit(1, "line")
  ) +
  scale_y_continuous("Probability of Detecting Target Panel", limit = c(0,1.1), breaks = seq(0,1,0.2)) +
  scale_x_discrete("Null Plot Type") +
  scale_fill_manual("Target Plot Type", values = c("#004400", "#116611", "#55aa55"))
lsmeans_plot
