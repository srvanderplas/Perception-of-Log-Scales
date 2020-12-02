# --------------------------------------------------------------------------
# Collect Newest Pilot Data ------------------------------------------------
# --------------------------------------------------------------------------

# source("lineups-pilot-analysis/rCode/data-management.R")
library(readr)
lineup_results_data <- read.csv("lineups-pilot-analysis/data/jsm-student-paper-11302020.csv")
summary(lineup_results_data)
model_data <- lineup_results_data %>%
  filter(participant_count > 5) %>%
  mutate(test_param = factor(test_param, levels = c("log", "linear")))

# ------------------------------------------------------------
# Load packages for modeling ---------------------------------
# ------------------------------------------------------------
library(lme4)
library(emmeans)

# --------------------------------------------------------------------------
# Run GLMM Model -----------------------------------------------------------
# --------------------------------------------------------------------------

glmm_mod <- glmer(correct ~ curvature*test_param + 
                            (1 | run) +
                            (1 | data_name),
                  data = model_data,
                  family = binomial(link = "logit"))
summary(glmm_mod)
anova(glmm_mod)

# lsmeans
lsmeans <- emmeans(glmm_mod, c("curvature", "test_param"))
lsmeans <- regrid(emmeans(glmm_mod, c("curvature", "test_param")), type = "response")
lsmeans

lsmeans %>%
  as_tibble() %>%
  mutate(curvature = factor(curvature, levels = c("t-E_n-H", "t-H_n-E", "t-E_n-M", "t-M_n-E", "t-M_n-H", "t-H_n-M"))) %>%
  ggplot(aes(x = curvature, y = prob, group = test_param)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  # geom_text(aes(y = asymp.UCL + 0.05)) +
  facet_wrap(~test_param, ncol = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  theme_bw() +
  scale_y_continuous("Probability of target panel detected", limit = c(-0.05,1.1), breaks = seq(0,1,0.2)) +
  scale_x_discrete("Curvature")

# Odds Ratios
slices <- emmeans(glmm_mod, ~ test_param | curvature)
slices
odds_ratios <- pairs(slices) %>% 
                    as_tibble() %>%
  mutate(OR = exp(estimate))

odds_ratios %>%
ggplot(aes(x = OR, y = reorder(curvature, OR))) +
  geom_point() + 
  # geom_errorbar(aes(xmin = Lower_Odds_Ratio_log_linear, xmax = Upper_Odds_Ratio_log_linear), width = 0) +
  geom_vline(xintercept = 1) +
  theme_bw() +
  scale_y_discrete("") +
  scale_x_continuous("Odds ratio (on log scale) \n (Log vs Linear)", trans = "log10") +
  ggtitle("Odds Ratio of selecting the target panel on the log scale compared to the linear scale")
