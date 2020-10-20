# --------------------------------------------------------------------------
# Load Additional Libaries for Models --------------------------------------
# --------------------------------------------------------------------------
library(lme4)

# --------------------------------------------------------------------------
# Collect Newest Pilot Data ------------------------------------------------
# --------------------------------------------------------------------------

source("lineups-pilot-analysis/rCode/data-management.R")
lineup_model_data <- lineup_results_data %>%
  filter(participant_count > 6, 
         rorschach == "0",
         target_variability == null_variability,
         )
summary(lineup_model_data)

# export data to csv for sas modeling
# write.csv(lineup_results_data, file = "lineups-pilot-analysis/data/lineup_results_data.csv", row.names = F, na = "")
# write.csv(lineup_model_data, file = "lineups-pilot-analysis/data/lineup_model_data.csv", row.names = F, na = "")

# --------------------------------------------------------------------------
# Binomial Model -----------------------------------------------------------
# --------------------------------------------------------------------------

glmm_mod <- glmer(correct ~ target_curvature*null_curvature*target_variability*test_param + 
                            (1 | nick_name),
                  data = lineup_model_data,
                  family = binomial(link = "logit"))
summary(glmm_mod)
anova(glmm_mod)
