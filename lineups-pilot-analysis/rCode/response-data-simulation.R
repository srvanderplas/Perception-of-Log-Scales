library(here)
library(tidyverse)
load("lineups-pilot-analysis/data/sim_response_data.rda")

# --------------------------------------------------------------------------
# Simulate Response Data----------------------------------------------------
# --------------------------------------------------------------------------
nparticipants = 40
participant_var = 0.2
data_var = 0.15
wp_var = 0.23

# funciton to simulate one individual's plots --------------------------------------------------

samplingPicID <- function(participant, num_param_values = 9, param_values_per_participant = 6, num_sets = 2){
    
    picture_details_randomization <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))
    picture_details_randomization$set <- rep(c(1,1,2,2),num_param_values) 

    # Select param values per participant
    param_details <- picture_details_randomization
    unique_param_values <- unique(param_details$param_value)
    param_block_ids <- tibble(param_id = sample(1:length(unique_param_values), param_values_per_participant)) %>%
                 mutate(param_value = unique_param_values[param_id],
                        set  = sample(1:num_sets,param_values_per_participant, replace = T)) %>%
                 expand_grid(test_param = c("linear", "log"))
    
    joinCols = c("test_param", "param_value", "set")
    pic_ids <- sample(right_join(picture_details_randomization, param_block_ids, by = joinCols)$pic_id, 2*(param_values_per_participant))
}

# set up participant's plot ids --------------------------------------------------

picture_details <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))
picture_details$set <- rep(c(1,1,2,2),9)
set.seed(56156)
sim_response_data <- tibble(nick_name = 1:nparticipants) %>%
                     mutate(pic_id = map(nick_name, samplingPicID)) %>%
                     unnest(pic_id) %>%
                     full_join(picture_details, by = c("pic_id")) %>%
                     mutate(pic_id             = factor(pic_id),
                            test_param         = factor(test_param),
                            data_name          = factor(data_name),
                            pic_name           = factor(pic_name),
                            target_curvature   = factor(substr(param_value,8,8), levels = c("E", "M", "H")),
                            target_variability = factor(substr(param_value,10,11), levels = c("Lv")),
                            null_curvature     = factor(substr(param_value,18,18), levels = c("E", "M", "H")),
                            null_variability   = factor(substr(param_value,20,21), levels = c("Lv")),
                            rorschach          = factor(substr(param_value,24,24), levels = c("0", "1")),
                            wp                 = as.factor(as.numeric(interaction(nick_name, data_name)))
                            ) %>%
                     filter(rorschach == 0) %>%
                     mutate(curvature = paste("t-", target_curvature, "_n-", null_curvature, sep =""))

# Set-up eta effects ------------------------------------------

set.seed(56156)
unique_phat_ijk <- sim_response_data %>%
  select(target_curvature, null_curvature, test_param) %>%
  unique() %>%
  mutate(phat_ijk = map(target_curvature, function(target_curvature){phat_ijk = runif(1, min = 0.1, max = 0.9)})) %>%
  unnest(phat_ijk)

unique_nickname_l <- sim_response_data %>%
  select(nick_name) %>%
  unique() %>%
  mutate(nick_name_l = map(nick_name, function(nick_name){nick_name_l = rnorm(1, 0, sqrt(participant_var))})) %>%
  unnest(nick_name_l)

unique_dataname_m <- sim_response_data %>%
  select(data_name) %>%
  unique() %>%
  mutate(data_name_m = map(data_name, function(data_name){data_name_m = rnorm(1, 0, sqrt(data_var))})) %>%
  unnest(data_name_m)

unique_wp_ijlm <- sim_response_data %>%
  select(target_curvature, null_curvature, nick_name, data_name) %>%
  unique() %>%
  mutate(wp_ijlm = map(nick_name, function(data_name){wp_ijlm = rnorm(1, 0, sqrt(wp_var))})) %>%
  unnest(wp_ijlm)

# funciton to simulate response ---------------------------------------------

# i = target curvature
# j = null curvature
# k = scale
# l = nick name / participant
# m = dataset

sampleResponse <- function(phat_ijk, nick_name_l, data_name_m, wp_ijlm){
  eta = log(phat_ijk/(1-phat_ijk)) + nick_name_l + data_name_m + wp_ijlm
  p = 1/(1+exp(-eta))
  rbinom(1, 1, p)
}

# combine eta effects into the simulated dataset for response simulation ----------------------------

sim_response_data2 <- sim_response_data %>%
                      full_join(unique_phat_ijk, by = c("target_curvature", "null_curvature", "test_param")) %>%
                      full_join(unique_nickname_l, by = c("nick_name")) %>%
                      full_join(unique_dataname_m, by = c("data_name")) %>%
                      full_join(unique_wp_ijlm, by = c("target_curvature", "null_curvature", "nick_name", "data_name")) %>%
                      mutate(correct = pmap(list(phat_ijk, nick_name_l, data_name_m, wp_ijlm), sampleResponse)) %>%
                      unnest(correct)

# final simualted data ------------------------------------------------------------------------------

sim_response_data <- sim_response_data2
save(sim_response_data, file = "lineups-pilot-analysis/data/sim_response_data.rda")
write.csv(sim_response_data, file = "lineups-pilot-analysis/data/sim_response_data.csv", row.names = F)

# --------------------------------------------------------------------------
# Create Labeler -----------------------------------------------------------
# --------------------------------------------------------------------------

target_curvature.labs <- c("Target: Lots of Curvature", "Target: Medium Curvature", "Target: Little Curvature")
names(target_curvature.labs) <- c("E", "M", "H")

null_curvature.labs <- c("Null: Lots of Curvature", "Null: Medium Curvature", "Null: Little Curvature")
names(null_curvature.labs) <- c("E", "M", "H")

global_labeller <- labeller(
  target_curvature = target_curvature.labs,
  null_curvature = null_curvature.labs)

# --------------------------------------------------------------------------
# Plot Simulated Data ------------------------------------------------------
# --------------------------------------------------------------------------

p_curvature <- sim_response_data %>%
  ggplot(aes(x = test_param, y = correct, group = test_param, color = test_param)) +
  # geom_jitter(width = 0.15, height = 0.15, alpha = 0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0.1, dodge.width = 1), alpha = 0.5) +
  theme_bw() +
  facet_grid(
    null_curvature~target_curvature,
    labeller = global_labeller
  ) +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Target panel detected", breaks = c(0,1)) +
  scale_x_discrete("Target panel curvature embeded in null panel curvature") +
  scale_color_brewer(name = "Scale", labels = c("Linear", "Log"), palette = "Paired")
p_curvature

# --------------------------------------------------------------------------
# GLM model ----------------------------------------------------------------
# --------------------------------------------------------------------------
library(lme4)
names(sim_response_data)

glm.mod <- glmer(correct ~ curvature*test_param +
                           (1 | nick_name) +
                           (1 | data_name) +
                           (curvature | nick_name:data_name),
                 family = binomial(link = "logit"),
                 data = sim_response_data
                )
length(unique(sim_response_data$wp))*6
summary(glm.mod)
anova(glm.mod)
