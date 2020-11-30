library(tidyverse)
library(RSQLite)
library(DBI)
library(svglite)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# Creates db connection
con <- dbConnect(RSQLite::SQLite(), "lineups-pilot-app/exp_data.db")

# List of datasets contained in db
dbListTables(con)

# Summary of picture ID's evaluated
# tbl() needs a database connection and a table name
res <- tbl(con, "feedback") %>%
  # normal dplyr works
  group_by(pic_id) %>%
  count() %>%
  # collect() pulls the results into memory instead of just
  # working within the database
  collect()
res

# Combine feedback, picture_details, and users
experiment_details_data <- dbReadTable(con, "experiment_details") 

feedback_data <- dbReadTable(con, "feedback") 

picture_details_data <- dbReadTable(con,"picture_details")

users_data <- dbReadTable(con,"users") %>%
  unique()

dbDisconnect(con)

feedback_data <- feedback_data %>%
  filter(nick_name != "test") %>%
  arrange(nick_name, start_time) %>%
  group_by(nick_name) %>%
  mutate(end_time_lag = lag(end_time)) %>%
  mutate(same_time = ifelse(start_time < end_time_lag + 30, 1, 0),
         run = NA) %>%
  mutate(same_time = ifelse(is.na(same_time), 0, 
                            ifelse(same_time == 0, 0, 1))) %>%
  select("ip_address", "nick_name", "start_time", "end_time", "end_time_lag", "same_time", "run", "pic_id", 
         "response_no", "conf_level", "choice_reason", "description")

feedback_data$run[1] <- 1
for(i in 2:nrow(feedback_data)){
  feedback_data$run[i] <- ifelse(feedback_data$same_time[i] == 1, feedback_data$run[(i-1)], feedback_data$run[(i-1)]+1)
}

lineup_results_data_raw <- feedback_data %>%
                  left_join(picture_details_data %>% filter(trial == 0), by = "pic_id") %>%
                  # left_join(users_data %>% select(-ip_address), by = "nick_name") %>%
                  mutate(rorschach = right(param_value, 1),
                         run_time = end_time - start_time,
                         correct = ifelse(obs_plot_location == response_no, 1, 0)) %>%
                  select(description, ip_address, nick_name,
                         start_time, end_time, run_time, run, 
                         pic_id, test_param, param_value, rorschach, sample_size,
                         obs_plot_location, response_no, correct, conf_level, choice_reason,
                         data_name, pic_name)
names(lineup_results_data_raw)

# Number of plots evaluated per participant
participant_summary <- lineup_results_data_raw %>% 
  group_by(run) %>%
  summarise(participant_count = n())
participant_summary

# Number of participants who evaluated each plot
plots_summary <- lineup_results_data_raw %>% 
  group_by(pic_id) %>%
  summarise(plot_count = n())
plots_summary

lineup_results_data <- lineup_results_data_raw %>%
                  filter(rorschach == 0) %>%
                  left_join(participant_summary, by = "run") %>%
                  left_join(plots_summary, by = "pic_id") %>%
                  mutate(
                         # nick_name          = factor(nick_name),
                         # age                = factor(age),
                         description        = factor(description),
                         # gender             = factor(gender),
                         # academic_study     = factor(academic_study),
                         run                = factor(run),
                         pic_id             = factor(pic_id),
                         test_param         = factor(test_param),
                         rorschach          = factor(rorschach),
                         conf_level         = factor(conf_level),
                         choice_reason      = factor(choice_reason),
                         data_name          = factor(data_name),
                         pic_name           = factor(pic_name),
                         target_curvature   = factor(substr(param_value,8,8), levels = c("E", "M", "H")),
                         target_variability = factor(substr(param_value,10,11), levels = c("Lv", "Hv")),
                         null_curvature     = factor(substr(param_value,18,18), levels = c("E", "M", "H")),
                         null_variability   = factor(substr(param_value,20,21), levels = c("Lv", "Hv"))
                      ) %>%
  mutate(curvature = paste("t-", target_curvature, "_n-", null_curvature, sep ="")) %>%
  select(
         description, 
         # ip_address, 
         nick_name,
         # age, 
         # gender, 
         # academic_study, 
         start_time, 
         end_time, 
         run_time,
         run,
         data_name, 
         pic_name, 
         pic_id, 
         test_param, 
         param_value, 
         rorschach, 
         curvature, 
         target_curvature, 
         null_curvature,
         target_variability, 
         null_variability, 
         sample_size, 
         obs_plot_location, 
         response_no, 
         correct, 
         conf_level, 
         choice_reason, 
         participant_count, 
         plot_count
         )

write.csv(lineup_results_data, file = "lineups-pilot-analysis/data/jsm-student-paper-11302020.csv", row.names = F, na = "")

rm(list=setdiff(ls(), "lineup_results_data"))
