library(tidyverse)
library(RSQLite)
library(DBI)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# Creates db connection
con <- dbConnect(RSQLite::SQLite(), "lineups-pilot-app/20200917-graphics_group.db")

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
feedback_data <- dbReadTable(con, "feedback") 
head(feedback_data)

picture_details_data <- dbReadTable(con,"picture_details")
head(picture_details_data)

users_data <- dbReadTable(con,"users") %>%
                    unique() %>%
                    filter(age > 0)

dbDisconnect(con)

results_data <- feedback_data %>%
                  left_join(picture_details_data %>% filter(trial == 0), by = "pic_id") %>%
                  left_join(users_data %>% select(-ip_address), by = "nick_name") %>%
                  mutate(rorschach = right(param_value, 1),
                         run_time = end_time - start_time,
                         correct = ifelse(obs_plot_location == response_no, 1, 0)) %>%
                  select(description, ip_address, nick_name, age, gender, academic_study,
                         start_time, end_time, run_time, 
                         pic_id, test_param, param_value, rorschach, sample_size,
                         obs_plot_location, response_no, correct, conf_level, choice_reason,
                         data_name, pic_name) %>%
                  filter(nick_name != "test")
names(results_data)

# Number of plots evaluated per participant
participant_summary <- results_data %>% 
  group_by(nick_name) %>%
  summarise(participant_count = n())
participant_summary

# Number of participants who evaluated each plot
plots_summary <- results_data %>% 
  group_by(pic_id) %>%
  summarise(plot_count = n())
plots_summary

# Participant x Plot Summary
summary <- with(results_data, table(nick_name, pic_id))
summary

results_data2 <- results_data %>%
                  left_join(participant_summary, by = "nick_name") %>%
                  left_join(plots_summary, by = "pic_id") %>%
  mutate(param_value = factor(param_value, levels = c("target-E-Hv_null-E-Lv_r0",
                                                      "target-E-Lv_null-E-Hv_r0",
                                                      #"target-M-Hv_null-M-Lv_r0",
                                                      #"target-M-Lv_null-M-Hv_r0",
                                                      "target-H-Hv_null-H-Lv_r0",
                                                      #"target-H-Lv_null-H-Hv_r0",
                                                      
                                                      #"target-H-Hv_null-E-Hv_r0",
                                                      "target-E-Hv_null-H-Hv_r0",
                                                      "target-H-Lv_null-E-Lv_r0",
                                                      "target-E-Lv_null-H-Lv_r0",
                                                      
                                                      "target-H-Hv_null-M-Hv_r0",
                                                      "target-M-Hv_null-H-Hv_r0",
                                                      "target-H-Lv_null-M-Lv_r0",
                                                      "target-M-Lv_null-H-Lv_r0",
                                                      
                                                      "target-M-Hv_null-E-Hv_r0",
                                                      "target-E-Hv_null-M-Hv_r0",
                                                      "target-M-Lv_null-E-Lv_r0",
                                                      #"target-E-Lv_null-M-Lv_r0",
                                                      
                                                      #"target-E-Hv_null-E-Hv_r1",
                                                      #"target-E-Lv_null-E-Lv_r1",
                                                      "target-M-Hv_null-M-Hv_r1",
                                                      "target-M-Lv_null-M-Lv_r1",
                                                      "target-H-Hv_null-H-Hv_r1"
                                                      #"target-H-Lv_null-H-Lv_r1"
                                                      ))
  )


# write.csv(results_data2, file = "pilot_analysis/data/graphics-group-09.17.2020.csv", row.names = F, na = "")
