library(RSQLite)
library(DBI)
library(here)
library(tidyverse)
library(lubridate)

# ---- Set up experiment details -----------------------------------------------
experiment_id <- "emily-log-2"
question <- "Which plot is the most different?"
#reasons <- "Extreme value,Different range,Different start point,Different slope,Outlier(s),Other"
reasons <- "Clustering,Different range,Different shape,Different slope,Outlier(s),Other"
lineups_per_person <- 13 # lineups per person
trials_req <- 0


experiment_details <- tibble::tibble(
  experiment = experiment_id,
  question = question,
  reasons = reasons,
  lpp = lineups_per_person,
  trials_req = trials_req
)


# ---- Feedback -- participant answers -----------------------------------------
# This table is blank until participants actually enter the app
feedback <- tibble::tibble(ip_address = "test", nick_name = "test",
                           start_time = now(), end_time = now(),
                           pic_id = 0, response_no = 0,
                           conf_level = "test", choice_reason = "test",
                           description = "test")

# ---- Picture details -- about each lineup ------------------------------------
# The only thing that affects sampling strategy at the moment is experiments/.../randomization.R
# which creates a vector of pic_id and a vector of trial_id values
#
# Create the empty database table - this is here for documentation
picture_details <- tibble::tibble(
  pic_id = NULL, # this should be numeric and unique to the picture
  sample_size = NULL, # this can be anything -- used as a metadata field
  test_param = NULL, # this can be anything -- used as a metadata field
  param_value = NULL, # this can be anything -- metadata
  p_value = NULL, # this can be anything -- metadata -- most useful for any numerical simulation evaluation measure
  obs_plot_location = NULL, # a number, or multiple comma-separated numbers. For Rorschach lineups, use 0
  pic_name = NULL, # picture name without folder/path info
  experiment = NULL, # experiment shorthand
  difficulty = NULL, # this can be anything -- metadata -- useful for separating blocks
  data_name = NULL, # name/file of saved dataset to generate the lineup
  trial = NULL # Used to indicate if a picture is a trial plot or not...
  # added when picture_details and picture_details_trial are combined.
)

picture_details <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))

picture_details_trial <- readr::read_csv(here("lineups-pilot-app", "trials", "picture-details-trial.csv"))

pic_details <- rbind(mutate(picture_details, trial = 0), mutate(picture_details_trial, trial = 1)) %>%
  mutate(experiment = experiment_details$experiment)


# ---- Create database ---------------------------------------------------------
if (file.exists(here("lineups-pilot-app", "exp_data.db"))) {
  file.rename(here("lineups-pilot-app", "exp_data.db"), here("lineups-pilot-app", "exp_data.db.bkup"))
}

con <- dbConnect(RSQLite::SQLite(), here("lineups-pilot-app", "exp_data.db"))
dbWriteTable(con, "experiment_details", experiment_details, overwrite =T)
dbWriteTable(con, "feedback", feedback, overwrite = T)
dbWriteTable(con, "picture_details", pic_details, overwrite = T)

dbDisconnect(con)


# ---- Create examples ---------------------------------------------------------
# Examples to match the first 2 reasons in the list of possible reasons

# Extreme value
tibble(
  i = 1:5,
  x = purrr::map(i, ~seq(-10, 10, length.out = 50) + rnorm(50, 0, .2)),
  y = purrr::map(i, ~rnorm(50, 0, 1))
) %>%
  tidyr::unnest(c(x, y)) %>%
  mutate(y = x + y + c(rep(0, 3*50 + 5), 10, rep(0, 50 + 44))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = .75) +
  facet_wrap(~i, nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
ggsave(here("lineups-pilot-app/examples/example1.png"), width = 10, height = 2, dpi = 600)


# different starting point
tibble(
  i = 1:5,
  x = purrr::map(i, ~seq(-10, 10, length.out = 50) + rnorm(50, 0, .2)),
  y = purrr::map(i, ~rnorm(50, 0, 1))
) %>%
  tidyr::unnest(c(x, y)) %>%
  mutate(y = rep(c(.5, 1, 1, 1, 1), each = 50) * x + y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = .75) +
  facet_wrap(~i, nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
ggsave(here("lineups-pilot-app/examples/example2.png"), width = 10, height = 2, dpi = 600)



# Pull data out of the database...

# con <- dbConnect(RSQLite::SQLite(), here("lineups-pilot-app", "exp_data.db"))
# exp_details_db <- dbReadTable(con, "experiment_details", experiment_details)
# feedback_db <- dbReadTable(con, "feedback", feedback)
# pic_details_db <- dbReadTable(con, "picture_details", pic_details)
#
# dbDisconnect(con)


# If running on linux, change the permissions so that the
# app doesn't crash
if (str_detect(sessionInfo()$platform, "linux")) {
  system(paste0("chmod 777 ", here::here("lineups-pilot-app", "exp_data.db")))
  system(paste0("chmod 777 ", here::here("lineups-pilot-app")))
}
