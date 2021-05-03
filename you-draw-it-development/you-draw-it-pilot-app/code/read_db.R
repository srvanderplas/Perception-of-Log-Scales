library(RSQLite)
library(DBI)
library(tidyverse)

# Connect to data base ---------------------------------------------

filename <- "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_data.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Experiment Details ------------------------------------------------

experiment_details <- dbReadTable(db_con,"experiment_details")
# experiment_details <- experiment_details[0,]
# experiment_details <- data.frame(experiment = "emily-you-draw-it-pilot-app",
# question   = "Use your mouse to fill in the trend in the yellow box region.",
# ydi_pp     = 12,
# trials_req = 0
# )
# dbRemoveTable(db_con, "experiment_details")
# dbWriteTable(db_con, "experiment_details", experiment_details)
# experiment_details <- dbReadTable(db_con,"experiment_details")
experiment_details

# Exponential Scale Study Parameter Details ------------------------

exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
# exp_parameter_details <- data.frame(beta = c(0.1, 0.23), 
#                                     sd = c(0.09, 0.25)) %>%
#                           expand_grid(N = 30,
#                                       x_min = 0,
#                                       x_max = 20,
#                                       x_by = 0.25
#                           )
# dbRemoveTable(db_con, "exp_parameter_details")
# dbWriteTable(db_con,  "exp_parameter_details", exp_parameter_details)
# exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
# exp_parameter_details

# Eye Fitting Parameter Details -----------------------------------

eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
# eyefitting_parameter_details <- tibble(parm_id = c("S", "F", "V", "N"),
#                                        y_xbar = c(3.88, 3.9, 3.89, 4.11),
#                                        slope  = c(0.66, 0.66, 1.98, -0.70),
#                                        sigma  = c(1.3, 2.8, 1.5, 2.5),
#                                        x_min   = c(0, 0, 4, 0),
#                                        x_max   = c(20, 20, 16, 20)) %>%
#                                 expand_grid(x_by = 0.25)
# dbRemoveTable(db_con, "eyefitting_parameter_details")
# dbWriteTable(db_con,  "eyefitting_parameter_details", eyefitting_parameter_details)
# eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
eyefitting_parameter_details

# Users Data ---------------------------------------------------

users <- dbReadTable(db_con,"users")
# users <- tibble(nick_name       = "test",
#                 study_starttime = NA,
#                 age             = NA,
#                 gender          = NA,
#                 academic_study  = NA,
#                 recruitment     = NA,
#                 ip_address      = NA
#                 )
# users <- users[0,]
# dbRemoveTable(db_con, "users")
# dbWriteTable(db_con, "users", users)
# users <- dbReadTable(db_con,"users")
users


# Feedback Drawn Data ---------------------------------------------------

feedback <- dbReadTable(db_con,"feedback")
unique(feedback$parm_id)
# feedback <- tibble(parm_id    = "test",
#                    x          = NA,
#                    y          = NA,
#                    ydrawn     = NA,
#                    linear     = NA,
#                    ip_address = "test",
#                    nick_name  = "test",
#                    study_starttime = NA,
#                    start_time = NA,
#                    end_time   = NA
#                    )
# feedback <- feedback[0,]
# dbRemoveTable(db_con, "feedback")
# dbWriteTable(db_con, "feedback", feedback)
# feedback <- dbReadTable(db_con, "feedback")
feedback
feedback %>%
  dplyr::filter(study_starttime == max(users$study_starttime)) %>%
  select(parm_id, linear) %>%
  unique()

# Simulated Data ---------------------------------------------------
simulated_data <- dbReadTable(db_con,"simulated_data")
# simulated_data <- tibble(parm_id    = "test",
#                          dataset    = NA,
#                          x          = NA,
#                          y          = NA,
#                          ip_address = "test",
#                          nick_name  = "test",
#                          study_starttime = NA
#                          )
# simulated_data <- simulated_data[0,]
# dbRemoveTable(db_con, "simulated_data")
# dbWriteTable(db_con, "simulated_data", simulated_data)
# simulated_data <- dbReadTable(db_con, "simulated_data")
simulated_data
simulated_data %>%
  dplyr::filter(study_starttime == max(users$study_starttime)) %>%
  select(parm_id) %>%
  unique()

dbDisconnect(db_con)
