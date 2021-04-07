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
# exp_parameters_details <- data.frame(beta = c(0.1, 0.23), sd = c(0.09, 0.25)) %>%
#                           expand_grid(points_end_scale = c(0.5, 0.75),
#                                       points_choice = "partial",
#                                       N = 30,
#                                       aspect_ratio = 1,
#                                       free_draw = FALSE,
#                                       x_min = 0,
#                                       x_max = 20,
#                                       x_by = 0.25,
#                                       ymin_scale = 0.5,
#                                       ymax_scale = 2,
#                                       draw_start_scale = 0.5,
#                                       #linear = c("true", "false")
#                           ) %>%
#                           rownames_to_column("parm_id")
# dbRemoveTable(db_con, "exp_parameter_details")
# dbWriteTable(db_con,  "exp_parameter_details", exp_parameters_details)
# exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
exp_parameter_details

# Eye Fitting Parameter Details -----------------------------------

eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
# eyefitting_parameter_details <- tibble(
#                                   parm_id = c("S", "F", "V", "N"),
#                                   y_xbar = c(3.88, 3.9, 3.89, 4.11),
#                                   slope  = c(0.66, 0.66, 1.98, -0.70),
#                                   sigma  = c(1.3, 2.8, 1.5, 2.5),
#                                   x_min   = c(0, 0, 4, 0),
#                                   x_max   = c(20, 20, 18, 20))
# dbRemoveTable(db_con, "eyefitting_parameter_details")
# dbWriteTable(db_con,  "eyefitting_parameter_details", eyefitting_parameter_details)
# eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
eyefitting_parameter_details

# Feedback Drawn Data ---------------------------------------------------

feedback <- dbReadTable(db_con,"feedback")
# feedback <- tibble(parm_id    = NA,
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

# Simulated Data ---------------------------------------------------
simulated_data <- dbReadTable(db_con,"simulated_data")
# simulated_data <- tibble(parm_id    = NA,
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

dbDisconnect(db_con)


library(RMySQL)
conn <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "srvanderplas.com",
                       dbname = "log_scales",
                       user = "emily",
                       password = "unl-statistics")

dbWriteTable(conn, "users", users, overwrite = T, append = F)
dbWriteTable(conn, "simulated_data", simulated_data, overwrite = T, append = F)
dbWriteTable(conn, "feedback", feedback, overwrite = T, append = F)
dbWriteTable(conn,  "eyefitting_parameter_details", eyefitting_parameter_details, overwrite = T, append = F)
dbWriteTable(conn,  "exp_parameter_details", exp_parameter_details, overwrite = T, append = F)
dbWriteTable(conn, "experiment_details", experiment_details, overwrite = T, append = F)

dbDisconnect(conn)

# dbRemoveTable(conn, "users")
# dbWriteTable(conn, "users", users, append = T)
# users2 <- dbReadTable(conn,"users")


devtools::install_github("r-lib/keyring")
keyring::key_set(service = "my-database", username = "emily") # password is one sent to you by email.

# gets username
keyring::key_list("my-database")[1,2]
# gets password
keyring::key_get("my-database", keyring::key_list("my-database")[1,2])

