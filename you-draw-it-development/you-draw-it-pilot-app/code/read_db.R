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
#                           rownames_to_column("parm_id") %>%
#                           mutate(parm_id = paste("exp_", parm_id, sep = "")) %>%
#                           mutate(parm_id = as.character(parm_id))
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
#                                   x_max   = c(20, 20, 18, 20))  %>%
#                                   mutate(parm_id = as.character(parm_id))
# dbRemoveTable(db_con, "eyefitting_parameter_details")
# dbWriteTable(db_con,  "eyefitting_parameter_details", eyefitting_parameter_details)
# eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
eyefitting_parameter_details

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
  select(parm_id, linear) %>%
  unique() %>%
  arrange(parm_id, linear)

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
  select(parm_id) %>%
  unique() %>%
  arrange(parm_id)

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

# users <- tibble(nick_name = "needatleast32charactersherexxxxxxx", study_starttime = as.character(lubridate::now()),
#                 age = 31, gender = 0, academic_study = 0, recruitment = 0, ip_address = "127.0.0.1")
# dbWriteTable(conn, "users", users, overwrite = T, append = F)
# simulated_data <- tibble(parm_id= "exp_0", dataset = "point_data", x = 0.000, y = 1.000, ip_address = "127.0.0.1", nick_name = "needatleast32charactersherexxxxxxx", study_starttime = as.character(lubridate::now()))
# dbWriteTable(conn, "simulated_data", simulated_data, overwrite = T, append = F)
# feedback <- tibble(parm_id = "exp_0", x = 0.000, y = 0.000, ydrawn = 0.000, linear = T, ip_address = "127.0.0.1", nick_name = "needatleast32charactersherexxxxxxx", study_starttime = as.character(lubridate::now()), start_time = as.character(lubridate::now()), end_time = as.character(lubridate::now()))
# dbWriteTable(conn, "feedback", feedback, overwrite = T, append = F)
# dbWriteTable(conn,  "eyefitting_parameter_details", eyefitting_parameter_details, overwrite = T, append = F)
# dbWriteTable(conn,  "exp_parameter_details", exp_parameter_details, overwrite = T, append = F)
# dbWriteTable(conn, "experiment_details", experiment_details, overwrite = T, append = F)

users2 <- dbReadTable(conn, "users", row.names = NULL)
simulated_data2 <- dbReadTable(conn, "simulated_data", row.names = NULL)
feedback2 <- dbReadTable(conn, "feedback", row.names = NULL)
eyefitting_parameter_details2 <- dbReadTable(conn, "eyefitting_parameter_details", row.names = NULL)
exp_parameter_details2 <- dbReadTable(conn, "exp_parameter_details", row.names = NULL)
experiment_details2 <- dbReadTable(conn, "experiment_details", row.names = NULL)

dbDisconnect(conn)

# dbRemoveTable(conn, "users")
# dbWriteTable(conn, "users", users, append = T)
# users2 <- dbReadTable(conn,"users")


# devtools::install_github("r-lib/keyring")
# keyring::key_set(service = "my-database", username = "emily") # password is one sent to you by email.

# gets username
keyring::key_list("my-database")[1,2]
# gets password
keyring::key_get("my-database", keyring::key_list("my-database")[1,2])
