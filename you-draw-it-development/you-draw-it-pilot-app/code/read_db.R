library(RSQLite)
library(DBI)

filename <- "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_exp_data.db"
# filename <- "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_exp_data-original.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db_con)

experiment_details <- dbReadTable(db_con,"experiment_details")
# experiment_details <- experiment_details[0,]
experiment_details <- data.frame(experiment = "emily-log-you-draw-it-pilot-app",
                                 question   = "Use your mouse to fill in the trend in the yellow box region.",
                                 ydi_pp     = 8,
                                 trials_req = 0
                                 )
# dbRemoveTable(db_con, "experiment_details")
# dbWriteTable(db_con, "experiment_details", experiment_details)
experiment_details

exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
exp_parameters <- data.frame(beta = c(0.1, 0.23), sd = c(0.09, 0.25)) %>%
  expand_grid(points_end_scale = c(0.5, 0.75), 
              points_choice = "partial", 
              N = 30, 
              aspect_ratio = 1, 
              free_draw = FALSE, 
              x_min = 0,
              x_max = 20,
              x_by = 0.25,
              ymin_scale = 0.5,
              ymax_scale = 2,
              draw_start_scale = 0.5,
              linear = c("true", "false")
  ) %>%
  rownames_to_column(var = "parm_id")
# dbRemoveTable(db_con, "exp_parameter_details")
dbWriteTable(db_con,  "exp_parameter_details", exp_parameters)
exp_parameters_details <- dbReadTable(db_con,"exp_parameter_details")
exp_parameters_details

feedback <- dbReadTable(db_con,"feedback")
# feedback <- feedback[0,]
feedback <- tibble(ip_address = "test",
                   nick_name  = "test",
                   start_time = NA,
                   end_time   = NA,
                   parm_id    = NA,
                   x          = NA,
                   y          = NA,
                   ydrawn     = NA
                   )
# dbRemoveTable(db_con, "feedback")
dbWriteTable(db_con, "feedback", feedback)
feedback <- dbReadTable(db_con,"feedback")
feedback

users <- dbReadTable(db_con,"users")
# users <- users[0,]
# dbRemoveTable(db_con, "users")
dbWriteTable(db_con, "users", users)
users <- dbReadTable(db_con,"users")
users

dbDisconnect(db_con)
