library(RSQLite)
library(DBI)
library(tidyverse)
library(patchwork)
library(here)

# Connect to data base ---------------------------------------------

filename <- "estimation-development/estimation-pilot-app/estimation_data.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Experiment Details ------------------------------------------------

experiment_details <- dbReadTable(db_con,"experiment_details")
# experiment_details <- experiment_details[0,]
# experiment_details <- data.frame(experiment = "emily-log-estimation-pilot-app",
#                                  num_qs      = 14,
#                                  trials_req = 0
#                                  )
# 
# dbRemoveTable(db_con, "experiment_details")
# dbWriteTable(db_con, "experiment_details", experiment_details)
# experiment_details <- dbReadTable(db_con,"experiment_details")
experiment_details

# true_parameters --------------------------------------------------------------

true_parameters <- dbReadTable(db_con,"true_parmameters")
true_parameters <- tibble(xmin  = 3000,
                          xmax  = 3050,
                          xby   = 1,
                          alpha = 130,
                          beta  = 0.12,
                          theta = 50,
                          sigma = 1.5,
                          )

dbRemoveTable(db_con, "true_parameters")
dbWriteTable(db_con,  "true_parameters", true_parameters)
true_parameters <- dbReadTable(db_con, "true_parameters")
true_parameters

# simulated_data -----------------------------------------------------

simulated_data  <- dbReadTable(db_con,"simulated_data")
true_parameters <- dbReadTable(db_con, "true_parameters")

# set.seed(56156)
# simulated_data <- tibble(x       = seq(true_parameters$xmin, true_parameters$xmax, true_parameters$xby),
#                          tribble = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin + rnorm(length(x), mean = 0, sd = true_parameters$sigma))) + true_parameters$theta,
#                          ewok    = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin + rnorm(length(x), mean = 0, sd = true_parameters$sigma))) + true_parameters$theta,
#                          y0      = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin)) + true_parameters$theta) %>%
#   pivot_longer(cols = c("tribble", "ewok"),
#                names_to = "creature",
#                values_to = "y") %>%
#   arrange(creature, x)
# 
# dbRemoveTable(db_con, "simulated_data")
# dbWriteTable(db_con,  "simulated_data", simulated_data)
# simulated_data <- dbReadTable(db_con, "simulated_data")
simulated_data

# plot simulated data
base_plot <- simulated_data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = y0), color = "steelblue") +
  facet_grid(~creature) +
  theme_bw() +
  theme(aspect.ratio = 1)

linear_plot <- base_plot +
  scale_y_continuous("Population",
                     labels = comma)

log_plot <- base_plot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     breaks = 2^seq(0,10000,1),
                     labels = comma)

final_plot <- linear_plot / log_plot
final_plot
# ggsave(final_plot, filename = "estimation-development/estimation-pilot-app/simulated-data-plots.png", width = 12, height = 12)

# scanario_text_data -----------------------------------------------------------

scenario_text_data <- dbReadTable(db_con, "scenario_text_data")
scenario_text_data <- tibble(creature = c("tribble", "ewok"),
                             text = c("Hi, we're tribbles! We were taken from our native planet, Iota Germinorum IV, and brought abroad Starfleet in the year 3000. A Starfleet scientist, Edward Larkin, genetically engineered us to increase our reproductive rate in an attempt to solve a planetary food shortage. <br> <br> The tribble population on Starfleet over the next 50 years is illustrated in the graph below. We need your help answering a few questions regarding the population of tribbles.",
                                      
                                      "Hi, we're ewoks! We are native to the forst moon of Endor. After the Galactic Civil War, some Ewoks traveled offworld to help Rebel veterens as 'therapy Ewoks' and began to repopulate. <br> <br> The Ewok population offworld is illustrated in the graph below. We need your help answering a few questions regarding the population of Ewoks offworld."))

dbRemoveTable(db_con, "scenario_text_data")
dbWriteTable(db_con, "scenario_text_data", scenario_text_data)
scenario_text_data <- dbReadTable(db_con,"scenario_text_data")
scenario_text_data

# estimation_questions ---------------------------------------------------------

estimation_questions <- dbReadTable(db_con, "estimation_questions")
# estimation_questions <- tibble(q_id = rep(c("scenario", "Q0", "QE1", "QE2", "QI1", "QI2", "QI3"),2),
#                                creature = c(rep("tribble", 7), rep("ewok", 7)),
#                                           
#                                          # tribble scenario
#                                qtext = c("tribble",
#                                          "Between 3030 and 3040, how does the population of tribbles change?",
#                                          "What is the population of tribbles in year 3010?",
#                                          "In what year does the population of tribbles reach 4,000?",
#                                          "How many more tribbles are there in 3040 than in 3020?",
#                                          "How many times more tribbles are there in 3040 than in 3020?",
#                                          "How many years does it take for the population of tribbles in 3010 to double?",
#                                          
#                                          # ewok scenario
#                                          "ewok",
#                                          "Between 3030 and 3040, how does the population of ewoks change?",
#                                          "What is the population of ewoks in year 3010?",
#                                          "In what year does the population of ewoks reach 4,000?",
#                                          "How many more ewoks are there in 3040 than in 3020?",
#                                          "How many times more ewoks are there in 3040 than in 3020?",
#                                          "How many years does it take for the population of ewoks in 3010 to double?"
#                                         )
#                                )
# dbRemoveTable(db_con, "estimation_questions")
# dbWriteTable(db_con, "estimation_questions", estimation_questions)
# estimation_questions <- dbReadTable(db_con,"estimation_questions")
estimation_questions

# Users Data -------------------------------------------------------------------

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

feedback <- dbReadTable(db_con, "feedback")

feedback <- tibble(ip_address = "test",
                   nick_name  = "test",
                   study_starttime = NA,
                   start_time = NA,
                   end_time   = NA,
                   qNum       = NA,
                   q_id       = NA,
                   creature   = "test",
                   scale      = NA,
                   response   = "test"
                   )
feedback <- feedback[0,]
dbRemoveTable(db_con, "feedback")
dbWriteTable(db_con, "feedback", feedback)
feedback <- dbReadTable(db_con, "feedback")
feedback

# Disconnect to data base ---------------------------------------------

dbDisconnect(db_con)
