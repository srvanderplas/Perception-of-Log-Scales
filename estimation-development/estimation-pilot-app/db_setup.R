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
# true_parameters <- tibble(xmin  = 3000,
#                           xmax  = 3050,
#                           xby   = 1,
#                           alpha = 130,
#                           beta  = 0.12,
#                           theta = 50,
#                           sigma = 1.5,
#                           )
# 
# dbRemoveTable(db_con, "true_parameters")
# dbWriteTable(db_con,  "true_parameters", true_parameters)
# true_parameters <- dbReadTable(db_con, "true_parameters")
true_parameters

# simulated_data -----------------------------------------------------

simulated_data  <- dbReadTable(db_con,"simulated_data")
true_parameters <- dbReadTable(db_con, "true_parameters")

# set.seed(56156)
# simulated_data <- tibble(x         = seq(true_parameters$xmin, true_parameters$xmax, true_parameters$xby),
#                          dataset1  = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin + rnorm(length(x), mean = 0, sd = true_parameters$sigma))) + true_parameters$theta,
#                          dataset2  = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin + rnorm(length(x), mean = 0, sd = true_parameters$sigma))) + true_parameters$theta,
#                          y0        = true_parameters$alpha*exp(true_parameters$beta*(x - true_parameters$xmin)) + true_parameters$theta) %>%
#   pivot_longer(cols = c("dataset1", "dataset2"),
#                names_to = "dataset",
#                values_to = "y") %>%
#   arrange(dataset, x)
# 
# dbRemoveTable(db_con, "simulated_data")
# dbWriteTable(db_con,  "simulated_data", simulated_data)
# simulated_data <- dbReadTable(db_con, "simulated_data")
simulated_data
# write.csv(simulated_data, "estimation-development/estimation-pilot-app/simulated-data.csv", row.names = F, na = "")

# plot simulated data
base_plot <- simulated_data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = y0), color = "steelblue") +
  facet_grid(~scenario) +
  theme_bw() +
  theme(aspect.ratio = 1)

linear_plot <- base_plot +
  scale_y_continuous("Population",
                     labels = comma,
                     limits = c(100, 55000),
                     breaks = seq(0, 55000, 5000),
                     minor_breaks = c())

log_plot <- base_plot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     breaks = 2^seq(0,10000,1),
                     labels = comma,
                     minor_breaks = c(2^seq(0,10000,1) + (2^seq(0,10000,1)/2), 2^seq(0,10000,1) + 3*(2^seq(0,10000,1)/4))
                     # minor_breaks = 2^seq(0,10000,1) + (2^seq(0,10000,1)/2)
                     # minor_breaks = seq(0, 55000, 5000)
                     # minor_breaks = c(156, 312)
                     # minor_breaks = c()
  )

final_plot <- linear_plot / log_plot
final_plot
# ggsave(final_plot, filename = "estimation-development/estimation-pilot-app/simulated-data-plots.png", width = 12, height = 12)

# scanario_text_data -----------------------------------------------------------

# Startrek (Tribbles)
# stardate = 4523.3 (universe year = Fri Jan 11 2267 21:58:18)
# https://trekguide.com/Stardates.htm
# 1 week span: Jan 11, 2267 (4516.69) to Jan 18, 2267 (4567.19) = 50.5 stardates
# 4516.69 - 3000 (+ 1516.69)
# Let's round to 4500 (+ 1500)

# Star Wars (Ewoks)
# Galactic Civil War 0 - 5 ABY....
# https://starwars.fandom.com/wiki/Timeline_of_galactic_history
# Start at 0 ABY (After the Battle of Yavin) (- 3000)

scenario_text_data <- dbReadTable(db_con, "scenario_text_data")
# scenario_text_data <- tibble(creature = c("tribble", "ewok"),
#                              text = c("Hi, we're Tribbles! We were taken from our native planet, Iota Germinorum IV, and brought abroad Starfleet in stardate 4500. A Starfleet scientist, Edward Larkin, genetically engineered us to increase our reproductive rate in an attempt to solve a planetary food shortage. <br> <br> The Tribble population on Starfleet over the next 50 Stardates (equivalent to 1 week universe time) is illustrated in the graph. We need your help answering a few questions regarding the population of Tribbles.",
# 
#                                       "Hi, we're Ewoks! We are native to the forest moon of Endor. After the Galactic Civil War, some Ewoks traveled offworld to help Rebel veterens as 'therapy Ewoks' and began to repopulate. <br> <br> The Ewok population After the Battle of Yavin (ABY) is illustrated in the graph. We need your help answering a few questions regarding the population of Ewoks offworld."))
# 
# dbRemoveTable(db_con, "scenario_text_data")
# dbWriteTable(db_con, "scenario_text_data", scenario_text_data)
# scenario_text_data <- dbReadTable(db_con,"scenario_text_data")
scenario_text_data

# estimation_questions ---------------------------------------------------------

# Startrek (Tribbles)
# stardate = 4523.3 (universe year = Fri Jan 11 2267 21:58:18)
# https://trekguide.com/Stardates.htm
# 1 week span: Jan 11, 2267 (4516.69) to Jan 18, 2267 (4567.19) = 50.5 stardates
# 4516.69 - 3000 (+ 1516.69)
# Let's round to 4500 (+ 1500)

# Star Wars (Ewoks)
# Galactic Civil War 0 - 5 ABY....
# https://starwars.fandom.com/wiki/Timeline_of_galactic_history
# Start at 0 ABY (After the Battle of Yavin) (- 3000)
true_parameters <- dbReadTable(db_con, "true_parameters")

estimation_questions <- dbReadTable(db_con, "estimation_questions")
# estimation_questions <- tibble(q_id = rep(c("scenario", "Q0", "QE1", "QE2", "QI1", "QI2", "QI3"),2),
#                                creature = c(rep("tribble", 7), rep("ewok", 7)),
# 
#                                          # tribble scenario
#                                qtext = c("tribble",
#                                          "Between stardates 4530 and 4540, how does the population of Tribbles change?",
#                                          "What is the population of Tribbles in stardate 4510?",
#                                          "In what stardate does the population of Tribbles reach 4,000?",
#                                          "From 4520 to 4540, the population increases by ____ Tribbles.",
#                                          "How many times more Tribbles are there in 4540 than in 4520?",
#                                          "How long does it take for the population of Tribbles in stardate 4510 to double?",
# 
#                                          # ewok scenario
#                                          "ewok",
#                                          "Between 30 and 40 ABY, how does the population of Ewoks change?",
#                                          "What is the population of Ewoks in 10 ABY?",
#                                          "In what ABY does the population of Ewoks reach 4,000?",
#                                          "From 20 ABY to 40 ABY, the population increases by ____ Ewoks.",
#                                          "How many times more Ewoks are there in 40 ABY than in 20 ABY?",
#                                          "How long does it take for the population of Ewoks in 10 ABY to double?"
#                                         ),
#                                
#                                true_value = c(NA,
#                                               
#                                               NA,
#                                               
#                                               true_parameters$alpha*exp(true_parameters$beta*(3010 - true_parameters$xmin)) + true_parameters$theta,
#                                               
#                                               log((4000 - true_parameters$theta)/true_parameters$alpha)/true_parameters$beta + true_parameters$xmin + 1500,
#                                               
#                                               (true_parameters$alpha*exp(true_parameters$beta*(3040 - true_parameters$xmin)) + true_parameters$theta) - (true_parameters$alpha*exp(true_parameters$beta*(3020 - true_parameters$xmin)) + true_parameters$theta),
#                                               
#                                               (true_parameters$alpha*exp(true_parameters$beta*(3040 - true_parameters$xmin)) + true_parameters$theta)/(true_parameters$alpha*exp(true_parameters$beta*(3020 - true_parameters$xmin)) + true_parameters$theta),
#                                               
#                                               log((((true_parameters$alpha*exp(true_parameters$beta*(3010 - true_parameters$xmin)) + true_parameters$theta)*2) - true_parameters$theta)/true_parameters$alpha)/true_parameters$beta + true_parameters$xmin,
#                                               
#                                               NA,
#                                               
#                                               NA,
#                                               
#                                               true_parameters$alpha*exp(true_parameters$beta*(3010 - true_parameters$xmin)) + true_parameters$theta,
#                                               
#                                               log((4000 - true_parameters$theta)/true_parameters$alpha)/true_parameters$beta + true_parameters$xmin - 3000,
#                                               
#                                               (true_parameters$alpha*exp(true_parameters$beta*(3040 - true_parameters$xmin)) + true_parameters$theta) - (true_parameters$alpha*exp(true_parameters$beta*(3020 - true_parameters$xmin)) + true_parameters$theta),
#                                               
#                                               (true_parameters$alpha*exp(true_parameters$beta*(3040 - true_parameters$xmin)) + true_parameters$theta)/(true_parameters$alpha*exp(true_parameters$beta*(3020 - true_parameters$xmin)) + true_parameters$theta),
#                                               
#                                               log((((true_parameters$alpha*exp(true_parameters$beta*(3010 - true_parameters$xmin)) + true_parameters$theta)*2) - true_parameters$theta)/true_parameters$alpha)/true_parameters$beta + true_parameters$xmin
#                                               
#                                )
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

# Feedback Estimates ---------------------------------------------------

feedback <- dbReadTable(db_con, "feedback")
# feedback <- tibble(ip_address = "test",
#                    nick_name  = "test",
#                    study_starttime = NA,
#                    start_time = NA,
#                    end_time   = NA,
#                    order      = NA,
#                    q_id       = NA,
#                    creature   = "test",
#                    dataset    = NA,
#                    scale      = NA,
#                    response   = "test",
#                    scratchpad = "test"
#                    )
# feedback <- feedback[0,]
# dbRemoveTable(db_con, "feedback")
# dbWriteTable(db_con, "feedback", feedback)
# feedback <- dbReadTable(db_con, "feedback")
feedback

# Feedback Estimated Data ---------------------------------------------------

feedback <- dbReadTable(db_con, "feedback")
# feedback <- tibble(ip_address = "test",
#                    nick_name  = "test",
#                    study_starttime = NA,
#                    start_time = NA,
#                    end_time   = NA,
#                    order      = NA,
#                    q_id       = NA,
#                    creature   = "test",
#                    dataset    = NA,
#                    scale      = NA,
#                    response   = "test"
#                    )
# feedback <- feedback[0,]
# dbRemoveTable(db_con, "feedback")
# dbWriteTable(db_con, "feedback", feedback)
# feedback <- dbReadTable(db_con, "feedback")
feedback

# Feedback Calculation Data -------------------------------------------

calc_feedback <- dbReadTable(db_con, "calc_feedback")
# calc_feedback <- tibble(ip_address = "test",
#                         nick_name  = "test",
#                         study_starttime = NA,
#                         q_id       = NA,
#                         creature   = "test",
#                         dataset    = NA,
#                         scale      = NA,
#                         expression = "test",
#                         evaluated  = NA
#                    )
# calc_feedback <- calc_feedback[0,]
# dbRemoveTable(db_con, "calc_feedback")
# dbWriteTable(db_con, "calc_feedback", calc_feedback)
# calc_feedback <- dbReadTable(db_con, "calc_feedback")
calc_feedback

# Disconnect to data base ---------------------------------------------

dbDisconnect(db_con)
