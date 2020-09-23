library(tidyverse)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "lineups-pilot-app/20200917-graphics_group.db")

dbListTables(con)

tbl(con, "experiment_details")

# tbl() needs a database connection and a table name
tbl(con, "feedback") %>%
  # normal dplyr works
  group_by(pic_id) %>%
  count() %>%
  # collect() pulls the results into memory instead of just
  # working within the database
  collect() -> res

tbl(con, "picture_details")
