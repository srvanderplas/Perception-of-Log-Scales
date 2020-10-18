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

# Disconnect from database
dbDisconnect(con)
