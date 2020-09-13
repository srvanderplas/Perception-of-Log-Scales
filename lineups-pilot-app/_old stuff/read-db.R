library(RSQLite)
library(DBI)

filename <- "data/turk2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

dbListTables(db)

experiment_details <- dbReadTable(db,"experiment_details")
# experiment_details <- experiment_details[0,]
dbRemoveTable(db, "experiment_details")
dbWriteTable(db, "experiment_details", experiment_details)
experiment_details <- dbReadTable(db,"experiment_details")
experiment_details

feedback <- dbReadTable(db,"feedback")
# feedback <- feedback[0,]
dbRemoveTable(db, "feedback")
dbWriteTable(db, "feedback", feedback)
feedback <- dbReadTable(db,"feedback")
feedback

picture_details <- dbReadTable(db,"picture_details")
# picture_details <- picture_details[0,]
dbRemoveTable(db, "picture_details")
dbWriteTable(db, "picture_details", picture_details)
picture_details <- dbReadTable(db,"picture_details")
picture_details

users <- dbReadTable(db,"users")
# users <- users[0,]
dbRemoveTable(db, "users")
dbWriteTable(db, "users", users)
users <- dbReadTable(db,"users")
users
