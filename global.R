## Paqueterías
library(shiny)
library(shinydashboard)
library(shinyauthr)
library(sodium)
library(dplyr)
library(lubridate)
library(DBI)
library(RSQLite)
library(DT)
library(leaflet)



# connect to, or setup and connect to local SQLite db
if (file.exists("srv/shiny-server/my_db_file")) {
  db <- dbConnect(SQLite(), "srv/shiny-server/my_db_file")
} else {
  db <- dbConnect(SQLite(), "srv/shiny-server/my_db_file")
  dbCreateTable(db, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
  
  dbCreateTable(db, "user_base", c(user = "TEXT", password = "TEXT", permissions = "TEXT", name= "TEXT"))
}


# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 7 # Days until session expires

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}





