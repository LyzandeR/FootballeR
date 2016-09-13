#load needed packages
library(shiny)
library(data.table)
library(highcharter)
library(DT)
library(tableHTML)

#initital teams
init_teams <- c("Arsenal",
                "Aston Villa",
                "Burnley",
                "Chelsea",
                "Crystal Palace",
                "Everton",
                "Hull",
                "Leicester",
                "Liverpool",
                "Man City",
                "Man United",
                "Newcastle",
                "QPR",
                "Southampton",
                "Stoke",
                "Sunderland",    
                "Swansea",
                "Tottenham",
                "West Brom",
                "West Ham")

#use function to set NAs to zero
na_converter <- function(dt, value = 0) {
  
 for (j in names(dt)) set(dt, which(is.na(dt[[j]])), j, value)
  
 dt
  
}