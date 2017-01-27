# Shiny App for KWH estimates for ESSNet BigData Project
# 

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estimates for KWH on synthetic Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("socio",label=h3("Select criteria"),
                  choices = list("State"="state","Income-Groups"="HHOLD_INCOME_GROUP_CD","Household Size"="NUM_OCCUPANTS","Children younger than 10"="NUM_CHILDREN_0_10",
                                 "Children between 11 and 17"="NUM_CHILDREN_11_17","Occupants older equal 70 years"="NUM_OCCUPANTS_70PLUS",
                                 "Home during Day"="IS_HOME_DURING_DAYTIME")),
      selectInput("time",label=h3("Select Timespan"),
                  choices = list("Quarter"="QUARTER","Month"="MONTH","Weekday"="WEEKDAY","Workday-Weekend"="DAYTYPE","Daytime"="DAYTIME","Hour"="HOUR","Hour by Workday-Weekend"="DAYTYPE_HOUR")),
      conditionalPanel(
        condition = "input.time=='QUARTER'",
        selectInput("time2",label=h3("Partial period"),
                    choices=list("NULL"="EMPTY","Weekday"="WEEKDAY","Workday-Weekend"="DAYTYPE","Daytime"="DAYTIME","Hour"="HOUR"))
      )
    ),
    # Show table and plot results
    mainPanel(
      plotOutput("Plot")
    )
  ),
  tableOutput("Table")
))
