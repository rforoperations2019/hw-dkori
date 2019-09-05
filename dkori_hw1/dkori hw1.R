#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(httr)
remote_data<-getURL('https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv')
guns<-read_csv(remote_data)
unique(guns$year)
head(guns)
unique(guns$intent)
unique(guns$race)
guns$date<-paste(guns$year,guns$month,'01',sep=' ')
guns$datetime<-as.Date(guns$date,format='%Y %m %d')

#inputs: 
  # checkbox to exclude suicide
  # slider for years back
  # Dropdown for line chart categories

#plots: 
 # Line Chart - dropdown chooses category
 # Seasonal Scatter - slider chooses month
 # 
ui <- fluidPage(
   
   # Application title
   titlePanel("Gun Deaths in America"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year_range",
                     "Starting Year:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

