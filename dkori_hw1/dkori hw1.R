#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyr)
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(httr)
library(RCurl)
library(scales)
remote_data<-getURL('https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv')
guns<-read_csv(remote_data)%>%
  filter(!is.na(intent))
#head(guns)
#unique(guns$intent)
#unique(guns$race)
guns$date<-paste(guns$year,guns$month,'01',sep=' ')
guns$datetime<-as.Date(guns$date,format='%Y %m %d')
guns$`Police Involved?`=ifelse(guns$police==1,"Yes","No")
getwd()

#inputs: 
  # checkbox to exclude suicide
  # slider for years back
  # Dropdown for line chart categories

#plots: 
 # Line Chart - Deaths over time by category
 # Bar Chart - deaths by age group for chosen category (stacked bar)
 # Donut Chart - percentage of deaths by each thing in category

ui <- fluidPage(
   
   # Application title
   titlePanel("Gun Deaths in America:\n2012-2015"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        #create input to select date range
         sliderInput("start_date",
                     "Select earliest date:",
                     min = min(guns$datetime),
                     max = max(guns$datetime),
                     value = min(guns$datetime)),
         #input to select whether or not suicides are included (since those are the majority of deaths)
         checkboxInput("Checkbox", "Include Suicides?",
                       value = TRUE),
         #create dropdown for user to select series grouping
         selectInput("series_choice", "Choose Series:",
                     c("Sex" = "sex",
                       "Race" = "race",
                       "Location" = "place",
                       "Education" = "education",
                       "Police Involved?" = "Police Involved?"),
                     selected="Location")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        downloadButton("download_subset","Get this data"),
        plotOutput(outputId = "line_plot"),
        plotOutput("bar_plot"),
        plotOutput("donut"),
        dataTableOutput(outputId="datatable")
        
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  #first subset of guns, whether or not suicides are included
  guns_subset1<-reactive({
    if(!input$Checkbox){
      guns%>%
        filter(intent!="Suicide")%>%
        filter(datetime>input$start_date)
    }else{
      guns%>%
        filter(datetime>input$start_date)
    }
  })
  #store series choice as a string var
  series_choice<-reactive({paste0(input$series_choice)})
  #subset guns and roll up based on 
   guns_subset2<-reactive({
     #create temp df limited to given series
     temp<-guns_subset1()[,c("datetime", "age",series_choice())]
     #rename columns (only way I know how to do this when selecting column through var)
     names(temp)<-c("datetime","age", "series_choice")
     temp
   })
   
   output$datatable=renderDataTable(
     datatable(guns_subset2()%>%
                 group_by(series_choice)%>%
                 count(name="value")%>%
                 mutate(value=comma(value)),rownames=FALSE)
   )
   output$line_plot <- renderPlot({
     
     guns_subset2()%>%
       group_by(datetime,series_choice)%>%
       count(name="value")%>%
       ggplot(aes(x=datetime,y=value,color=series_choice))+
       geom_line()+
       theme_minimal()+
       labs(x="Month",y="Number of fatalities",color=names(input$series_choice))
       
   })
   
   
   #create donut plot 
   output$donut<-renderPlot({
     guns_subset2()%>%
       group_by(series_choice)%>%
       count(name="value")%>%
       ungroup()%>%
       mutate(ymax=cumsum(value),ymin=c(0,head(ymax,n=-1)))%>%
       ggplot(aes(fill=series_choice,ymin=ymin,ymax=ymax,xmin=3,xmax=5,label=series_choice),colour="white")+
       #make chart rectangular
       geom_rect()+
       #convert rectangular chart to polar (donut)
       coord_polar(theta="y")+
       xlim(c(0,5))+
       #remove background
       theme_void() +
       #remove x and y value labels
       xlab("")+
       ylab("")+
       #remove other aesthetic elements of chart
       theme(panel.grid=element_blank()) +
       theme(axis.text=element_blank()) +
       theme(axis.ticks=element_blank()) +
       #center title
       theme(plot.title = element_text(hjust = 0.5))+
       #place legend on bottom with no legend title
       theme(legend.title=element_blank())+
       theme(legend.position="bottom")+
       #resize legend items to fit well
       guides(fill=guide_legend(nrow=2,byrow=TRUE,keywidth=.4,keyheight=.2,default.unit="inch"),
              color="none")+
       geom_label(
         aes(x=4,y=(ymax+ymin)/2,label=comma(value)),
         label.size=.175,
         show.legend=FALSE
       )
   })
   
   #Barplot by age
   output$bar_plot<-renderPlot({
     guns_subset2()%>%
      group_by(age,series_choice)%>%
      count(name="value")%>%
      ungroup()%>%
      ggplot(aes(x=age,y=value,fill=series_choice))+
      geom_bar(stat="identity",position="stack")+
      theme_minimal()+
      labs(title="fatalities by age",
           fill="",
           y="fatalities")
   })
   
   #data for download button
   output$downloadData<-reactive({
     downloadHandler(guns_subset2())
     
   })
}
# Run the application 
shinyApp(ui = ui, server = server)
