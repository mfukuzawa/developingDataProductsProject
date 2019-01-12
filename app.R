## Coursera
## Developing Data Products, Week 4
## Shiny Projects

library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(plotly)
library(leaflet)
library(gridExtra)

airlines <- read.csv("airlines.csv", header = T)
hubs <- read.csv("airlinehubs.csv", header = T)

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("US Airline Passenger Data, 12-Jan-19"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
           width = 3,
           radioButtons("airinput", "Airline", choices = c("American","Delta","Southwest","United"),
                        selected = "American"),
           br(),
           br(),
           selectInput('air', 'Select an airline to view a scatter plot and predict the 2018 passenger load', 
                       unique(airlines$Airline), selected="American")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
           tabsetPanel(type = "tabs",
                tabPanel("README", htmlOutput("helpstuff")),
                tabPanel("Tab 1", tableOutput("airtable"), br(), br(), h4("Average Enplaned Passengers"), 
                         htmlOutput("text1"), hr(), br(), br(), h4("2018 Prediction"), textOutput("text2"), br(), br(),
                         plotOutput("plot2")),
                tabPanel("Tab 2", plotlyOutput("plot1")),
                tabPanel("Tab 3", leafletOutput("mymap")))
           )
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output){
     
     output$helpstuff <- renderUI({
          HTML(paste("In this app, we'll compare passenger loads for the big four airline companies--American (AAL), 
          Delta (DAL), Southwest (SWA), and United (UAL).", "On Tab 1 in the sidebar panel, select the airline from the radio buttons, 
                     which populates a five-year table of passenger loads; an average passenger load is also calculated. 
                     In the lower half of the tab, select an airline from the drop-down menu in the sidebar panel, which 
                     produces a scatterplot for the passenger loads for each airline, along with a smoother and a prediction 
                     for the 2018 passenger load.", "Tab 2 displays a plotly scatterplot with all of the airline data.", 
                     "Tab 3 shows a leaflet map with the corresponding hubs in the continental US. The airline groups 
                     are toggled by the layer icon on the left side of the map.", "References:", 
                     "investors.southwest.com/news-and-events/news-releases/...",
                     "https://hub.united.com/united-reports-operational-performance-2567373623.html",
                     "ir.united.com/static-files/049fdc19-db16-4efd-8923-56f331335ea8",
                     "https://ir.delta.com/stock-and-financial/traffic-releases/default.aspx",
                     "http://news.aa.com/news/default.aspx",
                     sep = "<br/> <br/>"))
     })
     
     output$airtable <- renderTable({
          airlines_adj <- airlines %>% filter(Airline == input$airinput)
     })
     
     output$text1 <- renderUI({
          airlines$Passengers <- as.numeric(gsub(",","",airlines$Passengers))
          airlines2 <- airlines %>% filter(Airline == input$airinput)
          airavg <- mean(airlines2$Passengers)
          HTML(paste(comma(airavg),""))
     })
     
     airplot <- airlines %>% group_by(Airline) %>%
          ggplot(aes(x = CY, y = Passengers, shape = Airline, color = Airline)) + geom_point(size = 3)
     
     output$plot1 <- renderPlotly({
          g <- ggplotly(airplot)
          g
     })
     
     model.air <- reactive({
          fit1 <- lm(Passengers~CY, data=filter(airlines, Airline==input$air))
          newdf <- data.frame(2018)
          colnames(newdf) <- "CY"
          predict(fit1,newdf)[[1]]
     })
     
     output$text2 <- renderText({
          comma(model.air())
     })
     
     output$plot2 <- renderPlot({
          airplot2 <- airlines %>% filter(Airline==input$air) %>%
               ggplot(aes(x = CY, y = Passengers)) + ggtitle(input$air) + 
               theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=14,face="bold")) +
               geom_point(size = 3) + 
               geom_smooth(method = "lm")
          airplot2
       })
     
     #changing style of icon
     icons_aal <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = 'blue'
     )
     
     icons_dal <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = 'gray'
     )
     
     icons_swa <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = 'red'
     )
     
     icons_ual <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = 'green'
     )
     
     group_aal <- hubs %>% filter(Airline == 'AAL')
     group_dal <- hubs %>% filter(Airline == 'DAL')
     group_swa <- hubs %>% filter(Airline == 'SWA')
     group_ual <- hubs %>% filter(Airline == 'UAL')
     
     output$mymap <- renderLeaflet({
          leaflet(hubs) %>%
               addTiles() %>%
               setView(-95.7, 37.1, 3) %>%
               addAwesomeMarkers(group_aal$Longitude,group_aal$Latitude,icon=icons_aal, 
                                 popup=paste0(group_aal$Airport, ', ',group_aal$Code),
                                 group = 'AAL') %>%
               addAwesomeMarkers(group_dal$Longitude,group_dal$Latitude,icon=icons_dal, 
                                 popup=paste0(group_dal$Airport, ', ',group_dal$Code),
                                 group = 'DAL') %>%
               addAwesomeMarkers(group_swa$Longitude,group_swa$Latitude,icon=icons_swa,
                                 popup=paste0(group_swa$Airport, ', ',group_swa$Code),
                                 group = 'SWA') %>%
               addAwesomeMarkers(group_ual$Longitude,group_ual$Latitude,icon=icons_ual,
                                 popup=paste0(group_ual$Airport, ', ',group_ual$Code),
                                 group = 'UAL') %>%
               addLayersControl(position = "topleft", overlayGroups = c('AAL','DAL','SWA','UAL')) %>%
               hideGroup(c("DAL","SWA","UAL"))
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

