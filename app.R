library(shiny)
### Fetching and configuring the weather data from FMI using api
fmi_weather <- function(location,date) {
  
  days<-seq(Sys.Date(), by = "day", length.out = 7) #creating array f dates starting at current day.
  url<-paste("http://ilmatieteenlaitos.fi/saa/",location,sep="") #making url with input city name
  fmi<-xml2::read_html(url) #fetching data
  weather<-rvest::html_table(rvest::html_nodes(fmi, "table")) #making table of data
  #the following is organizing and cleaning up the table to make it more understandable
  w <- list()
  for(i in c(2:8)){
    w[[as.character(i)]]<-weather[[i]] 
    names(w[[as.character(i)]]) <- c("Time","NA","TempHigh (\u00B0C)","TempLow  (\u00B0C)","Windspeed (m/s)","Precipitation")
    w[[as.character(i)]] <- w[[as.character(i)]][,-2]
  }
  names(w) <- days
  for(i in names(w)) w[[i]]$day <- i
  
  neww <- rbind(w[[1]],w[[2]])
  for(i in c(3:7)) neww <- rbind(neww,w[[i]])
  
  neww <- neww[neww$day==date,]
  
  
  return(neww)
}

# 

days<-seq(Sys.Date(), by = "day", length.out = 7)  #creating list of input das for shiny app
#image_time<-
#Shiny app user interface
ui <- fluidPage(
  
  # App title ----
  titlePanel("FMI Weather Forecast"),
  textInput("City","Input City (no ä/ö/å allowed)",value='helsinki'), #input searchable city name ()
  
  
  #optional date selection, default is current day
  selectInput("day",
              label="Date",
              choices = days,
              selected = days[1]),
  
  # Main panel for displaying outputs ----
  mainPanel(width = 15,
            tabPanel('Weather',DT::dataTableOutput('mytable')), #creating table in app
            
            #additonally I added an image of whole of Finland's weather for current day
            mainPanel(
              h2("Weather in Finland now"),
              htmlOutput("picture"),label="Date"))
  
)



# Define server logic required to make table and image ----
server <- function(input, output) {
  #output weather forecast table
  output$mytable<-DT::renderDataTable({
    DT::datatable(caption = 'Weather Forecast',
                  options = list(pageLength = 30),
                  class='cell-border',
                  as.data.frame(fmi_weather(location=input$City, date = input$day)))}) #calling weather function
  
  #output weather map of finland
  output$picture <-
    renderText({
      c(
        '<img src="',  #calling weather map
        paste("https://cdn.fmi.fi/weather-observations/products/finland/frontpage_wx_obs-latest.png?t=",paste(gsub(days[1],pattern = "-",replacement = ""),"0000",sep=""),sep=""),
        '">'
      )
    })
  
  output
  
  
}

shinyApp(ui = ui, server = server)
