library(shinydashboard)
library(shiny)
library(dplyr)


ui <- fluidPage(
  
  titlePanel("Shiny - First Interactive Visualization Example"),
  
  sidebarLayout(
    sidebarPanel(
      # dateInput("date_from", h3("Select From Date"), value = Sys.Date()),
      # dateInput("date_to", h3("Select To Date"), value = Sys.Date()),
      dateRangeInput('dateRange',
        label = 'provide a date range',
        start = as.Date('2020-01-01') , end = as.Date('2021-02-11')
      ),
      width = 4
    ), 
    
    mainPanel(
      dataTableOutput("table"), 
      width = 8
    ), 
    
    position = c("left", "right"),
    fluid = TRUE
  )
  
  
)

server <- function(input, output) {
  
  # Sample data
  Bookings = read.csv("Booking_Dump_new.csv")
  
  Bookings$DATE <- as.Date(Bookings$DATE, format = "%d-%m-%Y")
  
  Bookings$DATE = format(Bookings$DATE, "%Y-%m-%d")
  
  # from_date <- reactive(input$date_from) 
  # # to_date <- reactive(as.Date(input$date_to, format = "%Y-%m-%d")) 

  observeEvent(input$dateRange,{
    filtered_data <- Bookings %>% filter(DATE >= input$dateRange[1] & DATE <= input$dateRange[2])
    DF <- filtered_data %>%
      group_by(Customer_City) %>%
      summarise(
        total_pax = sum(Pax, na.rm = T))%>%
      arrange(Customer_City)

    topten  = top_n(DF,10)
    output$table <- renderDataTable(topten)
  })

  
  # 
  
  
}

shinyApp(ui = ui, server = server)

