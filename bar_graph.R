library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


Bookings = read.csv("Booking_Dump_new.csv")

Bookings$DATE <- as.Date(Bookings$DATE, format = "%d-%m-%Y")

Bookings$DATE = format(Bookings$DATE, "%Y-%m-%d")


DF <- Bookings %>%
  group_by(Customer_City) %>%
  summarise(
    total_pax = sum(Pax, na.rm = T),
  ) %>%
  arrange(Customer_City)



ui <- fluidPage(
  titlePanel("Title"),
  mainPanel(
    dataTableOutput("TableOut"),
    plotOutput("Histo")
  )
)

server <- function(input, output) {
  DF2 <- top_n(DF, 10)
  output$Histo <- renderPlot({
    coul <- brewer.pal(5, "Set2") 
    bp <- barplot(height=DF2$total_pax, names=DF2$Customer_City, col=coul , xlab="Cities", 
                  ylab="pax count", 
                  main="City wise pax",ylim=c(0,30000))
    text(bp, 0 ,  DF2$total_pax ,cex=.9,pos=3)
  })
  
}

shinyApp(ui = ui, server = server)


 
