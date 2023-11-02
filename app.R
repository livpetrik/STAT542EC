#install.packages('rsconnect')
library(readr)
#library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

accident <- read.csv("accident.csv")

accident <- accident[accident$HOUR != 99, ]



ui <- fluidPage(
  
  titlePanel("Accident Data"),
  
  sidebarLayout(
    sidebarPanel(selectInput("cid","Time", choices = colnames(accident[,c("MINUTE", "HOUR", "DAY", "MONTH")])),
                 selectInput("day", "Weekday", choices = unique(accident$DAY_WEEKNAME))),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("barPlot"),
      plotOutput("scatterPlot")
    )
  )
)


server <- function(input, output) {
  
  new_data <- reactive({
    accident %>%
      filter(DAY_WEEKNAME == input$day)
  })
  
  output$distPlot <- renderPlot({
    x    <- accident[, input$cid]
    bins <- seq(min(x), max(x), length.out = 12)
    
    titl <- paste("Histogram of", input$cid, sep = " ")
    h <- hist(x, breaks = bins, col = rainbow(10), border = 'white', main = titl)
    yfit <- dnorm(bins, mean = mean(x), sd = sd(x)) 
    yfit <- yfit *  diff( h$mids[1:2]) * length(x) 
    lines(bins, yfit, col = "blue")
  })
  
  output$barPlot <- renderPlot({
      new_data() %>%
        group_by(FATALS, DAY_WEEKNAME) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = FATALS, y = Count, fill = DAY_WEEKNAME)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Fatality Count by Weekday",
             x = "Fatalities",
             y = "Count")
  })
  
  output$scatterPlot <- renderPlot({
    if (input$cid == 'MINUTE'){
      ggplot(accident)+aes(x = MINUTE, y = FATALS) +
        geom_point() +
        labs(
          title = "Minutes vs Fatalities",
          x = "Minute",
          y = "Fatalities"
        ) +
        theme_minimal()}
      

    else if(input$cid == 'HOUR'){
      ggplot(accident)+ aes(x = HOUR, y = FATALS) +
        geom_point() +
        labs(
          title = "Hours vs Fatalities",
          x = "Hour",
          y = "Fatalities"
         ) +
         theme_minimal()}
      
    else if(input$cid == 'DAY'){
      ggplot(accident)+ aes(x = DAY, y = FATALS) +
        geom_point() +
        labs(
          title = "Days vs Fatalities",
          x = "Day",
          y = "Fatalities"
         ) +
         theme_minimal()}
      
    else{
      ggplot(accident)+ aes(x = MONTH, y = FATALS) +
        geom_point() +
        labs(
          title = "Months vs Fatalities",
          x = "Month",
          y = "Fatalities")+
        theme_minimal()
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)