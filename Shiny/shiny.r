#First we have to import librarys
source("library.r", encoding="UTF-8")


# Load data
trend_data <- read_csv("Data/koef.csv",col_types = cols(Time = col_character()))

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Congested grids"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "name2", label = strong("Name of Grid"),
                                choices = unique(trend_data$Name2)[order(unique(trend_data$Name2))],
                                selected = "Diele - Meeden 380 Black [DIR] [DE]"),
                    
                    selectInput(inputId = "time", label = strong("Hour"),
                                choices = unique(trend_data$Time),
                                selected = "00:00:00"),
                    
                    selectInput(inputId = "day", label = strong("Day of the week"),
                                choices = unique(trend_data$Day)[order(unique(trend_data$Day))],
                                selected = 1),
                    
                    numericInput(inputId = "AT", label = strong("PTDF for AT"),
                                 value = 0.58860, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "BE", label = strong("PTDF for BE"),
                                 value = 0.25637, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "DE", label = strong("PTDF for DE"),
                                 value = 0.58860, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "FR", label = strong("PTDF for FR"),
                                 value = 0.38150, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "NL", label = strong("PTDF for NL"),
                                 value = 0, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "ALBE", label = strong("PTDF for ALBE"),
                                 value = -0.30955, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "ALDE", label = strong("PTDF for ALDE"),
                                 value = 0, min = -1, max = 1, step = 0.00001),
                    
                    numericInput(inputId = "RAM", label = strong("RAM"),
                                 value = 1399, min = 0, max = 99999, step = 0.01),
                    
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    tableOutput(outputId = "table"),
                    plotOutput(outputId = "lineplot", height = "300px")
                    # textOutput(outputId = "desc"),
                    # tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  hh2 <- lmer(c ~ AT + BE + DE + FR + NL + ALBE + ALDE + RAM + (1|Name2/Day) + (1|Name2/Time), data = trend_data)
  
  output$lineplot <- renderPlot({
    # plot(1,1, type='l')
    trend_data1 <- trend_data[trend_data$Name2==input$name2,]
    trend_data1 <- trend_data1[trend_data1$Time==input$time,]
    plot(trend_data1$c, type='l')
    #color = "#434343"
    #par(mar = c(4, 4, 1, 1))
    # plot(selected_trends()$c, type = "l",
    #      xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    #if(input$smoother){
    # print(1+1)
    #}
  })
  
  # Pull in description of trend
  output$table <- renderTable({
    
    PTDF <- data.frame("Day"=input$day,"Name2"=input$name2, "Time"=input$time, "AT"=input$AT, "BE"=input$BE, "DE"=input$DE, "FR"=input$FR, "NL"=input$NL, "ALBE"=input$ALBE, "ALDE"=input$ALDE, "RAM"=input$RAM)
    
    out <- tryCatch(
      {
        v = predict(hh2,PTDF)
      },
      error=function(cond) {
        return(0)
      }#,
      # finally = {
      #   v
      # }
    )
    interval <- c(out - 1.96*sigma(hh2), out + 1.96*sigma(hh2))
    if (out == 0){hh = data.frame("Change some parameters!" = out)}
    else{
      hh = data.frame("low boundary" = interval[1], "value" = out, "High boundary" = interval[2], "RAM" = input$RAM, "RAM - Value" = input$RAM - out)}
    print(hh)
  })
}