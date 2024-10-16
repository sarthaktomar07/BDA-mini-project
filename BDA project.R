# Required Libraries
library(shiny)
library(quantmod)
library(plotly)
library(DT)
library(TTR)

# UI Code
ui <- fluidPage(
  titlePanel("NIFTY Stock Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateInput("startDate", "Start Date:", value = as.Date("2000-01-01"), min = as.Date("2000-01-01"), max = Sys.Date()), 
      dateInput("endDate", "End Date:", value = Sys.Date(), min = as.Date("2000-01-01"), max = Sys.Date()),  
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 DTOutput("stockTable")
        ),
        tabPanel("Stock Charts",
                 plotlyOutput("closingVsOpeningChart"),
                 plotlyOutput("highVsLowChart"),
                 plotlyOutput("volumeChart"),
                 plotlyOutput("candlestickChart"),
                 plotlyOutput("movingAvgChart"),
                 plotlyOutput("rsiChart")
        )
      ),
      width = 9
    )
  )
)

# Server Code
server <- function(input, output, session) {
  
  stock_data <- reactive({
    req(input$startDate, input$endDate)  
    start_date <- as.Date(input$startDate)
    end_date <- as.Date(input$endDate)
    
    # Fetch the NIFTY stock data dynamically (Symbol for NIFTY is "^NSEI" on Yahoo Finance)
    stock_df <- quantmod::getSymbols("^NSEI", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    stock_df <- data.frame(Date = index(stock_df), coredata(stock_df))  # Convert to data frame
    colnames(stock_df) <- gsub(paste0("^", "^NSEI", "."), "", colnames(stock_df))  
    return(stock_df)
  })
  
  output$stockTable <- renderDT({
    df <- stock_data()
    req(df)
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # chart No. 1 - Closing vs Opening chart (dynamic)
  output$closingVsOpeningChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = 'Close') %>%
      add_lines(y = ~Open, name = 'Open') %>%
      layout(title = "NIFTY Closing vs Opening Prices")
  })
  
  # chart No. 2 - High vs Low chart
  output$highVsLowChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~High, type = 'scatter', mode = 'lines', name = 'High') %>%
      add_lines(y = ~Low, name = 'Low') %>%
      layout(title = "NIFTY High vs Low Prices")
  })
  
  # chart No. 3 -  Volume chart
  output$volumeChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~Volume, type = 'bar', name = 'Volume') %>%
      layout(title = "NIFTY Trading Volume")
  })
  
  # chart No. 4 -  Candlestick chart
  output$candlestickChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, type = "candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
      layout(title = "NIFTY Candlestick Chart")
  })
  
  # chart No. 5 - Moving average chart (50-day SMA)
  output$movingAvgChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    df$MA50 <- TTR::SMA(df[, "Close"], n = 50)
    
    plot_ly(df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = 'Close') %>%
      add_lines(y = ~MA50, name = '50-Day Moving Average') %>%
      layout(title = "NIFTY 50-Day Moving Average")
  })
  
  # chart No. 6 - RSI chart (14-day RSI)
  output$rsiChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    df$RSI <- RSI(df[, "Close"], n = 14)
    
    plot_ly(df, x = ~Date, y = ~RSI, type = 'scatter', mode = 'lines', name = 'RSI') %>%
      layout(title = "NIFTY Relative Strength Index (RSI)")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
