library(shiny)
library(tidyverse)
library(readxl)
library(maps)
library(DT)
library(lubridate)

# Load data
data <- read_excel("US Superstore data.xls") |>
  select(- "Row ID") |>
  mutate(across(where(is.character) & !all_of(c("Product ID", "Product Name")), 
                as.factor),
         `Postal Code` = as.factor(`Postal Code`),
         Order_Month = floor_date(`Order Date`, 
                                  "month"))

# Define UI
ui <- fluidPage(
  titlePanel("US Superstore Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category_var1", 
                  "Choose Category (Product Type)", 
                  choices = c("All", 
                              levels(data$Category))),
      
      selectInput("category_var2", 
                  "Choose Segment (Customer Type)", 
                  choices = c("All", 
                              levels(data$Segment))),
      
      selectInput("numeric_var1", 
                  "Select Numeric Variable 1", 
                  choices = names(select(data, where(is.numeric)))),
      uiOutput("slider1"),
      
      selectInput("numeric_var2", 
                  "Select Numeric Variable 2", 
                  choices = names(select(data, where(is.numeric)))),
      uiOutput("slider2"),
      
      actionButton("apply_filter", 
                   "Apply Filter")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", 
                 h4("App Purpose"),
                 p("This app allows users to explore data related to US Superstore sales."),
                 h4("Data Source"),
                 p("The data comes from a fictional superstore dataset on Kaggle."),
                 a(href = "https://www.kaggle.com/datasets/juhi1994/superstore/data", 
                   "US Superstore Dataset on Kaggle", 
                   target = "_blank")),
        
        tabPanel("Data Download", 
                 fluidRow(
                   column(12, 
                          downloadButton("download_data", 
                                         "Download Filtered Data"))
                 ),
                 DTOutput("data_table")),
        
        tabPanel("Data Exploration", 
                 tabsetPanel(
                   tabPanel("Map Plot", 
                            plotOutput("state_map")),
                   tabPanel("Monthly Sales Trends by Segment", 
                            plotOutput("monthly_sales_plot")),
                   tabPanel("Average Profit by Sub-Category"),
                   tabPanel("Sales Distribution Histogram"),
                   tabPanel("Quantity by Ship Mode and Segment"),
                   tabPanel("Sales vs Profit Scatter Plot")
                 )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Dynamic UI sliders for numeric variables
  output$slider1 <- renderUI({
    req(input$numeric_var1)
    range <- range(data[[input$numeric_var1]], 
                   na.rm = TRUE)
    sliderInput("slider1", 
                label = input$numeric_var1, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  output$slider2 <- renderUI({
    req(input$numeric_var2)
    range <- range(data[[input$numeric_var2]], 
                   na.rm = TRUE)
    sliderInput("slider2", 
                label = input$numeric_var2, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  # Reactive filtered data based on user inputs, triggered by apply button
  filtered_data <- eventReactive(input$apply_filter, {
    data_filtered <- data
    if (input$category_var1 != "All") {
      data_filtered <- data_filtered |> 
        filter(Category == input$category_var1)
    }
    if (input$category_var2 != "All") {
      data_filtered <- data_filtered |> 
        filter(Segment == input$category_var2)
    }
    data_filtered |> 
      filter(between(.data[[input$numeric_var1]], 
                     input$slider1[1], 
                     input$slider1[2]),
             between(.data[[input$numeric_var2]], 
                     input$slider2[1], 
                     input$slider2[2]))
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      write.csv(filtered_data(), 
                file)
    }
  )
  
  # Map Plot: Total Sales by State
  output$state_map <- renderPlot({
    req(filtered_data())
    summarized_data <- filtered_data() |> 
      group_by(State) |> 
      summarize(Total_Value = sum(Sales, 
                                  na.rm = TRUE))
    
    us_map <- map_data("state") |> 
      mutate(State = str_to_title(region))
    
    data_map <- us_map |> 
      left_join(summarized_data, 
                by = "State")
    
    ggplot(data_map, 
           aes(x = long, 
               y = lat, 
               group = group, 
               fill = Total_Value)) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_gradient(low = "lightblue", 
                          high = "darkblue", 
                          na.value = "grey90") +
      labs(title = "Total Sales by State", 
           fill = "Total Sales") +
      theme_minimal()
  })
  
  # Plot: Monthly Sales Trends by Segment
  output$monthly_sales_plot <- renderPlot({
    req(filtered_data())
    monthly_sales <- filtered_data() |> 
      group_by(Order_Month, 
               Segment) |> 
      summarize(Monthly_Sales = sum(Sales, 
                                    na.rm = TRUE))
    
    ggplot(monthly_sales, 
           aes(x = Order_Month, 
               y = Monthly_Sales, 
               color = Segment)) +
      geom_line(size = 1) +
      labs(title = "Monthly Sales Trends by Segment", 
           x = "Order Month", 
           y = "Monthly Sales") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
