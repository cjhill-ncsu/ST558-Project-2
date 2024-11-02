library(shiny)
library(tidyverse)
library(readxl)
library(maps)
library(DT)
library(lubridate)
library(plotly)

# Load data
data <- read_excel("US Superstore data.xls") |>
  select(- "Row ID") |>
  mutate(across(where(is.character) & 
                  !all_of(c("Product ID", "Product Name")), 
                as.factor),
         `Postal Code` = as.factor(`Postal Code`),
         Order_Month = floor_date(`Order Date`, "month"))

# Define reusable message
no_filter_message <- tags$h3("Please select a subset of the data by pressing the Apply Filter button in the sidebar.")

# Define UI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  titlePanel("US Superstore Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category_var1", 
                  "Choose Category (Product Type)", 
                  choices = c("All", levels(data$Category))),
      
      selectInput("category_var2", 
                  "Choose Segment (Customer Type)", 
                  choices = c("All", levels(data$Segment))),
      
      selectInput("numeric_var1", 
                  "Select Numeric Variable 1", 
                  choices = names(select(data, where(is.numeric)))),
      uiOutput("slider1"),
      
      selectInput("numeric_var2", 
                  "Select Numeric Variable 2", 
                  choices = names(select(data, where(is.numeric)))),
      uiOutput("slider2"),
      
      actionButton("apply_filter", 
                   "Apply Filter",
                   class = "btn-primary btn-lg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", 
                 img(src = "logo.jpg", height = "100px", width = "auto"),
                 
                 h4("App Purpose"),
                 p("This app allows users to explore data related to US Superstore sales."),
                 
                 h4("Data Source"),
                 p("The data comes from a fictional superstore dataset on Kaggle."),
                 a(href = "https://www.kaggle.com/datasets/juhi1994/superstore/data", 
                   "US Superstore Dataset on Kaggle", 
                   target = "_blank"),
                 
                 h4("Sidebar"),
                 p("The sidebar allows users to filter the data based on product type, customer type, and/or two numeric variables."),
                 
                 h4("Data Download"),
                 p("Users can download the filtered data as a CSV file."),
                 
                 h4("Data Exploration"),
                 p("Here various plots are available to explore the data further.")
                 ),
        
        
          tabPanel("Data Download", 
             fluidRow(
               column(12, uiOutput("conditional_download"))
             ),
             DTOutput("data_table")),
          
          tabPanel("Data Exploration", 
             tabsetPanel(
               tabPanel("Map Plot", 
                        plotlyOutput("state_map")),  
               tabPanel("Monthly Sales Trends by Segment", 
                        plotlyOutput("monthly_sales_plot")),  
               tabPanel("Average Profit by Sub-Category", 
                        plotlyOutput("subcat_profit_plot")),  
               tabPanel("Sales Distribution Histogram", 
                        plotlyOutput("sales_distribution_plot")),  
               tabPanel("Quantity by Ship Mode and Segment", 
                        plotlyOutput("quantity_shipmode_segment_plot")),  
               tabPanel("Sales vs Profit Scatter Plot", 
                        plotlyOutput("sales_profit_scatter_plot")),  
               tabPanel("Numeric Summary Statistics", 
                  fluidRow(
                    column(6, selectInput("summary_category",
                        "Choose Category for Summary", 
                        choices = names(select(data, 
                                               where(is.factor)) |> 
                         select(-c("Order ID", 
                                   "Customer ID", 
                                   "Customer Name"))))
                    ),
                    column(6, selectInput("summary_numeric", 
                         "Choose Numeric Variable for Summary", 
                         choices = names(
                           select(data, where(is.numeric))))
                    )
                  ),
                  DTOutput("summary_stats"))
               )
            )
        )
      )
    ),
  
  # Footer for the conditional message
  tags$footer(
    uiOutput("footer_message")
  )
)


# Define server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output, session) {
  
  # Dynamic UI sliders for numeric variables
  output$slider1 <- renderUI({
    req(input$numeric_var1)
    range <- range(data[[input$numeric_var1]], na.rm = TRUE)
    sliderInput("slider1", 
                label = input$numeric_var1, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  output$slider2 <- renderUI({
    req(input$numeric_var2)
    range <- range(data[[input$numeric_var2]], na.rm = TRUE)
    sliderInput("slider2", 
                label = input$numeric_var2, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  # Don't allow the same variable in var2 as in var1
  observeEvent(input$numeric_var1, {
    numeric_vars <- names(select(data, 
                                 where(is.numeric)))
    available_choices <- setdiff(numeric_vars, 
                                 input$numeric_var1)
    updateSelectInput(session, "numeric_var2", 
                      choices = available_choices, 
                      selected = ifelse(input$numeric_var2 %in%
                                          available_choices, 
                                        input$numeric_var2, 
                                        available_choices[1]))
  })
  
  # Filter data on Apply Filter press
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

  # Track if Apply Filter has been pressed
  filter_applied <- reactiveVal(FALSE)
  observeEvent(input$apply_filter, {
    filter_applied(TRUE)
  })
  
  # Conditionally Download button or message
  output$conditional_download <- renderUI({
    if (filter_applied()) {
      div(
        class = "p-3",  
        downloadButton(
          "download_data", 
          "Download Filtered Data",
          class = "btn-primary btn-lg"
        )
      )
    } 
  })
  
  # Conditional message in the About tab
  output$footer_message <- renderUI({
    if (!filter_applied()) {
      no_filter_message  
    }
  })
  
  # Data table for display
  output$data_table <- renderDT({
    req(filter_applied())
    datatable(filtered_data())
  })
  
  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
  
  # Map Plot by State 
  output$state_map <- renderPlotly({
    req(filtered_data())
    summarized_data <- filtered_data() |> 
      group_by(State) |> 
      summarize(Total_Value = sum(Sales, na.rm = TRUE))
    
    us_map <- map_data("state") |> 
      mutate(State = str_to_title(region))
    
    data_map <- us_map |> 
      left_join(summarized_data, by = "State")
    
    gg <- ggplot(data_map, 
                 aes(x = long, y = lat, 
                     group = group, 
                     fill = Total_Value)) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_gradient(low = "lightblue", 
                          high = "darkblue", 
                          na.value = "grey90") +
      labs(title = "Sales by State", 
           fill = "Sales") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Monthly Sales by Segment 
  output$monthly_sales_plot <- renderPlotly({
    req(filtered_data())
    monthly_sales <- filtered_data() |> 
      group_by(Order_Month, Segment) |> 
      summarize(Monthly_Sales = sum(Sales, na.rm = TRUE))
    
    gg <- ggplot(monthly_sales, 
                 aes(x = Order_Month, y = Monthly_Sales, 
                     color = Segment)) +
      geom_line(size = 1) +
      labs(title = "Monthly Sales Trends by Segment", 
           x = "Order Month", y = "Monthly Sales") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Average Profit by Sub-Category 
  output$subcat_profit_plot <- renderPlotly({
    req(filtered_data())
    subcat_profit <- filtered_data() |> 
      group_by(`Sub-Category`) |> 
      summarize(Avg_Profit = mean(Profit, na.rm = TRUE))
    
    gg <- ggplot(subcat_profit, 
                 aes(x = reorder(`Sub-Category`, Avg_Profit), 
                     y = Avg_Profit, 
                     fill = Avg_Profit)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Average Profit by Sub-Category", 
           x = "Sub-Category", y = "Average Profit") +
      scale_fill_gradient(low = "lightblue", 
                          high = "darkblue") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Sales Distribution Histogram 
  output$sales_distribution_plot <- renderPlotly({
    req(filtered_data())
    gg <- ggplot(filtered_data(), aes(x = Sales)) +
      geom_histogram(binwidth = 50, 
                     fill = "blue", 
                     color = "black", 
                     alpha = 0.7) +
      labs(title = "Sales Distribution Histogram", 
           x = "Sales", y = "Frequency") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Aggregated Quantity by Ship Mode and Segment 
  output$quantity_shipmode_segment_plot <- renderPlotly({
    req(filtered_data())
    quantity_data <- filtered_data() |> 
      group_by(`Ship Mode`, Segment) |> 
      summarize(Total_Quantity = sum(Quantity, na.rm = TRUE))
    
    gg <- ggplot(quantity_data, 
                 aes(x = `Ship Mode`, 
                     y = Total_Quantity, 
                     fill = Segment)) +
      geom_bar(stat = "identity", 
               position = "dodge") +
      labs(title = "Total Quantity by Ship Mode and Segment", 
           x = "Ship Mode", y = "Total Quantity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(gg)
  })
  
  # Sales vs Profit Scatter Plot with Discount Gradient 
  output$sales_profit_scatter_plot <- renderPlotly({
    req(filtered_data())
    gg <- ggplot(filtered_data(), 
                 aes(x = Sales, 
                     y = Profit, 
                     color = Discount)) +
      geom_point(alpha = 0.6) +
      scale_color_gradient(low = "blue", 
                           high = "red") +
      labs(title = "Sales vs Profit with Discount Gradient", 
           x = "Sales", y = "Profit", 
           color = "Discount") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Summary Statistics Table
  output$summary_stats <- renderDT({
    req(input$summary_category)
    req(input$summary_numeric)
    req(filtered_data())
    
    # Calculate summary statistics
    summary_data <- filtered_data() |> 
      group_by(across(all_of(input$summary_category))) |> 
      summarize(
        Min = min(.data[[input$summary_numeric]], 
                  na.rm = TRUE),
        Q1 = quantile(.data[[input$summary_numeric]], 0.25, 
                      na.rm = TRUE),
        Mean = mean(.data[[input$summary_numeric]], 
                    na.rm = TRUE),
        Median = median(.data[[input$summary_numeric]], 
                        na.rm = TRUE),
        Q3 = quantile(.data[[input$summary_numeric]], 0.75, 
                      na.rm = TRUE),
        Max = max(.data[[input$summary_numeric]], 
                  na.rm = TRUE),
        SD = sd(.data[[input$summary_numeric]], 
                na.rm = TRUE),
        .groups = "drop"
      )
    
    datatable(summary_data, 
              options = list(pageLength = 10)) |> 
      formatRound(columns = c("Min", "Q1", "Mean", "Median", "Q3", "Max", "SD"),
                  digits = 3)
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)
