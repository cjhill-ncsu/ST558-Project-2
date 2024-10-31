library(shiny)
library(tidyverse)
library(readxl)
library(maps)
library(DT)

# Load data
data <- read_excel("US Superstore data.xls") |>
  select(-"Row ID") |>
  mutate(across(
    where(is.character) & !all_of(c("Product ID", "Product Name")),
    as.factor
  )) |>
  mutate(`Postal Code` = as.factor(`Postal Code`))

# Define UI
ui <- fluidPage(
  titlePanel("US Superstore Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category_var1", 
                  "Choose Category 1", 
                  choices = levels(data$Category)),
      
      selectInput("category_var2", 
                  "Choose Category 2", 
                  choices = levels(data$Segment)),
      
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
                   "US Superstore Dataset on Kaggle", target = "_blank")),
        
        tabPanel("Data Download", 
                 DTOutput("data_table"),
                 downloadButton("download_data", 
                                "Download Filtered Data")),
        
        tabPanel("Data Exploration", 
                 tabsetPanel(
                   tabPanel("Map Plot", 
                            plotOutput("state_map")),
                   tabPanel("Plot 2",
                            h4("Plot 2")),
                   tabPanel("Plot 3",
                            h4("Plot 3")),
                   tabPanel("Plot 4",
                            h4("Plot 4")),
                   tabPanel("Plot 5",
                            h4("Plot 5")),
                   tabPanel("Plot 6",
                            h4("Plot 6"))
                 )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$slider1 <- renderUI({
    req(input$numeric_var1)
    range <- range(data[[input$numeric_var1]], 
                   na.rm = TRUE)
    sliderInput("slider1", 
                input$numeric_var1, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  output$slider2 <- renderUI({
    req(input$numeric_var2)
    range <- range(data[[input$numeric_var2]], 
                   na.rm = TRUE)
    sliderInput("slider2", 
                input$numeric_var2, 
                min = range[1], 
                max = range[2], 
                value = range)
  })
  
  filtered_data <- reactive({
    req(input$apply_filter)
    data %>%
      filter(
        Category %in% input$category_var1,
        Segment %in% input$category_var2,
        between(.data[[input$numeric_var1]], 
                input$slider1[1], 
                input$slider1[2]),
        between(.data[[input$numeric_var2]], 
                input$slider2[1], 
                input$slider2[2])
      )
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data.csv") },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
  
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
}

shinyApp(ui = ui, server = server)
