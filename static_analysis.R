library(tidyverse)
library(reshape2)
library(readxl)
library(maps)

data <- read_excel("US Superstore data.xls")

str(data)

na_counts <- data |>
  summarize(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), 
               names_to = "Column", 
               values_to = "NA_Count")

na_counts

data <- data |> select(-"Row ID")

data <- data |>
  mutate(across(
    where(is.character) & !all_of(c("Product ID", "Product Name")),
    as.factor
  )) |>
  mutate(`Postal Code` = as.factor(`Postal Code`)) |>
  mutate(`Discount` = as.factor(`Discount`))

str(data)
summary(data)
str(data)

summarize_by_category <- function(data, category_var) {
  
  # Check if the input variable is a factor
  if (!is.factor(data[[category_var]])) {
    stop(paste(category_var, "must be a factor."))
  }
  
  numeric_vars <- data |>
    select(where(is.numeric)) |>
    colnames()
  
  # Group data by the categorical variable
  grouped_data <- data |>
    group_by(across(all_of(category_var)))
  
  # Loop over numeric variables to apply summary statistics
  for (var in numeric_vars) {
    cat("\n", paste(var, "by", category_var), "\n")
    
    # Summarize each numeric variable for each category level
    summary_data <- grouped_data |>
      summarise(
        Q1 = quantile(get(var), 0.25, na.rm = TRUE),
        Mean = mean(get(var), na.rm = TRUE),
        Median = median(get(var), na.rm = TRUE),
        Q3 = quantile(get(var), 0.75, na.rm = TRUE),
        SD = sd(get(var), na.rm = TRUE)
      )
    
    print(as.data.frame(summary_data))
  }
}

# Example usage: Summarize numeric variables by 'Gender'
summarize_by_category(data, "Region")




# Function to aggregate data by state and generate a map plot based on the chosen metric
get_data_map_plot <- function(data, 
                              metric = "Profit") {
  
  num_vars <- colnames(data |> select(where(is.numeric)))
  
  if (!(metric %in% colnames(data)) || 
      !is.numeric(data[[metric]])) {
    stop("Metric must be: ",
         paste(num_vars, collapse = ", "))
  }
  
  # Summarize data by State
  summarized_data <- data |>
    group_by(State) |>
    summarize(Total_Value = sum(.data[[metric]], na.rm = TRUE))
  
  # Load U.S. map data and prepare for merging
  us_map <- map_data("state") |>
    mutate(State = str_to_title(region)) 
  
  # Merge map data with summarized data
  data_map <- us_map |>
    left_join(summarized_data, by = "State")
  
  # Generate the plot with dynamic title
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
    labs(title = paste("Total", metric, "by State"), 
         fill = paste("Total", metric)) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank())
}

get_data_map_plot(data, 
                  metric = "Sales")








# Monthly Sales Trends by Customer Segment
data <- data |> 
  mutate(
    Order_Month = floor_date(`Order Date`, "month")
  )
monthly_sales <- data |> 
  group_by(
    Order_Month, 
    Segment
  ) |> 
  summarise(
    Monthly_Sales = sum(
      Sales, 
      na.rm = TRUE
    )
  )

ggplot(monthly_sales, 
       aes(
         x = Order_Month, 
         y = Monthly_Sales, 
         color = Segment
       )
) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Sales Trends by Segment", 
    x = "Order Month", 
    y = "Monthly Sales"
  ) +
  theme_minimal()

# Discount vs. Sales and Profit by Region with Scatter Plot
ggplot(data, 
       aes(
         x = as.numeric(Discount), 
         y = Sales, 
         color = Region
       )
) +
  geom_point(alpha = 0.6) +
  facet_wrap(
    ~ Region
  ) +
  labs(
    title = "Impact of Discount on Sales by Region", 
    x = "Discount", 
    y = "Sales"
  ) +
  theme_minimal()

# Quantity by Discount Level across Shipping Modes with Violin Plot
ggplot(data, 
       aes(
         x = Discount, 
         y = Quantity, 
         fill = `Ship Mode`
       )
) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Quantity Distribution by Discount Level and Shipping Mode", 
    x = "Discount", 
    y = "Quantity"
  ) +
  theme_minimal()

# Average Profit by Sub-Category with Bar Plot
subcat_profit <- data |> 
  group_by(
    `Sub-Category`
  ) |> 
  summarise(
    Avg_Profit = mean(
      Profit, 
      na.rm = TRUE
    )
  )

ggplot(subcat_profit, 
       aes(
         x = reorder(`Sub-Category`, Avg_Profit), 
         y = Avg_Profit, 
         fill = Avg_Profit
       )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Profit by Sub-Category", 
    x = "Sub-Category", 
    y = "Average Profit"
  ) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue"
  ) +
  theme_minimal()