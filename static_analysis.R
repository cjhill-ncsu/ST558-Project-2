# Load libraries
library(tidyverse)
library(reshape2)
library(readxl)
library(maps)
library(lubridate)

# Load data
data <- read_excel("US Superstore data.xls")

# Initial data inspection and cleaning
str(data)

na_counts <- data |> 
  summarize(across(everything(), 
                   ~ sum(is.na(.)))) |> 
  pivot_longer(everything(), 
               names_to = "Column", 
               values_to = "NA_Count")

print(na_counts)

data <- data |> 
  select(- "Row ID") |> 
  mutate(across(where(is.character) & !all_of(c("Product ID", "Product Name")), 
                as.factor)) |> 
  mutate(`Postal Code` = as.factor(`Postal Code`),
         `Discount` = as.numeric(`Discount`))

str(data)
summary(data)

# Helper function to summarize numeric variables by category
summarize_by_category <- function(data, category_var) {
  
  if (!is.factor(data[[category_var]])) {
    stop(paste(category_var, "must be a factor."))
  }
  
  numeric_vars <- colnames(data |> 
                             select(where(is.numeric)))
  grouped_data <- data |> 
    group_by(across(all_of(category_var)))
  
  for (var in numeric_vars) {
    cat("\n", 
        paste(var, 
              "by", 
              category_var), 
        "\n")
    
    summary_data <- grouped_data |> 
      summarise(Q1 = quantile(get(var), 0.25, na.rm = TRUE),
                Mean = mean(get(var), na.rm = TRUE),
                Median = median(get(var), na.rm = TRUE),
                Q3 = quantile(get(var), 0.75, na.rm = TRUE),
                SD = sd(get(var), na.rm = TRUE))
    
    print(as.data.frame(summary_data))
  }
}

# Test
summarize_by_category(data, "Region")

# PLOTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_data_map_plot <- function(data, metric = "Sales") {
  summarized_data <- data |> 
    group_by(State) |> 
    summarize(Total_Value = sum(.data[[metric]], 
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
    labs(title = paste("Total", metric, 
                       "by State"), 
         fill = paste("Total", metric)) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank())
}

get_data_map_plot(data, metric = "Sales")


# Sales by Segment
data <- data |> 
  mutate(Order_Month = floor_date(`Order Date`, 
                                  "month"))

monthly_sales <- data |> 
  group_by(Order_Month, 
           Segment) |> 
  summarise(Monthly_Sales = sum(Sales, 
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

# Ave Profit by Sub-Category
subcat_profit <- data |> 
  group_by(`Sub-Category`) |> 
  summarise(Avg_Profit = mean(Profit, 
                              na.rm = TRUE))

ggplot(subcat_profit, 
       aes(x = reorder(`Sub-Category`, Avg_Profit), 
           y = Avg_Profit, 
           fill = Avg_Profit)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Profit by Sub-Category", 
       x = "Sub-Category", 
       y = "Average Profit") +
  scale_fill_gradient(low = "lightblue", 
                      high = "darkblue") +
  theme_minimal()

# Histogram of Sales
ggplot(data, 
       aes(x = Sales)) +
  geom_histogram(binwidth = 50, 
                 fill = "blue", 
                 color = "black", 
                 alpha = 0.7) +
  labs(title = "Sales Distribution Histogram", 
       x = "Sales", 
       y = "Frequency") +
  theme_minimal()

# Qty by Ship Mode and Segment
quantity_data <- data |> 
  group_by(`Ship Mode`, 
           Segment) |> 
  summarize(Total_Quantity = sum(Quantity, 
                                 na.rm = TRUE))

ggplot(quantity_data, 
       aes(x = `Ship Mode`, 
           y = Total_Quantity, 
           fill = Segment)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "Total Quantity by Ship Mode and Segment", 
       x = "Ship Mode", 
       y = "Total Quantity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Sales vs Profit with Discount
ggplot(data, 
       aes(x = Sales, 
           y = Profit, 
           color = Discount)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", 
                       high = "red") +
  labs(title = "Sales vs Profit with Discount Gradient", 
       x = "Sales", 
       y = "Profit", 
       color = "Discount") +
  theme_minimal()
