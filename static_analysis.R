library(tidyverse)

data <- read_csv("user_behavior_dataset.csv")

str(data)

data <- data |>
  mutate_if(is.character, as.factor)

data$`User Behavior Class` <- as.factor(data$`User Behavior Class`)

summary(data)
str(data)

# Function to summarize numeric variables by levels of a categorical variable
summarize_by_category <- function(data, category_var) {

  if (!is.factor(data[[category_var]])) {
    stop(paste(category_var, "must be a factor."))
  }

  numeric_vars <- data |>
    select(where(is.numeric), -`User ID`) |>
    colnames()
  
  summary_data <- data |>
    group_by(across(all_of(category_var))) |>
    summarise(across(all_of(numeric_vars), list(
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
      Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
    ), .names = "{col}_{fn}"))
  
  return(summary_data)
}

summarize_by_category(data, "Gender")
