---
title: "Quantium analysis task 2 Tushar"
author: "Tushar_K"
date: "2026-06-24"
output: pdf_document
classoption: landscape
mainfont: Roboto
monofont: Consolas
latex_engine: MiKteX
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
```


# Experimentation & uplift testing (Quantium Task 2)


### This is the solution file for Task 2 on the lines of quantium vertual internship.




# Step 1: Understanding the problem statement

#### Task is to evaluate the stores' sales performace with a comparison among controlled vs trial group involving store 77, 86 and 88 and providing recommendation for these locations based on the result findings.





# Step 2: Preparing data

We have been given the data file in csv format named "QVI_data.csv". I am going to use R for this analysis.

Now moving the files in desired directory for entire R analysis and installing and loading required packages.

```
# setwd("C:/Users/Admin/Downloads")
```
```{r Required packages}
# Installing and loading required packages (I have masked them for rmd file)

# Installing and Loading required packages
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("data.table")
#install.packages("knitr")
#install.packages("devtools")
#devtools::install_github("r-lib/conflicted")


library(tidyverse)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table)
library(knitr)
library(conflicted)
library(lubridate)
library(dplyr)


# default to the dplyr version of filter
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

```


#### Load and Read the raw data file

```{r Loading files}
# csv file
file.path <- ("C:/Users/Admin/Downloads/") 
csv_data <- fread(paste0(file.path, "QVI_data.csv"))


# view dataset if it is loaded perfectly and what it contains
head(csv_data)

# {view(csv_data)} (in case i want to see the full picture)
```

#### Setting themes for plots
Giving plots a crisp white background and adjusting plot themes at middle of plot header

```{r setting cleaner background theme}
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
```


## Selecting control stores
Store 77, 86, 88 are trial stores.

I need to match trial stores to control stores that were similar in metrics before the trial period started in February 2019. The metrics that I considered for it are as follows:

1) Monthly sales revenue
2) Monthly customer footfall
3) Monthly transactions per customer

In order to select stores some precautions need to be kept in mind, these are:

1) The stores must be operational throughout the trial period for a quality study.
2) 


```{r adding required columns}
# adding monthID_y_m column
csv_data[, monthID_y_m := format(DATE, "%Y_%m")]

head(csv_data)
```


```{r}
library(dplyr)

store_monthly_metrics <- csv_data %>%
  group_by(STORE_NBR, monthID_y_m) %>%
  summarise(
    total_sales = sum(TOT_SALES, na.rm = TRUE),
    num_customers = n_distinct(LYLTY_CARD_NBR),
    txn_per_cust = n_distinct(TXN_ID) / num_customers,
    chips_per_cust = sum(PROD_QTY, na.rm = TRUE) / num_customers,
    avg_price_per_unit = total_sales / sum(PROD_QTY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(STORE_NBR, monthID_y_m)


head(store_monthly_metrics)
```

```{r}
library(dplyr)

# Filter to the pre-trial months (Before February 2019)
pre_trial_data <- store_monthly_metrics %>%
  filter(monthID_y_m < "2019_02")

# Identify and keep stores with exactly 7 months of data
pre_trial_stores_7month <- pre_trial_data %>%
  group_by(STORE_NBR) %>%
  filter(n() == 7) %>%
  ungroup()

head(pre_trial_stores_7month)
```

## Now I am ranking potential control stores' based on similarity to the trial stores

```{r ranking control stores with respective trial stores}
calculateCorrelation <- function(inputTable, metricCol, trial_store) {
  
  calcCorrTable <- data.frame(Store1 = integer(), 
                              Store2 = integer(), 
                              corr_measure = numeric())
  
 
  storeNumbers <- unique(inputTable$STORE_NBR)
  
  # Extracting the trial store's data and ensure it is ordered chronologically
  trial_data <- inputTable[inputTable$STORE_NBR == trial_store, ]
  trial_data <- trial_data[order(trial_data$monthID_y_m), ]
  trial_metric <- trial_data[[metricCol]] # Extract just the column we want to measure
  
  # Loop through every store to compare it against the trial store
  for (i in storeNumbers) {
    
    # Extract the potential control store's data and order it chronologically
    control_data <- inputTable[inputTable$STORE_NBR == i, ]
    control_data <- control_data[order(control_data$monthID_y_m), ]
    control_metric <- control_data[[metricCol]]
    
    # Calculate the Pearson correlation between the two stores
    # 'complete.obs' handles any NA values safely
    correlation_value <- cor(trial_metric, control_metric, use = "complete.obs")
    
    # Store the result in a temporary row, then attach it to our main results table
    temp_row <- data.frame(Store1 = trial_store, 
                           Store2 = i, 
                           corr_measure = correlation_value)
    
    calcCorrTable <- rbind(calcCorrTable, temp_row)
  }
  
  # Return the final table of correlations
  return(calcCorrTable)
}

```



## Checking correlations for the stores individually and Visually plotting for finding similarity in stores' patterns:

### Store number 77
```{r Top 5 controls against Store 77}
# Calculate correlation for total sales for Trial Store 77
corr_nSales <- calculateCorrelation(inputTable = pre_trial_stores_7month, 
                                    metricCol = "total_sales", 
                                    trial_store = 77)
# top 5 correlated stores
head(corr_nSales[order(-corr_nSales$corr_measure), ])

```
```{r Plot store 77 to most closest control store}
library(ggplot2)
library(dplyr)


# Plotting FOR STORE 77--------------------------------

# Define your stores (I can review the plots and their correlations by just changing the control_store number right below)
trial_store <- 77
control_store <- 233 #(I tried 71,233 , 119, 17, 3 as well but 233 showed consistent results)
# Filter the pre-trial dataset for just these two stores
plot_data <- pre_trial_stores_7month %>%
  filter(STORE_NBR %in% c(trial_store, control_store)) %>%
  # Create a clearer label for the legend
  mutate(Store_Type = ifelse(STORE_NBR == trial_store, 
                             paste("Trial Store", trial_store), 
                             paste("Control Store", control_store)))
# Generate the line chart
ggplot(plot_data, aes(x = monthID_y_m, y = total_sales, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Total Sales: Trial vs Control Store (Pre-Trial)",
       x = "Month",
       y = "Total Sales ($)",
       color = "Store") +
  # Expand the y-axis to start at 0 to prevent visual distortion of the gap
  scale_y_continuous(limits = c(0, NA))
```


### Store number 86
```{r Top 5 controls against Store 86}
# Calculate correlation for total sales for Trial Store 86
corr_nSales <- calculateCorrelation(inputTable = pre_trial_stores_7month, 
                                    metricCol = "total_sales", 
                                    trial_store = 86)
# top 5 correlated stores
head(corr_nSales[order(-corr_nSales$corr_measure), ])
```

```{r Plot store 86 to most closest control store}
library(ggplot2)
library(dplyr)

# Plotting fOR STORE 86--------------------------------

# Define your stores (I can review the plots and their correlations by just changing the control_store number right below)
trial_store <- 86
control_store <- 155 #(I tried 155, 132, 240,222,109 but 155 showed best results, although 222 and 109 were also close enough)
# Filter the pre-trial dataset for just these two stores
plot_data <- pre_trial_stores_7month %>%
  filter(STORE_NBR %in% c(trial_store, control_store)) %>%
  # Create a clearer label for the legend
  mutate(Store_Type = ifelse(STORE_NBR == trial_store, 
                             paste("Trial Store", trial_store), 
                             paste("Control Store", control_store)))
# Generate the line chart
ggplot(plot_data, aes(x = monthID_y_m, y = total_sales, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Total Sales: Trial vs Control Store (Pre-Trial)",
       x = "Month",
       y = "Total Sales ($)",
       color = "Store") +
  # Expand the y-axis to start at 0 to prevent visual distortion of the gap
  scale_y_continuous(limits = c(0, NA))

```




### Store number 88
```{r Top 5 controls against Store 88}
# Calculate correlation for total sales for Trial Store 88
corr_nSales <- calculateCorrelation(inputTable = pre_trial_stores_7month, 
                                    metricCol = "total_sales", 
                                    trial_store = 88)
# top 5 correlated stores
view(corr_nSales[order(-corr_nSales$corr_measure), ])
#for this store the top5 didn't show that much similarity so I added another metric.
```

 
```{r Plot store 88 to most closest control store}

# Plotting fOR STORE 88---------------------------
library(ggplot2)
library(dplyr)
# Define your stores (I can review the plots and their correlations by just changing the control_store number right below)
trial_store <- 88
control_store <- 237 #(I tried 159,237, 204, 134, 1, 253 and 237 showed best results)
# Filter the pre-trial dataset for just these two stores
plot_data <- pre_trial_stores_7month %>%
  filter(STORE_NBR %in% c(trial_store, control_store)) %>%
  # Create a clearer label for the legend
  mutate(Store_Type = ifelse(STORE_NBR == trial_store, 
                             paste("Trial Store", trial_store), 
                             paste("Control Store", control_store)))
# Generate the line chart
ggplot(plot_data, aes(x = monthID_y_m, y = total_sales, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Total Sales: Trial vs Control Store (Pre-Trial)",
       x = "Month",
       y = "Total Sales ($)",
       color = "Store") +
  # Expand the y-axis to start at 0 to prevent visual distortion of the gap
  scale_y_continuous(limits = c(0, NA))

```


The store with the 2nd highest score was usually the one which also performed best in plotting test and selected as the control store since it is most similar to the trial store.
# Findings for most similar control store for the trial stores are as follows:

## Most correlated store for Trial store 77 = 233

## Most correlated store for Trial store 86 = 155

## Most correlated store for Trial store 88 = 237



### Conducting visual checks on customer count trends by comparing the trial store to the respective control stores and other stores

## For store 77 vs 233
```{r visual check store 77}
library(ggplot2)
library(dplyr)
library(tidyr)

trial_store <- 77
control_store <- 233 

# Separating stores into categories and calculate monthly averages
plot_data_cust <- pre_trial_stores_7month %>%
  mutate(Store_Type = case_when(
    STORE_NBR == trial_store   ~ "Trial Store",
    STORE_NBR == control_store ~ "Control Store",
    TRUE                       ~ "Other Stores"
  )) %>%
  # Group by month and the new store type to aggregate the data
  group_by(monthID_y_m, Store_Type) %>%
  summarise(avg_customers = mean(num_customers, na.rm = TRUE), .groups = "drop")
# Create the trend line chart
ggplot(plot_data_cust, aes(x = monthID_y_m, y = avg_customers, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Customer Counts: Trial vs Control vs Other Stores (Pre-Trial)",
    x = "Month",
    y = "Number of Customers",
    color = "Store Group"
  ) +
  scale_y_continuous(limits = c(0, NA))
```


## For store 86 vs 155
```{r visual check store 86}
library(ggplot2)
library(dplyr)
library(tidyr)

trial_store <- 86
control_store <- 155 

# Separating stores into categories and calculate monthly averages
plot_data_cust <- pre_trial_stores_7month %>%
  mutate(Store_Type = case_when(
    STORE_NBR == trial_store   ~ "Trial Store",
    STORE_NBR == control_store ~ "Control Store",
    TRUE                       ~ "Other Stores"
  )) %>%
  # Group by month and the new store type to aggregate the data
  group_by(monthID_y_m, Store_Type) %>%
  summarise(avg_customers = mean(num_customers, na.rm = TRUE), .groups = "drop")
# Create the trend line chart
ggplot(plot_data_cust, aes(x = monthID_y_m, y = avg_customers, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Customer Counts: Trial vs Control vs Other Stores (Pre-Trial)",
    x = "Month",
    y = "Number of Customers",
    color = "Store Group"
  ) +
  scale_y_continuous(limits = c(0, NA))
```


## For store 88 vs 237
```{r visual check store 88}
library(ggplot2)
library(dplyr)
library(tidyr)

trial_store <- 88
control_store <- 237 

# Separating stores into categories and calculate monthly averages
plot_data_cust <- pre_trial_stores_7month %>%
  mutate(Store_Type = case_when(
    STORE_NBR == trial_store   ~ "Trial Store",
    STORE_NBR == control_store ~ "Control Store",
    TRUE                       ~ "Other Stores"
  )) %>%
  # Group by month and the new store type to aggregate the data
  group_by(monthID_y_m, Store_Type) %>%
  summarise(avg_customers = mean(num_customers, na.rm = TRUE), .groups = "drop")
# Create the trend line chart
ggplot(plot_data_cust, aes(x = monthID_y_m, y = avg_customers, color = Store_Type, group = Store_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Customer Counts: Trial vs Control vs Other Stores (Pre-Trial)",
    x = "Month",
    y = "Number of Customers",
    color = "Store Group"
  ) +
  scale_y_continuous(limits = c(0, NA))
```

# Assessment of trial From period beyong February 2019.
#### We'll start with scaling the control store's sales to a level similar to control for any differences between the two stores outside of the trial period.


```{r Assessment store 77}
library(dplyr)
# store 77:

trial_store <- 77
control_store <- 233

# Calculate the sum of pre-trial sales for the Trial Store
pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the sum of pre-trial sales for the Control Store
pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the Scaling Factor
scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control
# Print the factor to see how much we are adjusting by
print(paste("Sales Scaling Factor:", round(scaling_factor_sales, 4)))
# Apply the scaling factor to the Control Store's FULL dataset
scaled_control_data <- store_monthly_metrics %>%
  filter(STORE_NBR == control_store) %>%
  mutate(control_sales_scaled = total_sales * scaling_factor_sales)


head(scaled_control_data %>% select(monthID_y_m, total_sales, control_sales_scaled))
```

```{r Assessment store 86}
library(dplyr)
# store 86:

trial_store <- 86
control_store <- 155

# Calculate the sum of pre-trial sales for the Trial Store
pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the sum of pre-trial sales for the Control Store
pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the Scaling Factor
scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control
# Print the factor to see how much we are adjusting by
print(paste("Sales Scaling Factor:", round(scaling_factor_sales, 4)))
# Apply the scaling factor to the Control Store's FULL dataset
scaled_control_data <- store_monthly_metrics %>%
  filter(STORE_NBR == control_store) %>%
  mutate(control_sales_scaled = total_sales * scaling_factor_sales)


head(scaled_control_data %>% select(monthID_y_m, total_sales, control_sales_scaled))
```

```{r Assessment store 88}
library(dplyr)
# store 88:

trial_store <- 88
control_store <- 237

# Calculate the sum of pre-trial sales for the Trial Store
pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the sum of pre-trial sales for the Control Store
pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum()
# Calculate the Scaling Factor
scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control
# Print the factor to see how much we are adjusting by
print(paste("Sales Scaling Factor:", round(scaling_factor_sales, 4)))
# Apply the scaling factor to the Control Store's FULL dataset
scaled_control_data <- store_monthly_metrics %>%
  filter(STORE_NBR == control_store) %>%
  mutate(control_sales_scaled = total_sales * scaling_factor_sales)


head(scaled_control_data %>% select(monthID_y_m, total_sales, control_sales_scaled))
```

Now that we have comparable sales figures for the control store, we can calculate the percentage difference between the scaled control sales and the trial store's sales during the trial period.



### Store 77 vs 233 comparison:
```{r Store 77 vs 233 comparison}
library(dplyr)
library(tidyr)

trial_store <- 77
control_store <- 233

pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control



# TRIAL PERIOD EVALUATION 
# Filter the main dataset for the trial period (February 2019 onwards)
trial_period_data <- store_monthly_metrics %>%
  filter(monthID_y_m >= "2019_02" & monthID_y_m <= "2019_07") %>%
  filter(STORE_NBR %in% c(trial_store, control_store))

# Apply the pre-calculated scaling factor to the control store ONLY
scaled_trial_data <- trial_period_data %>%
  mutate(scaled_sales = ifelse(STORE_NBR == control_store, 
                               total_sales * scaling_factor_sales, 
                               total_sales))

# Pivot the data using dynamic column names
performance_eval <- scaled_trial_data %>%
  select(monthID_y_m, STORE_NBR, scaled_sales) %>%
  pivot_wider(names_from = STORE_NBR, 
              names_prefix = "Store_", 
              values_from = scaled_sales)

# Grab the dynamic column names based on your selected stores
trial_col <- paste0("Store_", trial_store)
control_col <- paste0("Store_", control_store)

# Add context, metadata, and clean formatting
performance_detailed <- performance_eval %>%
  mutate(
    # Explicitly add the store numbers back for context
    Trial_Store_NBR = trial_store,
    Control_Store_NBR = control_store,
    Trial_Sales = .[[trial_col]],
    Scaled_Control_Sales = .[[control_col]],
    
    # Calculate differences
    Sales_Gap_Value = Trial_Sales - Scaled_Control_Sales,
    Percentage_Difference = abs(Sales_Gap_Value) / Scaled_Control_Sales,
    
    # Add a plain-text conclusion for each month
    Trial_Result = ifelse(Sales_Gap_Value > 0, 
                          "Trial Outperformed", 
                          "Trial Underperformed")
  ) %>%
  # Select and reorder only the columns we want to see
  select(
    Trial_Month = monthID_y_m,
    Trial_Store_NBR,
    Control_Store_NBR,
    Trial_Sales,
    Scaled_Control_Sales,
    Sales_Gap_Value,
    Percentage_Difference,
    Trial_Result
  ) %>%
  # Format the numbers so they are easy to read
  mutate(
    Trial_Sales = round(Trial_Sales, 2),
    Scaled_Control_Sales = round(Scaled_Control_Sales, 2),
    Sales_Gap_Value = round(Sales_Gap_Value, 2),
    Percentage_Difference = paste0(round(Percentage_Difference * 100, 2), "%")
  )

# Convert to standard data.frame so the console prints all columns without hiding them
as.data.frame(performance_detailed)
```



### Store 86 vs 155 comparison:
```{r Store 86 vs 155 comparison}
library(dplyr)
library(tidyr)

trial_store <- 86
control_store <- 155

pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control



# TRIAL PERIOD EVALUATION 
# Filter the main dataset for the trial period (February 2019 onwards)
trial_period_data <- store_monthly_metrics %>%
  filter(monthID_y_m >= "2019_02" & monthID_y_m <= "2019_07") %>%
  filter(STORE_NBR %in% c(trial_store, control_store))

# Apply the pre-calculated scaling factor to the control store ONLY
scaled_trial_data <- trial_period_data %>%
  mutate(scaled_sales = ifelse(STORE_NBR == control_store, 
                               total_sales * scaling_factor_sales, 
                               total_sales))

# Pivot the data using dynamic column names
performance_eval <- scaled_trial_data %>%
  select(monthID_y_m, STORE_NBR, scaled_sales) %>%
  pivot_wider(names_from = STORE_NBR, 
              names_prefix = "Store_", 
              values_from = scaled_sales)

# Grab the dynamic column names based on your selected stores
trial_col <- paste0("Store_", trial_store)
control_col <- paste0("Store_", control_store)

# Add context, metadata, and clean formatting
performance_detailed <- performance_eval %>%
  mutate(
    # Explicitly add the store numbers back for context
    Trial_Store_NBR = trial_store,
    Control_Store_NBR = control_store,
    Trial_Sales = .[[trial_col]],
    Scaled_Control_Sales = .[[control_col]],
    
    # Calculate differences
    Sales_Gap_Value = Trial_Sales - Scaled_Control_Sales,
    Percentage_Difference = abs(Sales_Gap_Value) / Scaled_Control_Sales,
    
    # Add a plain-text conclusion for each month
    Trial_Result = ifelse(Sales_Gap_Value > 0, 
                          "Trial Outperformed", 
                          "Trial Underperformed")
  ) %>%
  # Select and reorder only the columns we want to see
  select(
    Trial_Month = monthID_y_m,
    Trial_Store_NBR,
    Control_Store_NBR,
    Trial_Sales,
    Scaled_Control_Sales,
    Sales_Gap_Value,
    Percentage_Difference,
    Trial_Result
  ) %>%
  # Format the numbers so they are easy to read
  mutate(
    Trial_Sales = round(Trial_Sales, 2),
    Scaled_Control_Sales = round(Scaled_Control_Sales, 2),
    Sales_Gap_Value = round(Sales_Gap_Value, 2),
    Percentage_Difference = paste0(round(Percentage_Difference * 100, 2), "%")
  )

# Convert to standard data.frame so the console prints all columns without hiding them
as.data.frame(performance_detailed)
```





### Store 88 vs 237 comparison:
```{r Store 88 vs 237 comparison}
library(dplyr)
library(tidyr)

trial_store <- 88
control_store <- 237

pre_trial_sales_trial <- pre_trial_stores_7month %>%
  filter(STORE_NBR == trial_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

pre_trial_sales_control <- pre_trial_stores_7month %>%
  filter(STORE_NBR == control_store) %>%
  pull(total_sales) %>%
  sum(na.rm = TRUE)

scaling_factor_sales <- pre_trial_sales_trial / pre_trial_sales_control



# TRIAL PERIOD EVALUATION 
# Filter the main dataset for the trial period (February 2019 onwards)
trial_period_data <- store_monthly_metrics %>%
  filter(monthID_y_m >= "2019_02" & monthID_y_m <= "2019_07") %>%
  filter(STORE_NBR %in% c(trial_store, control_store))

# Apply the pre-calculated scaling factor to the control store ONLY
scaled_trial_data <- trial_period_data %>%
  mutate(scaled_sales = ifelse(STORE_NBR == control_store, 
                               total_sales * scaling_factor_sales, 
                               total_sales))

# Pivot the data using dynamic column names
performance_eval <- scaled_trial_data %>%
  select(monthID_y_m, STORE_NBR, scaled_sales) %>%
  pivot_wider(names_from = STORE_NBR, 
              names_prefix = "Store_", 
              values_from = scaled_sales)

# Grab the dynamic column names based on your selected stores
trial_col <- paste0("Store_", trial_store)
control_col <- paste0("Store_", control_store)

# Add context, metadata, and clean formatting
performance_detailed <- performance_eval %>%
  mutate(
    # Explicitly add the store numbers back for context
    Trial_Store_NBR = trial_store,
    Control_Store_NBR = control_store,
    Trial_Sales = .[[trial_col]],
    Scaled_Control_Sales = .[[control_col]],
    
    # Calculate differences
    Sales_Gap_Value = Trial_Sales - Scaled_Control_Sales,
    Percentage_Difference = abs(Sales_Gap_Value) / Scaled_Control_Sales,
    
    # Add a plain-text conclusion for each month
    Trial_Result = ifelse(Sales_Gap_Value > 0, 
                          "Trial Outperformed", 
                          "Trial Underperformed")
  ) %>%
  # Select and reorder only the columns we want to see
  select(
    Trial_Month = monthID_y_m,
    Trial_Store_NBR,
    Control_Store_NBR,
    Trial_Sales,
    Scaled_Control_Sales,
    Sales_Gap_Value,
    Percentage_Difference,
    Trial_Result
  ) %>%
  # Format the numbers so they are easy to read
  mutate(
    Trial_Sales = round(Trial_Sales, 2),
    Scaled_Control_Sales = round(Scaled_Control_Sales, 2),
    Sales_Gap_Value = round(Sales_Gap_Value, 2),
    Percentage_Difference = paste0(round(Percentage_Difference * 100, 2), "%")
  )

# Convert to standard data.frame so the console prints all columns without hiding them
as.data.frame(performance_detailed)
```



# Conclusion

## I have concluded that the following are the most suitable control stores against following trial stores:
## Stores 77 -> 233
## Stores 86 -> 155
## Stores 88 -> 237

The results for trial stores 77 and 88 during the trial period, show significant difference in at least 2 of the 3 trial months but this is not the case for trial store 86. We can check with the client if the implementation of the trial was different in trial store 86, but overall, the trial shows a significant increase in sales.

# I have attached my final presentation for the Category Manager in following slides below :-


-------------------------------------------------------------------------------------------------------------------------
# PRESENTATION
-------------------------------------------------------------------------------------------------------------------------


