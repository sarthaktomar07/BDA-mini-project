
# Load Required Libraries
library(data.table)  
library(ggplot2) 

# Load Data
df <- fread("C:/Users/Sarthak/Downloads/archive (4)/yellow_tripdata_2016-02.csv")  # Corrected file path

# Convert passenger_count to factor
df$passenger_count <- as.factor(df$passenger_count)

# Set seed for reproducibility
set.seed(123) 

# Sample 1% of the data
sample_df <- df[sample(.N, .N * 0.01)]  

# Histogram of Trip Distances
ggplot(sample_df, aes(x = trip_distance)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Trip Distances", x = "Trip Distance (miles)", y = "Frequency") +
  theme_minimal()

# Bar plot for Total Amount by Passenger Count
ggplot(sample_df, aes(x = passenger_count, y = total_amount, fill = passenger_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Amount Collected by Passenger Count", x = "Passenger Count", y = "Total Amount") +
  theme_minimal()

# Scatter plot for Trip Distance vs Fare Amount
ggplot(sample_df, aes(x = trip_distance, y = fare_amount)) +
  geom_point(color = "red", alpha = 0.6) +
  labs(title = "Trip Distance vs Fare Amount", x = "Trip Distance (miles)", y = "Fare Amount ($)") +
  theme_minimal()

# Convert payment_type to factor
sample_df$payment_type <- as.factor(sample_df$payment_type)

# Boxplot for Fare Amount by Payment Type
ggplot(sample_df, aes(x = payment_type, y = fare_amount, fill = payment_type)) +
  geom_boxplot() +
  labs(title = "Fare Amount by Payment Type", x = "Payment Type", y = "Fare Amount ($)") +
  theme_minimal()

# Bar plot for Frequency of Payment Types
ggplot(sample_df, aes(x = payment_type, fill = payment_type)) +
  geom_bar() +
  labs(title = "Frequency of Payment Types", x = "Payment Type", y = "Count") +
  theme_minimal()
