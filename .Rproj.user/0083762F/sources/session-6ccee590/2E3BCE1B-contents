getwd()
data <- read.csv("flight_data_DEL_CCU.csv" )
head(data,2)
library(ggplot2)  # For plotting
library(dplyr)
data1 <- data %>%
  distinct()
data1$Price1 <- as.numeric(gsub(",", "", data1$Price))
data1$Duration1 <- sapply(data1$Duration, function(x) {
  # Extract hours and minutes using regular expressions
  hours <- as.numeric(sub("^(\\d+) h .*", "\\1", x))  # Extract hours
  minutes <- as.numeric(sub(".* (\\d+) m$", "\\1", x))  # Extract minutes
  
  # Convert to total minutes
  return(hours * 60 + minutes)
})

ggplot(data1, aes(x = Duration1, y = Price1)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line with confidence interval
  labs(
    title = "Correlation Between Duration and Price",
    x = "Duration",
    y = "Price"
  ) +
  theme_minimal()
# Create the histogram with a density overlay
ggplot(data1, aes(x = Price1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +  # Add density curve
  labs(
    title = "Histogram of Price with Density Curve",
    x = "Price",
    y = "Density"
  ) +
  theme_minimal()
# Create the histogram with a density overlay
ggplot(data1, aes(x = Duration1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +  # Add density curve
  labs(
    title = "Histogram of Price with Density Curve",
    x = "Density",
    y = "Price"
  ) +
  theme_minimal()

aggregated_data <- data1 %>%
  group_by(FlightName) %>%
  summarise(AveragePrice = mean(Price1, na.rm = TRUE))

head(aggregated_data)


aggregated_data$FlightName <- as.factor(aggregated_data$FlightName)
ggplot(aggregated_data, aes(x = FlightName, y = AveragePrice)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(
    title = "Average Price by Flight Name",
    x = "Flight Name",
    y = "Average Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

