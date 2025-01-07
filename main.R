getwd()
data <- read.csv("flight_data_DEL_CCU.csv" )
head(data,2)
library(ggplot2)  # For plotting
library(dplyr)
data1 <- data %>%
  distinct()
data1$Price1 <- as.numeric(gsub(",", "", data1$Price))
data1$Duration1 <- sapply(data1$Duration, function(x) {
  
  
  #convert the duration into minutes
  hours <- as.numeric(sub("^(\\d+) h .*", "\\1", x))  
  minutes <- as.numeric(sub(".* (\\d+) m$", "\\1", x))
  
  
  return(hours * 60 + minutes)
})

ggplot(data1, aes(x = Duration1, y = Price1)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line
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

ggplot(data1, aes(y = Price1)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.colour = "red") +
  labs(title = "Boxplot of Price", y = "Price") +
  theme_minimal()
ggplot(data1, aes(y = Duration1)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.colour = "red") +
  labs(title = "Boxplot of Duration", y = "Duration") +
  theme_minimal()

ggplot(data1, aes(x = FlightCode, y = Price1, fill = FlightName)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Price by FlightCode and FlightName",
       x = "Flight Code", y = "Price") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
ggplot(data1, aes(x = FlightCode, y = Duration1, fill = FlightName)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Duration by FlightCode and FlightName",
       x = "Flight Code", y = "Duration") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

data1$Price <- as.numeric(data1$Price)
data1$Duration1 <- as.numeric(data1$Duration1)  # assuming you have Duration1

# Calculate the correlation matrix for numeric columns
correlation_matrix <- cor(data1[, c("Price1", "Duration1")], use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Optional: If you want to plot the correlation matrix
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", 
         title = "Correlation Matrix")

data2 <- data1[complete.cases(data1$Price1, data1$Duration1), ]
spearman_corr <- cor(data2$Price1, data2$Duration1, method = "spearman")
print(spearman_corr)
# Output the result
cat("Spearman's Correlation:", spearman_corr, "\n")

ggplot(data1, aes(x = Duration1, y = Price1)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Spearman's Correlation", x = "Duration", y = "Price")


pearson_corr <- cor(data2$Price1, data2$Duration1, method = "pearson")

# Print Pearson's Correlation
cat("Pearson's Correlation between Price and Duration1: ", round(pearson_corr, 2), "\n")


pearson_test <- cor.test(data2$Price1, data2$Duration1, method = "pearson")


cat("Pearson's Correlation Test Result:\n")
print(pearson_test)

kendall_corr <- cor(data2$Price1, data2$Duration1, method = "kendall")


print(kendall_corr)

pairwise_results <- pairwise.wilcox.test(data1$Price1, data1$FlightName, p.adjust.method = "fdr", exact = FALSE)
# Print the results
print(pairwise_results)

