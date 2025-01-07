# Load necessary libraries
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
library(dplyr)  # For data manipulation

# Load the data
data <- read.csv("flight_data_DEL_CCU.csv")

# Clean the data: remove duplicates and convert Price to numeric
data1 <- data %>% distinct()
data1$Price1 <- as.numeric(gsub(",", "", data1$Price))

# Make sure FlightName is treated as a factor (categorical variable
data1$FlightName <- factor(data1$FlightName)
#
#histogram density function with smooth bell curve
plot(data1$Price1 ~ data1$FlightName,
     main = "Flight between New Delhi and Kolkata: Price vs Airlines",
     xlab = "Airlines",
     ylab = "Price",
     pch = 19,         # Plotting points as solid circles
     col = "cyan",     # Color of points
     frame = TRUE,     # Add border around plot
     xaxt = "n")       # Turn off x-axis 

axis(1, at = 1:length(levels(data1$FlightName)), labels = levels(data1$FlightName), las = 2)
ggplot(data1, aes(x = FlightName, y = Price1)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Flight between New Delhi and Kolkata: Price vs Airlines",
       x = "Airlines", y = "Price in INR") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels




data1$Duration1 <- sapply(data1$Duration, function(x) {
  
  
  #convert the duration into minutes
  hours <- as.numeric(sub("^(\\d+) h .*", "\\1", x))  
  minutes <- as.numeric(sub(".* (\\d+) m$", "\\1", x))
  
  
  return(hours * 60 + minutes)
})


# Add lines for the mean and standard deviations
abline(v = mean_val, col = "red", lwd = 2)  # Mean
abline(v = mean_val + sd_val, col = "green", lwd = 2, lty = 2)  # +1 SD
abline(v = mean_val - sd_val, col = "green", lwd = 2, lty = 2)  # -1 SD
abline(v = mean_val + 2*sd_val, col = "blue", lwd = 2, lty = 2)  # +2 SD
abline(v = mean_val - 2*sd_val, col = "blue", lwd = 2, lty = 2)  # -2 SD
# Boxplot for standard deviations
boxplot(data1$Price1, 
        main = "Boxplot Showing Spread of Data", 
        ylab = "Price", 
        col = "lightblue")

# Lines for mean ± SD on boxplot
abline(h = mean_val, col = "red", lwd = 2)  # Mean line
abline(h = mean_val + sd_val, col = "green", lwd = 2, lty = 2)  # Mean +1 SD
abline(h = mean_val - sd_val, col = "green", lwd = 2, lty = 2)  # Mean -1 SD
# Add legend for SD lines
legend("topright", 
       legend = c("Mean", "+1 SD", "-1 SD", "+2 SD", "-2 SD"), 
       col = c("red", "green", "green", "blue", "blue"), 
       lty = c(1, 2, 2, 2, 2), lwd = 2)

# Print actual mean and SD
cat("Mean:", mean_val, "\n")
cat("Standard Deviation:", sd_val , "\n")
data1$Duration1 <- as.numeric(data1$Duration1)  # assuming you have Duration1
# Calculate the correlation matrix for numeric columns
correlation_matrix <- cor(data1[, c("Price1", "Duration1")], use = "complete.obs")
# Print the correlation matrix
#print(correlation_matrix)
# Perform pairwise Wilcoxon test with asymptotic p-values (no exact computation due to ties)
pairwise_results <- pairwise.wilcox.test(data1$Price1, data1$FlightName, p.adjust.method = "none", exact = FALSE)
# Print the results
print(pairwise_results)

