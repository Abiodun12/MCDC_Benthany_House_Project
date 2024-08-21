setwd("/Users/someoddsense/Desktop/MCDC")
# Read the CSV file
mydata <- read.csv("~/Desktop/MCDC/BHR(R.4).csv", na.strings = c("n/a", "NA", "NULL", ""))

# Check the structure of the data
str(mydata)


#mydata <- mydata[-c(117:1001), ]

names(mydata)
class(mydata)
mydata
class(mydata$Age)

mydata[, 'Age.at.move.in']<- as.numeric(mydata[,'Age.at.move.in'])
mydata[, 'Age']<- as.numeric(mydata[,'Age'])
mydata[, 'Length.of.Stay....of.days.']<- as.numeric(mydata[,'Length.of.Stay....of.days.'])
mydata[, 'Children']<- as.numeric(mydata[,'Children'])

#date data
mydata$DOB <- as.Date(mydata$DOB, format = "%m/%d/%y")
mydata$Move.in.date <- as.Date(mydata$Move.in.date, format = "%m/%d/%y")
mydata$Move.out.date <- as.Date(mydata$Move.out.date, format = "%m/%d/%y")

#Fixing the missing length of stay dates:

# Step 3: Identify the cell to edit
# For example, to edit the cell in the 3rd row and the 2nd column
# Replace with the appropriate row and column names
row_to_edit <- 54
column_to_edit <- "Length.of.Stay....of.days." 
# Step 4: Edit the cell
mydata[row_to_edit, column_to_edit] <- 1341  # Replace "New Value" with the desired value

row_to_edit <- 55
column_to_edit <- "Length.of.Stay....of.days." 
# Step 4: Edit the cell
mydata[row_to_edit, column_to_edit] <- 1333  # Replace "New Value" with the desired value

row_to_edit <- 101
column_to_edit <- "Length.of.Stay....of.days." 
# Step 4: Edit the cell
mydata[row_to_edit, column_to_edit] <- 1116  # Replace "New Value" with the desired value

# Step 5: (Optional) Save the modified data back to a CSV
write.csv(mydata, "~/Desktop/MCDC/BHR(R.2).csv", row.names = FALSE)


Age <- mydata$Age
Children <- mydata$Children

#Chat gpt code suggestion:

# Check names and classes
names(mydata)
class(mydata)

# Convert relevant columns to numeric
mydata$Age.at.move.in <- as.numeric(mydata$Age.at.move.in)
mydata$Age <- as.numeric(mydata$Age)
mydata$Length.of.Stay....of.days. <- as.numeric(mydata$Length.of.Stay....of.days.)
mydata$Children <- as.numeric(mydata$Children)

# Check for NAs after conversion
sum(is.na(mydata))

# Calculate correlations between numeric columns
correlations <- cor(mydata[, c("Age", "Children")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test <- cor.test(mydata$Age, mydata$Children, method = "pearson")
kendall_test <- cor.test(mydata$Age, mydata$Children, method = "kendall")
spearman_test <- cor.test(mydata$Age, mydata$Children, method = "spearman")

# Print results
print(correlations)
print(pearson_test)
print(kendall_test)
print(spearman_test)

#visualizing code
library(ggplot2)

a <- ggplot(mydata, aes(x = Age, y = Children)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Age vs. Children",
       x = "Age",
       y = "Children")
print(a)

#ggsave("(A) age vs children.jpg", plot = a, width = 5, height = 5, units = "in", dpi = 300, quality = 95)

# correlation between age at move in and number of children
# Calculate correlations between numeric columns
correlations2 <- cor(mydata[, c("Age.at.move.in", "Children")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test2 <- cor.test(mydata$Age.at.move.in, mydata$Children, method = "pearson")
kendall_test2 <- cor.test(mydata$Age.at.move.in, mydata$Children, method = "kendall")
spearman_test2 <- cor.test(mydata$Age.at.move.in, mydata$Children, method = "spearman")

# Print results
print(correlations2)
print(pearson_test2)
print(kendall_test2)
print(spearman_test)

#visualizing code
library(ggplot2)

b <- ggplot(mydata, aes(x = Age.at.move.in, y = Children)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Age at Move in vs. Children",
       x = "Age at Move in",
       y = "Children")
print(b)
#ggsave("(B) age at move in vs children.jpg", plot = b, width = 5, height = 5, units = "in", dpi = 300, quality = 95)

# repeating correlation between age and length of stay
# Calculate correlations between Age and Length.of.Stay....of.days.
correlations3 <- cor(mydata[, c("Age", "Length.of.Stay....of.days.")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test3 <- cor.test(mydata$Age, mydata$Length.of.Stay....of.days., method = "pearson")
kendall_test3 <- cor.test(mydata$Age, mydata$Length.of.Stay....of.days., method = "kendall")
spearman_test3 <- cor.test(mydata$Age, mydata$Length.of.Stay....of.days., method = "spearman")

# Print results
print(correlations3)
print(pearson_test3)
print(kendall_test3)
print(spearman_test3)

# checking age at move in and number of children
# Calculate correlations between Age at move in and Length.of.Stay....of.days.
correlations4 <- cor(mydata[, c("Age.at.move.in", "Length.of.Stay....of.days.")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test4 <- cor.test(mydata$Age.at.move.in, mydata$Length.of.Stay....of.days., method = "pearson")
kendall_test4 <- cor.test(mydata$Age.at.move.in, mydata$Length.of.Stay....of.days., method = "kendall")
spearman_test4 <- cor.test(mydata$Age.at.move.in, mydata$Length.of.Stay....of.days., method = "spearman")

# Print results
print(correlations4)
print(pearson_test4)
print(kendall_test4)
print(spearman_test4)

#check again between children and length of stay

# Calculate correlations between Children and Length.of.Stay....of.days.
correlations5 <- cor(mydata[, c("Children", "Length.of.Stay....of.days.")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test5 <- cor.test(mydata$Children, mydata$Length.of.Stay....of.days., method = "pearson")
kendall_test5 <- cor.test(mydata$Children, mydata$Length.of.Stay....of.days., method = "kendall")
spearman_test5 <- cor.test(mydata$Children, mydata$Length.of.Stay....of.days., method = "spearman")

# Print results
print(correlations5)
print(pearson_test5)
print(kendall_test5)
print(spearman_test5)

# checking countries and length of stay

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming 'mydata' is already loaded and contains COO and Length.of.Stay....of.days.

# Step 1: Group by Country and Calculate Average Length of Stay
average_stay <- mydata %>%
  group_by(COO) %>%
  summarise(Average_Length_of_Stay = mean(Length.of.Stay....of.days., na.rm = TRUE))

# View the results
print(average_stay)

# Step 2: Visualize the Average Length of Stay
c <- ggplot(average_stay, aes(x = reorder(COO, Average_Length_of_Stay), y = Average_Length_of_Stay)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Length of Stay by COO",
       x = "Country of Origin",
       y = "Average Length of Stay (days)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = .25),  # Increase title size and center it
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14)   # Adjust y-axis title size
  )
print(c)
#ggsave("(C) average length of stay by COO.jpg", plot = c, width = 8, height = 8, units = "in", dpi = 300, quality = 95)

# Step 3: (Optional) Statistical Testing
# Example: ANOVA to see if there are significant differences in stay length
anova_result <- aov(Length.of.Stay....of.days. ~ COO, data = mydata)
summary(anova_result)

#Tukey's HSD
# Assuming you have your ANOVA model already created, e.g.:
# model <- aov(Length.of.Stay....of.days. ~ COO, data = mydata)

# Run the ANOVA
model <- aov(Length.of.Stay....of.days. ~ COO, data = mydata)

# Perform Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(model)

# Print the results
print(tukey_results)


# Calculate the average length of stay
average_length_of_stay <- mean(mydata$Length.of.Stay....of.days., na.rm = TRUE)

# Print the result
print(average_length_of_stay)

# Load necessary library
library(ggplot2)

# Calculate the mean length of stay
mean_length_of_stay <- mean(mydata$Length.of.Stay....of.days., na.rm = TRUE)

# Create a density plot with a mean line
d <- ggplot(mydata, aes(x = Length.of.Stay....of.days.)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_length_of_stay), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density Plot of Length of Stay", 
       x = "Length of Stay (days)", 
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size and center it
        axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14),  # Adjust x-axis title size
        axis.title.y = element_text(size = 14))  # Adjust y-axis title size 

# Extract data from the plot
ggplot_build(d)$data[[1]] -> plot_data

# Find a suitable y position for the label
max_y <- max(plot_data$density)

# Add the label using annotate
d <- d + 
  annotate("text", x = mean_length_of_stay, y = max_y * 0.8,  # Adjust the y position based on the maximum y value
           label = paste("Mean =", round(mean_length_of_stay, 2)), 
           color = "black", vjust = -1.5, hjust = -.15)  # Adjust position for clarity

print(d)

# Save the plot with the adjusted dimensions
#ggsave("(D) density plot length of stay.jpg", plot = d, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


# histogram with bins 
average_age <- mean(mydata$Age.at.move.in, na.rm = TRUE)

# Create the histogram with fewer bins
e <- ggplot(mydata, aes(x = Age.at.move.in)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +  # Increase binwidth for fewer bins
  geom_vline(aes(xintercept = average_age), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Age at Move-in",
       x = "Age at Move-in",
       y = "Count") +
  annotate("text", x = average_age + 5, y = Inf,  # Adjust x position to move text to the right
           label = paste("Mean =", round(average_age, 2)), 
           color = "red", vjust = 2, hjust = 0.15) +  # Adjust hjust to align text to the left
  theme_minimal() +
  theme(plot.title = element_text(size = 16))  # Adjust the title size
print(e)

#ggsave("(E) Histogram of age at move in.jpg", plot = e, width = 7, height = 7, units = "in", dpi = 300, quality = 95)

# Plot density plot with mean line
f <- ggplot() +
  geom_density(data = mydata, aes(x = Age.at.move.in), fill = "blue", alpha = 0.5) +
  labs(x = "Age at Move-in", y = "Density", title = "Density Plot of Age at Move-in") +
  theme_minimal() +
  # Add mean line with annotation
  geom_vline(xintercept = average_age, linetype = "dashed", size = 1, color = "red") +
  annotate("text", x = average_age, y = 0.25, label = paste("Mean Age:", round(average_age, 2)),
           color = "red", size = 3, hjust = -0.1, vjust = 3.5) +
  theme(plot.title = element_text(size = 20))  # Adjust the title size
print(f)
#ggsave("(F) Density plot of age at move in.jpg", plot = f, width = 7, height = 7, units = "in", dpi = 300, quality = 95)

# distribution of move in dates by month
library(lubridate)
mydata$Month <- month(mydata$Move.in.date, label = TRUE)

# Count occurrences of each month
library(lubridate)
# Convert MoveInDate to Date class (if not already)
mydata$Move.in.date <- as.Date(mydata$Move.in.date)

# Extract month from MoveInDate
mydata$Month <- format(mydata$Move.in.date, "%b")  # Abbreviated month name, e.g., Jan, Feb, etc.

# Count occurrences of each month
month_counts <- table(mydata$Month)

# Create a data frame for plotting
month_counts_df <- data.frame(Month = names(month_counts), Frequency = as.numeric(month_counts))
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Convert Month to ordered factor with defined levels
month_counts_df$Month <- factor(month_counts_df$Month, levels = month_order)

# Plotting with ggplot2
g <- ggplot(month_counts_df, aes(x = Month, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Distribution of Move-In Dates by Month",
       x = "Month", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))  # Adjust the title size
print(g)
#ggsave("(G) Distribution of move in dates by month.jpg", plot = g, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


# 7/24
# creating new csv for average length of stay by countries map

# Step 1: Calculate average length of stay by country
avg_stay <- aggregate(Length.of.Stay....of.days. ~ COO, data = mydata, FUN = mean)

# Step 2: Add a new rounded field to the data frame
avg_stay$Average_Length_Rounded <- round(avg_stay$Length.of.Stay....of.days.)

# Step 3: Write results to a new CSV file
#write.csv(avg_stay, file = "average_stay_by_country.csv", row.names = FALSE)


#7/25

mydata2 <- read.csv("~/Desktop/MCDC/BHR(R.4).csv", na.strings = c("n/a", "NA", "NULL", ""))

mydata2[, 'Have.Kid']<- as.numeric(mydata2[,'Have.Kid'])

# Calculate correlations between numeric columns
correlations6 <- cor(mydata2[, c("Length.of.Stay....of.days.", "Have.Kid")], use = "pairwise.complete.obs")

# Perform correlation tests
pearson_test6 <- cor.test(mydata2$Length.of.Stay....of.days., mydata2$Have.Kid, method = "pearson")
kendall_test6 <- cor.test(mydata2$Length.of.Stay....of.days., mydata2$Have.Kid, method = "kendall")
spearman_test6 <- cor.test(mydata2$Length.of.Stay....of.days., mydata2$Have.Kid, method = "spearman")

# Print results
print(correlations6)
print(pearson_test6)
print(kendall_test6)
print(spearman_test6)


# total births per year from bdays
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Convert Child.Bday, Child.2, and Child.3 to Date format
mydata2$Child.Bday <- as.Date(mydata2$Child.Bday, format = "%m/%d/%y")
mydata2$Child.2 <- as.Date(mydata2$Child.2, format = "%m/%d/%y")
mydata2$Child.3 <- as.Date(mydata2$Child.3, format = "%m/%d/%y")

# Reshape data to long format
long_data <- mydata2 %>%
  pivot_longer(cols = c(Child.Bday, Child.2, Child.3), 
               names_to = "Type", 
               values_to = "Date") %>%
  filter(!is.na(Date)) %>%  # Remove rows with NA dates
  mutate(Year = as.numeric(format(Date, "%Y")))

# Count the number of children born each year
yearly_counts <- long_data %>%
  count(Year)

# Print the combined data frame to check the counts
print(yearly_counts)

# Plot the data using ggplot2
h <- ggplot(yearly_counts, aes(x = Year, y = n)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Total Number of Children Born Each Year",
       x = "Year",
       y = "Number of Children") +
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, hjust = 0.4))  # Adjust the size parameter here

print(h)
#ggsave("(H) Total Number of Children Born Each Year.jpg", plot = h, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


#7/30
#pie chart of Have.Kid
library(grid)  # For annotation_custom
# Summarize the data
kid_summary <- mydata2 %>%
  count(Have.Kid)

# Create labels for the pie chart
labels <- c("No", "Yes")
kid_summary$Label <- ifelse(kid_summary$Have.Kid == 1, "Yes", "No")

# Create pie chart
kid_summary$Label <- ifelse(kid_summary$Have.Kid == 1, "Yes", "No")

# Create pie chart with outline, custom colors, and counts
i <- ggplot(kid_summary, aes(x = "", y = n, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add color = "black" for outline
  coord_polar(theta = "y") +
  labs(title = "Do Residents Have Children?",
       x = NULL,
       y = NULL) +
  theme_void() +  # Remove unnecessary elements for pie chart
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 25, hjust = 0.5),
    plot.margin = unit(c(1, 1, 2, 1), "cm")  # Add margin space around the plot
  ) +
  scale_fill_manual(values = c("No" = "purple", "Yes" = "lightgreen")) +  # Customize the colors
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "black", size = 8)  # Add counts to the pie chart

print(i)
# Save the plot with adjusted dimensions
#ggsave("(i)Do_Residents_Have_Children.jpg", plot = i, width = 8, height = 8, units = "in", dpi = 300, quality = 95)


#bar chart of children born before during or after
combined_data <- mydata2 %>%
  pivot_longer(cols = starts_with("Before.After"), 
               names_to = "Source", 
               values_to = "Value") %>%
  filter(!is.na(Value)) %>%  # Remove NA values
  mutate(Value = recode(Value, `2` = "After", `1` = "During", `0` = "Before"))  # Recode values

# Summarize the data to get counts of each unique value
value_summary <- combined_data %>%
  count(Value) %>%
  mutate(Value = factor(Value, levels = c("Before", "During", "After")))  # Reverse the order of categories

# Create a bar chart showing the counts of different values with outlines and text labels
j <- ggplot(value_summary, aes(x = Value, y = n, fill = Value)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.5, color = "black") +
  labs(title = "Children Born Before, During, or After Resident Stay at BHH",
       x = " ",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5, size = 20),  # Adjust title size
        legend.text = element_text(size = 12),  # Adjust legend text size
        legend.title = element_text(size = 14),  # Adjust legend title size
        legend.position = "none") +
  scale_fill_manual(values = c("Before" = "lightblue", "During" = "blue", "After" = "darkblue"))
print(j)
# Save the plot
#ggsave("(j)Before.During.After.jpg", plot = j, width = 8, height = 6, units = "in", dpi = 300, quality = 95)

# move in and move outs by year
class(mydata2$Move.in.date)

mydata2$Move.in.date <- as.Date(mydata$Move.in.date, format = "%m/%d/%y")
mydata2$Move.out.date <- as.Date(mydata$Move.out.date, format = "%m/%d/%y")

# Extract the year from the dates
mydata2$MoveInYear <- format(mydata2$Move.in.date, "%Y")
mydata2$MoveOutYear <- format(mydata2$Move.out.date, "%Y")

# Count occurrences of each year for move-in dates
move_in_counts <- mydata2 %>%
  filter(!is.na(MoveInYear)) %>%
  count(MoveInYear)

# Count occurrences of each year for move-out dates
move_out_counts <- mydata2 %>%
  filter(!is.na(MoveOutYear)) %>%
  count(MoveOutYear)

# Rename columns for clarity
colnames(move_in_counts) <- c("Year", "MoveInCount")
colnames(move_out_counts) <- c("Year", "MoveOutCount")

# Merge the two data frames by Year
yearly_counts <- full_join(move_in_counts, move_out_counts, by = "Year")

# Replace NA values with 0 for plotting purposes
yearly_counts[is.na(yearly_counts)] <- 0

# Convert Year to numeric for proper ordering in the plot
yearly_counts$Year <- as.numeric(yearly_counts$Year)

# Plotting with ggplot2
k <- ggplot(yearly_counts, aes(x = Year)) +
  geom_line(aes(y = MoveInCount, color = "Move In")) +
  geom_line(aes(y = MoveOutCount, color = "Move Out")) +
  geom_point(aes(y = MoveInCount, color = "Move In")) +
  geom_point(aes(y = MoveOutCount, color = "Move Out")) +
  labs(title = "Number of Move-In and Move-Out Dates Each Year",
       x = "Year", 
       y = "Count") +
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("Move In" = "blue", "Move Out" = "red"),
                     name = "Legend")

# Print the plot
print(k)

# Save the plot
#ggsave("(K)move_in_out_dates_by_year.jpg", plot = k, width = 8, height = 6, units = "in", dpi = 300, quality = 95)

# 8/8
# create csv file with this info
# Define the range of years
start_date <- as.Date("2017-01-01")
end_date <- as.Date("2024-7-31")

# Create a sequence of months between start_date and end_date
months_seq <- seq(start_date, end_date, by = "month")

# Calculate the number of residents living in the house each month
residents_per_month <- sapply(months_seq, function(month) {
  sum(mydata2$Move.in.date <= month & mydata2$Move.out.date >= ceiling_date(month, "month") - days(1))
})

# Create a data frame for the results
residents_df <- data.frame(
  Month = months_seq,
  Residents = residents_per_month
)

# Remove months beyond the current date (optional)
residents_df <- residents_df %>% filter(Month <= Sys.Date())

# Export the data frame to a CSV file
#write.csv(residents_df, "ResidentMonthOccupancy.csv", row.names = FALSE)

#for loop for each month each year
library(dplyr)
library(lubridate)
library(ggplot2)

# Ensure Move.in.date and Move.out.date are in Date format
mydata2$Move.in.date <- as.Date(mydata2$Move.in.date, format = "%Y-%m-%d")
mydata2$Move.out.date <- as.Date(mydata2$Move.out.date, format = "%Y-%m-%d")

unique(mydata2$second.move.in)
# Convert the dates using the MM/DD/YY format
mydata2$second.move.in <- as.Date(mydata2$second.move.in, format = "%m/%d/%y")
mydata2$second.move.out <- as.Date(mydata2$second.move.out, format = "%m/%d/%y")

mydata2$second.move.in <- as.Date(mydata2$second.move.in, format = "%Y-%m-%d")
mydata2$second.move.out <- as.Date(mydata2$second.move.out, format = "%Y-%m-%d")

# Identify the years to iterate over
years <- 2017:2024

# Loop over each year
for (year in years) {
  # Create a sequence of months for the current year
  months_seq <- seq(as.Date(paste0(year, "-01-01")), 
                    as.Date(if (year == 2024) "2024-07-01" else paste0(year, "-12-01")), 
                    by = "month")
 
  # Calculate the number of residents living in the house each month
  residents_per_month <- sapply(months_seq, function(month) {
    residents <- sum(mydata2$Move.in.date <= month & 
                       (is.na(mydata2$Move.out.date) | mydata2$Move.out.date >= ceiling_date(month, "month") - days(1)), na.rm = TRUE)
    if (!is.na(residents) && residents > 15) residents <- 15
    return(residents)
  })
  
  # Create a data frame for plotting
  residents_df <- data.frame(
    Month = months_seq,
    Residents = residents_per_month
  )
  
  # Plot the number of residents per month
  L <- ggplot(residents_df, aes(x = Month, y = Residents)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title = paste("Number of Residents Living in BHH Monthly in", year),
         x = "Month",
         y = "Number of Residents") +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  print(L)
} 
  # Save the plot as a JPEG file
  #ggsave(paste0("(L)MonthlyOccupants_", year, ".jpg"), plot = L, width = 8, height = 6, units = "in", dpi = 300, quality = 95)


# 8/13
# ONE LONG CHART FOR MOVE IN AND MOVE OUT

library(ggplot2)
library(lubridate)

# Identify the years to iterate over
years <- 2017:2024

# Create an empty data frame to store combined data
all_residents_df <- data.frame()

# Loop over each year
for (year in years) {
  # Create a sequence of months for the current year
  months_seq <- seq(as.Date(paste0(year, "-01-01")), 
                    as.Date(if (year == 2024) "2024-07-01" else paste0(year, "-12-01")), 
                    by = "month")
  
  # Calculate the number of residents living in the house each month
  residents_per_month <- sapply(months_seq, function(month) {
    residents <- sum(mydata2$Move.in.date <= month & 
                       (is.na(mydata2$Move.out.date) | mydata2$Move.out.date >= ceiling_date(month, "month") - days(1)), na.rm = TRUE)
    if (!is.na(residents) && residents > 15) residents <- 15
    return(residents)
  })
  
  # Create a data frame for the current year
  residents_df <- data.frame(
    Month = months_seq,
    Residents = residents_per_month,
    Year = year
  )
  
  # Combine the data for the current year with the overall data frame
  all_residents_df <- rbind(all_residents_df, residents_df)
}

# Plot the number of residents per month across all years
# Plot the number of residents per month across all years
m <- ggplot(all_residents_df, aes(x = Month, y = Residents, color = as.factor(Year))) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Residents Living in BHH Monthly by Year",
       x = "Month",
       y = "Number of Residents",
       color = "Year") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Show only month abbreviations
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Smaller text size
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

print(m)
#ggsave("(m)ResidentsPerMonthByYear.jpg", plot = m, width = 14, height = 6, units = "in", dpi = 300, quality = 95)

# Export the data to a CSV file
#write.csv(all_residents_df, "OccupancyMonthlyByYear.csv", row.names = FALSE)

# NOW DO LONG GRAPH WITH CHILDREN
mydata2 <- read.csv("~/Desktop/MCDC/BHR(R.4).csv", na.strings = c("n/a", "NA", "NULL", ""))

print(head(mydata2$Move.in.date))
# Convert the date columns to Date format with the correct format
mydata2$Move.in.date <- as.Date(mydata2$Move.in.date, format = "%m/%d/%y")
mydata2$Move.out.date <- as.Date(mydata2$Move.out.date, format = "%m/%d/%y")
mydata2$Child.Start <- as.Date(mydata2$Child.Start, format = "%m/%d/%y")
mydata2$Child.Leave <- as.Date(mydata2$Child.Leave, format = "%m/%d/%y")
mydata2$Child2.Start <- as.Date(mydata2$Child2.Start, format = "%m/%d/%y")
mydata2$Child2.Leave <- as.Date(mydata2$Child2.Leave, format = "%m/%d/%y")
mydata2$Child3.Start <- as.Date(mydata2$Child3.Start, format = "%m/%d/%y")
mydata2$Child3.Leave <- as.Date(mydata2$Child3.Leave, format = "%m/%d/%y")

# Combine residents and children data into a single data frame with a Category column
combined_df <- data.frame()

years <- 2017:2024

for (year in years) {
  # Create a sequence of months for the current year
  months_seq <- seq(as.Date(paste0(year, "-01-01")),
                    as.Date(if (year == 2024) "2024-07-01" else paste0(year, "-12-01")),
                    by = "month")
  
  # Calculate the number of residents living in the house each month
  residents_per_month <- sapply(months_seq, function(month) {
    residents <- sum(mydata2$Move.in.date <= month & 
                       (is.na(mydata2$Move.out.date) | mydata2$Move.out.date >= ceiling_date(month, "month") - days(1)), na.rm = TRUE)
    if (!is.na(residents) && residents > 15) residents <- 15
    return(residents)
  })
  
  # Calculate the number of children living in the house each month
  children_per_month <- sapply(months_seq, function(month) {
    sum((mydata2$Child.Start <= month & 
           (is.na(mydata2$Child.Leave) | mydata2$Child.Leave >= ceiling_date(month, "month") - days(1))) |
          (mydata2$Child2.Start <= month & 
             (is.na(mydata2$Child2.Leave) | mydata2$Child2.Leave >= ceiling_date(month, "month") - days(1))) |
          (mydata2$Child3.Start <= month & 
             (is.na(mydata2$Child3.Leave) | mydata2$Child3.Leave >= ceiling_date(month, "month") - days(1))), na.rm = TRUE)
  })
  
  # Append the data to the combined data frame
  combined_df <- rbind(combined_df,
                       data.frame(Month = months_seq, Count = residents_per_month, Year = year, Category = "Residents"),
                       data.frame(Month = months_seq, Count = children_per_month, Year = year, Category = "Children"))
}

# Define colors and line types for categories
category_colors <- c("Residents" = "blue", "Children" = "red")
category_linetypes <- c("Residents" = "solid", "Children" = "dashed")
category_shapes <- c("Residents" = 16, "Children" = 17)

# Plot the data
n <- ggplot(combined_df, aes(x = Month, y = Count, group = interaction(Category, Year))) +
  geom_line(aes(color = as.factor(Year), linetype = Category), size = 1) +
  geom_point(aes(shape = Category, color = as.factor(Year)), size = 2) +
  scale_color_manual(values = rainbow(length(unique(combined_df$Year))), name = "Year") +
  scale_linetype_manual(values = category_linetypes, name = "Category") +
  scale_shape_manual(values = category_shapes, name = "Category") +
  labs(title = "Number of Residents and Children Living in BHH Monthly by Year",
       x = "Month",
       y = "Number of Residents/Children",
       color = "Year",
       linetype = "Category",
       shape = "Category") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Show only month abbreviations
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Smaller text size
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# Print the plot
print(n)

#ggsave("(n)ResidentsAndChildrenPerMonthByYear.jpg", plot = n, width = 14, height = 6, units = "in", dpi = 300, quality = 95)

# Save the data to a CSV file
#write.csv(combined_data, "monthly_occupancy.csv", row.names = FALSE)


# Initialize an empty data frame to store the results
occupancy_data <- data.frame()

# Define the years to iterate over
years <- 2017:2024

# Loop over each year
for (year in years) {
  # Create a sequence of months for the current year
  months_seq <- seq(as.Date(paste0(year, "-01-01")), 
                    as.Date(if (year == 2024) "2024-07-01" else paste0(year, "-12-01")), 
                    by = "month")
  
  # Calculate the number of residents living in the house each month
  residents_per_month <- sapply(months_seq, function(month) {
    residents <- sum(mydata2$Move.in.date <= month & 
                       (is.na(mydata2$Move.out.date) | mydata2$Move.out.date >= ceiling_date(month, "month") - days(1)), na.rm = TRUE)
    if (!is.na(residents) && residents > 15) residents <- 15
    return(residents)
  })
  
  # Calculate the number of children living in the house each month
  children_per_month <- sapply(months_seq, function(month) {
    sum((mydata2$Child.Start <= month & 
           (is.na(mydata2$Child.Leave) | mydata2$Child.Leave >= ceiling_date(month, "month") - days(1))) |
          (mydata2$Child2.Start <= month & 
             (is.na(mydata2$Child2.Leave) | mydata2$Child2.Leave >= ceiling_date(month, "month") - days(1))) |
          (mydata2$Child3.Start <= month & 
             (is.na(mydata2$Child3.Leave) | mydata2$Child3.Leave >= ceiling_date(month, "month") - days(1))), na.rm = TRUE)
  })
  
  # Combine the residents and children data for the current year into a data frame
  year_data <- data.frame(
    Year = year,
    Month = format(months_seq, "%Y-%m"),  # Format as Year-Month
    Residents = residents_per_month,
    Children = children_per_month
  )
  
  # Append the data for the current year to the overall data frame
  occupancy_data <- rbind(occupancy_data, year_data)
}

# Save the data to a CSV file
#write.csv(occupancy_data, "Monthly+Kids.csv", row.names = FALSE)

# Print a message to confirm the file was created
cat("CSV file 'monthly_occupancy_by_year.csv' has been created.")
