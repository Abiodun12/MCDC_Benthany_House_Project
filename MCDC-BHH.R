setwd("/Users/someoddsense/Desktop/MCDC")
# Read the CSV file
mydata <- read.csv("~/Desktop/MCDC/BHR(R).csv", na.strings = c("n/a", "NA", "NULL", ""))

# Check the structure of the data
str(mydata)


mydata <- mydata[-c(117:1001), ]

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

ggplot(mydata, aes(x = Age, y = Children)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Age vs. Children",
       x = "Age",
       y = "Children")

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

ggplot(mydata, aes(x = Age.at.move.in, y = Children)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Age at Move in vs. Children",
       x = "Age at Move in",
       y = "Children")

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
ggplot(average_stay, aes(x = reorder(COO, Average_Length_of_Stay), y = Average_Length_of_Stay)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Length of Stay by COO",
       x = "Country of Origin",
       y = "Average Length of Stay (days)") +
  theme_minimal()

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

# Create a density plot
ggplot(mydata, aes(x = Length.of.Stay....of.days.)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Length of Stay", x = "Length of Stay (days)", y = "Density") +
  theme_minimal()

# Calculate the mean
mean_length_of_stay <- mean(mydata$Length.of.Stay....of.days., na.rm = TRUE)

# Create a density plot with a mean line
ggplot(mydata, aes(x = Length.of.Stay....of.days.)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_length_of_stay), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density Plot of Length of Stay", 
       x = "Length of Stay (days)", 
       y = "Density") +
  theme_minimal() +
  annotate("text", x = mean_length_of_stay, y = 0, 
           label = paste("Mean = ", round(mean_length_of_stay, 2)), 
           color = "black", vjust = -17.1, hjust = -.1)

#histogram for length of stay plus mean line
ggplot(mydata, aes(x = Length.of.Stay....of.days.)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Length of Stay", x = "Length of Stay (days)", y = "Frequency") +
  theme_minimal() +
  # Add mean line
  geom_vline(xintercept = mean_length_of_stay, color = "red", linetype = "dashed", size = 1)

# histogram with bins ??
# Define bin width and breaks
binwidth <- 1
bins <- seq(min(mydata$Length.of.Stay....of.days.), max(mydata$Length.of.Stay....of.days.) + binwidth, by = binwidth)

#average age at move in
average_age <- mean(mydata$Age.at.move.in, na.rm = TRUE)

# Plot histogram with mean line
hist(mydata$Age.at.move.in, breaks = 10, xlab = "Age at Move-in", main = "Histogram of Age at Move-in")
abline(v = average_age, col = "red", lwd = 2)
legend("topright", legend = paste("Mean =", round(average_age, 2)), col = "red", lwd = 2)

# Plot density plot with mean line
plot <- ggplot() +
  geom_density(data = mydata, aes(x = Age.at.move.in), fill = "blue", alpha = 0.5) +
  labs(x = "Age at Move-in", y = "Density", title = "Density Plot of Age at Move-in") +
  theme_minimal() +
  # Add mean line with annotation
  geom_vline(xintercept = average_age, linetype = "dashed", size = 1, color = "red") +
  annotate("text", x = average_age, y = 0.25, label = paste("Mean Age:", round(average_age, 2)),
           color = "red", size = 3, hjust = -.1, vjust = 3.5)
print(plot)

# distribution of move in dates by month
mydata$Month <- month(mydata$MoveInDate, label = TRUE)

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
ggplot(month_counts_df, aes(x = Month, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Distribution of Move-In Dates by Month",
       x = "Month", y = "Frequency") +
  theme_minimal()

# 7/24
# creating new csv for average length of stay by countries map

# Step 1: Calculate average length of stay by country
avg_stay <- aggregate(Length.of.Stay....of.days. ~ COO, data = mydata, FUN = mean)

# Step 2: Add a new rounded field to the data frame
avg_stay$Average_Length_Rounded <- round(avg_stay$Length.of.Stay....of.days.)

# Step 3: Write results to a new CSV file
write.csv(avg_stay, file = "average_stay_by_country.csv", row.names = FALSE)


#7/25

mydata2 <- read.csv("~/Desktop/MCDC/BHR(R.2).csv", na.strings = c("n/a", "NA", "NULL", ""))

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

#attempting line graph for birth years
library(dplyr)   # Load dplyr
library(ggplot2)

mydata2$Child.Bday <- as.Date(mydata2$Child.Bday, format = "%m/%d/%y")

# Check for any NA values after conversion
na_count <- sum(is.na(mydata2$Child.Bday))
print(paste("Number of NA values in Child.Bday:", na_count))

# Filter out any NA values in Child.Bday
mydata2 <- mydata2 %>%
  filter(!is.na(Child.Bday))

# Extract Year from the Date column
mydata2$Year <- as.numeric(format(mydata2$Child.Bday, "%Y"))

# Check the Year column to ensure it's correctly created
print(table(mydata2$Year))

# Count the number of children born each year
yearly_counts <- mydata2 %>%
  count(Year) %>%
  rename(Number_of_Children = n) # Rename the count column

# Print the yearly_counts data frame to ensure it has data
print(yearly_counts)

# Plot the data using ggplot2
ggplot(yearly_counts, aes(x = Year, y = Number_of_Children)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Number of Children Born Each Year",
       x = "Year",
       y = "Number of Children") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = .4))  # Move title to the right

#overlay the second lines
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)  # For pivot_longer function

# Convert Child.Bday, Child.2, and Child.3 to Date format
mydata2$Child.Bday <- as.Date(mydata2$Child.Bday, format = "%m/%d/%y")
mydata2$Child.2 <- as.Date(mydata2$Child.2, format = "%m/%d/%y")
mydata2$Child.3 <- as.Date(mydata2$Child.3, format = "%m/%d/%y")

# Replace empty strings with NA
mydata2$Child.Bday[mydata2$Child.Bday == ""] <- NA
mydata2$Child.2[mydata2$Child.2 == ""] <- NA
mydata2$Child.3[mydata2$Child.3 == ""] <- NA

# Also check for specific placeholder values (e.g., 'N/A', 'NA', 'null', 'NULL')
placeholder_values <- c('NA', 'N/A', 'null', 'NULL', ' ', '')

mydata2$Child.Bday[mydata2$Child.Bday %in% placeholder_values] <- NA
mydata2$Child.2[mydata2$Child.2 %in% placeholder_values] <- NA
mydata2$Child.3[mydata2$Child.3 %in% placeholder_values] <- NA

# Check for any NA values after conversion
na_count_bday <- sum(is.na(mydata2$Child.Bday))
na_count_child2 <- sum(is.na(mydata2$Child.2))
na_count_child3 <- sum(is.na(mydata2$Child.3))
print(paste("Number of NA values in Child.Bday:", na_count_bday))
print(paste("Number of NA values in Child.2:", na_count_child2))
print(paste("Number of NA values in Child.3:", na_count_child3))

# Extract Year from the Date columns and count the number of children born each year
yearly_counts_bday <- mydata2 %>%
  filter(!is.na(Child.Bday)) %>%
  mutate(Year = as.numeric(format(Child.Bday, "%Y"))) %>%
  count(Year) %>%
  mutate(Type = "Child.Bday")

yearly_counts_child2 <- mydata2 %>%
  filter(!is.na(Child.2)) %>%
  mutate(Year = as.numeric(format(Child.2, "%Y"))) %>%
  count(Year) %>%
  mutate(Type = "Child.2")

yearly_counts_child3 <- mydata2 %>%
  filter(!is.na(Child.3)) %>%
  mutate(Year = as.numeric(format(Child.3, "%Y"))) %>%
  count(Year) %>%
  mutate(Type = "Child.3")

# Combine the data frames
yearly_counts <- bind_rows(yearly_counts_bday, yearly_counts_child2, yearly_counts_child3)

# Debug: print the combined data frame to check the counts
print(yearly_counts)

# Plot the data using ggplot2
ggplot(yearly_counts, aes(x = Year, y = n, color = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Children Born Each Year",
       x = "Year",
       y = "Number of Children") +
  scale_color_manual(values = c("Child.Bday" = "blue", "Child.2" = "red", "Child.3" = "green")) +
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 1))

# total births per year from bdays
mydata2 <- read.csv("~/Desktop/MCDC/BHR(R.2).csv", na.strings = c("n/a", "NA", "NULL", ""))

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
ggplot(yearly_counts, aes(x = Year, y = n)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Total Number of Children Born Each Year",
       x = "Year",
       y = "Number of Children") +
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.4))


