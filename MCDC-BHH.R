setwd("/Users/someoddsense/Desktop/MCDC")
# Read the CSV file
mydata <- read.csv("~/Desktop/MCDC/BHR(R.3).csv", na.strings = c("n/a", "NA", "NULL", ""))

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

a <- ggplot(mydata, aes(x = Age, y = Children)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Age vs. Children",
       x = "Age",
       y = "Children")
print(a)

ggsave("(A)scatterplot of age vs childre.jpg", plot = a, width = 5, height = 5, units = "in", dpi = 300, quality = 95)

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
ggsave("(B) age at move in vs children.jpg", plot = b, width = 5, height = 5, units = "in", dpi = 300, quality = 95)

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
ggsave("(C) average length of stay by COO.jpg", plot = c, width = 8, height = 8, units = "in", dpi = 300, quality = 95)

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
ggsave("(D) density plot length of stay.jpg", plot = d, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


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

ggsave("(E) Histogram of age at move in.jpg", plot = e, width = 7, height = 7, units = "in", dpi = 300, quality = 95)

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
ggsave("(F) Density plot of age at move in.jpg", plot = f, width = 7, height = 7, units = "in", dpi = 300, quality = 95)

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
g <- ggplot(month_counts_df, aes(x = Month, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Distribution of Move-In Dates by Month",
       x = "Month", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))  # Adjust the title size
print(g)
ggsave("(G) Distribution of move in dates by month.jpg", plot = g, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


# 7/24
# creating new csv for average length of stay by countries map

# Step 1: Calculate average length of stay by country
avg_stay <- aggregate(Length.of.Stay....of.days. ~ COO, data = mydata, FUN = mean)

# Step 2: Add a new rounded field to the data frame
avg_stay$Average_Length_Rounded <- round(avg_stay$Length.of.Stay....of.days.)

# Step 3: Write results to a new CSV file
write.csv(avg_stay, file = "average_stay_by_country.csv", row.names = FALSE)


#7/25

mydata2 <- read.csv("~/Desktop/MCDC/BHR(R.3).csv", na.strings = c("n/a", "NA", "NULL", ""))

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
ggsave("(H) Total Number of Children Born Each Year.jpg", plot = h, width = 7, height = 7, units = "in", dpi = 300, quality = 95)


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
ggsave("(i)Do_Residents_Have_Children.jpg", plot = i, width = 8, height = 8, units = "in", dpi = 300, quality = 95)


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
ggsave("(j)Before.During.After.jpg", plot = j, width = 8, height = 6, units = "in", dpi = 300, quality = 95)

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
ggsave("(K)move_in_out_dates_by_year.jpg", plot = k, width = 8, height = 6, units = "in", dpi = 300, quality = 95)
