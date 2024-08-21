import matplotlib.pyplot as plt

# Data for the plot
years = [2019, 2020, 2021, 2022, 2023]
total_revenues = [394587, 1022262, 1351212, 766933, 889489]
total_expenses = [447486, 535388, 479889, 583048, 699707]
net_income = [total_revenues[i] - total_expenses[i] for i in range(len(years))]

# Plotting the financial trends
plt.figure(figsize=(10, 6))
plt.plot(years, total_revenues, marker='o', label='Total Revenues', color='blue')
plt.plot(years, total_expenses, marker='o', label='Total Expenses', color='orange')
plt.plot(years, net_income, marker='o', label='Net Income', color='green')

# Adding titles and labels
plt.title("Financial Trends Over Time")
plt.xlabel("Year")
plt.ylabel("Amount ($)")
plt.xticks(years)  # Ensure only the years 2019-2023 are on the x-axis
plt.legend()
plt.grid(True)

# Display the plot
plt.show()
