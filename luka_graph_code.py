import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Data structure with 2021 data included
data = {
    2023: {
        'Total revenues': 920894.00,
        'Total expenses': 699707.00,
        'Program expenses': 643968.00,
        'Management and general': 41595.00,
        'Fundraising expenses': 14144.00,
        'Net assets-beginning of year': 2240655.00,
        'Net assets-end of year': 2461842.00,
    },
    2022: {
        'Total revenues': 773422.00,
        'Total expenses': 583048.00,
        'Program expenses': 517718.00,
        'Management and general': 49644.00,
        'Fundraising expenses': 15686.00,
        'Net assets-beginning of year': 2050281.00,
        'Net assets-end of year': 2240655.00,
    },
    2021: {
        'Total revenues': 1352004.00,
        'Total expenses': 479889.00,
        'Program expenses': 457083.00,
        'Management and general': 16413.00,
        'Fundraising expenses': 6393.00,
        'Net assets-beginning of year': 1178166.00,
        'Net assets-end of year': 2050281.00,
    },
    2020: {
        'Total revenues': 1023307.00,
        'Total expenses': 535388.00,
        'Program expenses': 523453.00,
        'Management and general': 11935.00,
        'Fundraising expenses': 0.00,
        'Net assets-beginning of year': 690247.00,
        'Net assets-end of year': 1178166.00,
    },
    2019: {
        'Total revenues': 408067.00,
        'Total expenses': 447486.00,
        'Program expenses': 434249.00,
        'Management and general': 10795.00,
        'Fundraising expenses': 2442.00,
        'Net assets-beginning of year': 729666.00,
        'Net assets-end of year': 690247.00,
    }
}

# Convert the data dictionary to a DataFrame
df = pd.DataFrame(data).T

# Calculate additional metrics
df['Net income'] = df['Total revenues'] - df['Total expenses']
df['Profit margin'] = df['Net income'] / df['Total revenues'] * 100
df['Program expense ratio'] = df['Program expenses'] / df['Total expenses'] * 100
df['Management expense ratio'] = df['Management and general'] / df['Total expenses'] * 100
df['Fundraising expense ratio'] = df['Fundraising expenses'] / df['Total expenses'] * 100

# Print summary statistics
print("Summary Statistics:")
print(df.describe())
print("\nAverage Metrics:")
print(f"Average Total Revenues: ${df['Total revenues'].mean():.2f}")
print(f"Average Total Expenses: ${df['Total expenses'].mean():.2f}")
print(f"Average Net Income: ${df['Net income'].mean():.2f}")
print(f"Average Profit Margin: {df['Profit margin'].mean():.2f}%")
print(f"Average Program Expense Ratio: {df['Program expense ratio'].mean():.2f}%")
print(f"Average Management Expense Ratio: {df['Management expense ratio'].mean():.2f}%")
print(f"Average Fundraising Expense Ratio: {df['Fundraising expense ratio'].mean():.2f}%")

# Plot financial trends
plt.figure(figsize=(10, 6))
plt.plot(df.index, df['Total revenues'], marker='o', label='Total Revenues')
plt.plot(df.index, df['Total expenses'], marker='o', label='Total Expenses')
plt.plot(df.index, df['Net income'], marker='o', label='Net Income')
plt.title('Financial Trends Over Time')
plt.xlabel('Year')
plt.ylabel('Amount ($)')
plt.legend()
plt.grid(True)
plt.savefig('financial_trends.png')
plt.show()

print("\nFinancial Trends Graph:")
print("This graph shows the trends of Total Revenues, Total Expenses, and Net Income over the years.")
print(f"Highest Total Revenue: ${df['Total revenues'].max():.2f} in {df['Total revenues'].idxmax()}")
print(f"Lowest Total Revenue: ${df['Total revenues'].min():.2f} in {df['Total revenues'].idxmin()}")
print(f"Highest Net Income: ${df['Net income'].max():.2f} in {df['Net income'].idxmax()}")
print(f"Lowest Net Income: ${df['Net income'].min():.2f} in {df['Net income'].idxmin()}")

# Plot expense breakdown
df_expenses = df[['Program expenses', 'Management and general', 'Fundraising expenses']]

df_expenses.plot(kind='bar', stacked=True, figsize=(10, 6))
plt.title('Expense Breakdown by Year')
plt.xlabel('Year')
plt.ylabel('Amount ($)')
plt.legend(title='Expense Categories')
plt.grid(True)
plt.savefig('expense_breakdown.png')
plt.show()

print("\nExpense Breakdown Graph:")
print("This stacked bar graph shows the breakdown of expenses into Program, Management, and Fundraising categories for each year.")
print(f"Year with highest total expenses: {df['Total expenses'].idxmax()} (${df['Total expenses'].max():.2f})")
print(f"Year with lowest total expenses: {df['Total expenses'].idxmin()} (${df['Total expenses'].min():.2f})")

# Plot correlation heatmap
corr = df.corr()
plt.figure(figsize=(10, 6))
sns.heatmap(corr, annot=True, cmap='viridis')
plt.title('Correlation Heatmap of Financial Metrics')
plt.savefig('correlation_heatmap.png')
plt.show()

print("\nCorrelation Heatmap:")
print("This heatmap shows the correlation between different financial metrics.")
print("Strongest positive correlation:", corr.unstack().sort_values(ascending=False).index[1])
print("Strongest negative correlation:", corr.unstack().sort_values().index[0])

# Plot efficiency ratios over time
plt.figure(figsize=(10, 6))
plt.plot(df.index, df['Program expense ratio'], marker='o', label='Program Expense Ratio')
plt.plot(df.index, df['Management expense ratio'], marker='o', label='Management Expense Ratio')
plt.plot(df.index, df['Fundraising expense ratio'], marker='o', label='Fundraising Expense Ratio')
plt.title('Expense Ratios Over Time')
plt.xlabel('Year')
plt.ylabel('Ratio (%)')
plt.legend()
plt.grid(True)
plt.savefig('efficiency_ratios.png')
plt.show()

print("\nEfficiency Ratios Graph:")
print("This graph shows how the ratios of Program, Management, and Fundraising expenses to Total expenses have changed over time.")
print(f"Highest Program Expense Ratio: {df['Program expense ratio'].max():.2f}% in {df['Program expense ratio'].idxmax()}")
print(f"Lowest Program Expense Ratio: {df['Program expense ratio'].min():.2f}% in {df['Program expense ratio'].idxmin()}")
print(f"Highest Management Expense Ratio: {df['Management expense ratio'].max():.2f}% in {df['Management expense ratio'].idxmax()}")
print(f"Highest Fundraising Expense Ratio: {df['Fundraising expense ratio'].max():.2f}% in {df['Fundraising expense ratio'].idxmax()}")