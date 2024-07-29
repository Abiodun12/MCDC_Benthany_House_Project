import pandas as pd
import plotly.graph_objects as go
import plotly.express as px
import plotly.io as pio

# Data structure
data = {
    2023: {
        'Total revenues': 1141871.00,
        'Total expenses': 699707.00,
        'Program expenses': 643968.00,
        'Management and general': 41595.00,
        'Fundraising expenses': 14144.00,
        'Net assets-beginning of year': 1620324.00,
        'Net assets-end of year': 2240655.00,
        'Supportive Services': 2023.00,
        'Stipends': 23814.00,
        'Employee benefits': 42833.00,
        'Payroll taxes': 20494.00,
        'Personnel expense': 344896.00,
        'Supplies': 6486.00,
        'Food for participants': 31608.00,
        'Office supplies and expense': 199.00,
        'Printing and postage': 1722.00,
        'Travel': 3389.00,
        'Telephone and internet': 1354.00,
        'Computer supplies': 446.00,
        'Computer support services': 24000.00,
        'Participant expenses': 50798.00,
        'Participant exp.-Beyond Bethany': 88350.00,
        'Professional fees': 14129.00,
        'Filing fees': 15.00,
        'Insurance': 11339.00,
        'Bank charges': 936.00,
        'Staff development': 482.00,
        'Board meetings': 702.00,
        'Other': 328.00,
        'Depreciation': 55.00,
        'Contributions-religious orders': 216319.00,
        'Foundation grants': 149068.00,
        'Churches and other organizations': 20623.00,
        'Inkind contributions': 88086.00,
        'Released from restrictions': 472977.00,
        'Total public support': 1110466.00,
        'Interest income': 31405.00,
        'Total other revenue': 31405.00,
    },
    2022: {
        'Total revenues': 1152614.00,
        'Total expenses': 583048.00,
        'Program expenses': 517718.00,
        'Management and general': 49644.00,
        'Fundraising expenses': 15686.00,
        'Net assets-beginning of year': 1050758.00,
        'Net assets-end of year': 2050281.00,
        'Stipends': 24758.00,
        'Employee benefits': 36720.00,
        'Payroll taxes': 16536.00,
        'Personnel expense': 287914.00,
        'Supplies': 3913.00,
        'Food for participants': 26003.00,
        'Office supplies and expense': 1208.00,
        'Printing and postage': 1196.00,
        'Travel': 644.00,
        'Telephone and internet': 4736.00,
        'Computer supplies': 446.00,
        'Computer support services': 24000.00,
        'Participant expenses': 31381.00,
        'Participant exp.-Beyond Bethany': 52855.00,
        'Professional fees': 20464.00,
        'Filing fees': 40.00,
        'Insurance': 8504.00,
        'Bank charges': 1663.00,
        'Other': 92.00,
        'Depreciation': 482.00,
        'Contributions-religious orders': 517938.00,
        'Foundation grants': 36250.00,
        'Churches and other organizations': 8998.00,
        'Inkind contributions': 78000.00,
        'Released from restrictions': 388192.00,
        'Total public support': 1146125.00,
        'Interest income': 6489.00,
        'Total other revenue': 6489.00,
    },
    2020: {
        'Total expenses': 189310.00,
        'Stipends': 47253.00,
        'Employee benefits': 49718.00,
        'Payroll taxes': 15998.00,
        'Personnel expense': 302279.00,
        'Supplies': 5775.00,
        'Food for participants': 28470.00,
        'Office supplies and expense': 3160.00,
        'Postage': 219.00,
        'Travel': 468.00,
        'Vehicle expenses': 1470.00,
        'Telephone and internet': 616.00,
        'Computer supplies': 3101.00,
        'Participant expenses': 83662.00,
        'Professional fees': 7327.00,
        'Filing fees': 154.00,
        'Insurance': 5282.00,
        'Bank charges': 995.00,
        'Depreciation': 1287.00,
        'Contributions-religious orders': 732604.00,
        'Foundation grants': 59300.00,
        'Churches and other organizations': 6000.00,
        'Inkind contributions': 121240.00,
        'Released from restrictions': 163441.00,
    },
    2019: {
        'Total revenues': 485663.00,
        'Total expenses': 447486.00,
        'Program expenses': 434249.00,
        'Management and general': 10795.00,
        'Fundraising expenses': 2442.00,
        'Net assets-beginning of year': 486281.00,
        'Net assets-end of year': 729666.00,
        'Stipends': 23674.00,
        'Employee benefits': 45017.00,
        'Payroll taxes': 14774.00,
        'Personnel expense': 235860.00,
        'Occupancy': 57890.00,
        'Supplies': 4543.00,
        'Food for participants': 25929.00,
        'Office supplies': 1596.00,
        'Postage': 422.00,
        'Travel': 1010.00,
        'Vehicle expenses': 2427.00,
        'Telephone and internet': 1120.00,
        'Computer supplies': 2319.00,
        'Participant expenses': 95100.00,
        'Professional fees': 8027.00,
        'Filing fees': 25.00,
        'Insurance': 6964.00,
        'Other': 356.00,
        'Depreciation': 75.00,
        'Contributions-religious orders': 106400.00,
        'Foundation grants': 258175.00,
        'Churches and other organizations': 151775.00,
        'Inkind contributions': 42700.00,
        'Released from restrictions': 183996.00,
        'Total public support': 472183.00,
        'Fundraising event': 9470.00,
        'Interest income': 4010.00,
        'Total other revenue': 13480.00,
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

# Plot financial trends
fig1 = go.Figure()
fig1.add_trace(go.Scatter(x=df.index, y=df['Total revenues'], mode='lines+markers', name='Total Revenues'))
fig1.add_trace(go.Scatter(x=df.index, y=df['Total expenses'], mode='lines+markers', name='Total Expenses'))
fig1.add_trace(go.Scatter(x=df.index, y=df['Net income'], mode='lines+markers', name='Net Income'))
fig1.update_layout(title='Financial Trends Over Time', xaxis_title='Year', yaxis_title='Amount ($)')
fig1.write_html('financial_trends.html')  # Save as HTML

# Plot expense breakdown
fig2 = go.Figure()
fig2.add_trace(go.Bar(x=df.index, y=df['Program expenses'], name='Program Expenses'))
fig2.add_trace(go.Bar(x=df.index, y=df['Management and general'], name='Management and General'))
fig2.add_trace(go.Bar(x=df.index, y=df['Fundraising expenses'], name='Fundraising Expenses'))
fig2.update_layout(title='Expense Breakdown by Year', xaxis_title='Year', yaxis_title='Amount ($)')
fig2.update_layout(barmode='stack')
fig2.write_html('expense_breakdown.html')  # Save as HTML

# Plot correlation heatmap
corr = df.corr()
fig3 = px.imshow(corr, text_auto=True, color_continuous_scale='Viridis')
fig3.update_layout(title='Correlation Heatmap of Financial Metrics')
fig3.write_html('correlation_heatmap.html')  # Save as HTML

# Plot efficiency ratios over time
fig4 = go.Figure()
fig4.add_trace(go.Scatter(x=df.index, y=df['Program expense ratio'], mode='lines+markers', name='Program Expense Ratio'))
fig4.add_trace(go.Scatter(x=df.index, y=df['Management expense ratio'], mode='lines+markers', name='Management Expense Ratio'))
fig4.add_trace(go.Scatter(x=df.index, y=df['Fundraising expense ratio'], mode='lines+markers', name='Fundraising Expense Ratio'))
fig4.update_layout(title='Expense Ratios Over Time', xaxis_title='Year', yaxis_title='Ratio (%)')
fig4.write_html('efficiency_ratios.html')  # Save as HTML
