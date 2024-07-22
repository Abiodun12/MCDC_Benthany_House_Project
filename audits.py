import pandas as pd
import matplotlib.pyplot as plt


def parse_financial_data(df):
    data = {}
    current_section = None
    for index, row in df.iterrows():
        if 'STATEMENT OF' in row['Description']:
            current_section = row['Description'].strip()
            data[current_section] = {}
        elif '$' in row['Description'] or row['Description'].replace(',', '').isdigit():
            parts = row['Description'].split('$')
            if len(parts) == 2:
                key, value = parts
            else:
                key = df.iloc[index - 1]['Description'] if index > 0 else 'Unknown'
                value = parts[0]
            key = key.strip()
            value = value.strip().replace(',', '')
            try:
                data[current_section][key] = float(value)
            except ValueError:
                pass  # Skip if value can't be converted to float
    return data


def analyze_data(data, year):
    print(f"\nAnalysis for {year}:")
    for section, items in data.items():
        print(f"\n{section}:")
        for key, value in items.items():
            print(f"{key}: ${value:,.2f}")


def plot_expenses(all_data):
    years = list(all_data.keys())
    total_expenses = []
    for data in all_data.values():
        if 'STATEMENT OF ACTIVITIES' in data and 'Total expenses' in data['STATEMENT OF ACTIVITIES']:
            total_expenses.append(data['STATEMENT OF ACTIVITIES']['Total expenses'])
        else:
            total_expenses.append(0)  # or None, depending on how you want to handle missing data

    plt.figure(figsize=(10, 6))
    plt.bar(years, total_expenses)
    plt.title('Total Expenses by Year')
    plt.xlabel('Year')
    plt.ylabel('Total Expenses ($)')
    plt.show()


# List of CSV files
files = [
    '2023_fin_audit.csv',
    '2022_fin_audit.csv',
    '2021_fin_audit.csv',
    '2020_fin_audit.csv',
    '2019_fin_audit.csv'
]

all_data = {}

for file in files:
    year = file.split('_')[0]
    try:
        df = pd.read_csv(file)
        parsed_data = parse_financial_data(df)
        all_data[year] = parsed_data
        analyze_data(parsed_data, year)
    except FileNotFoundError:
        print(f"File not found: {file}")
    except pd.errors.EmptyDataError:
        print(f"No data in file: {file}")
    except pd.errors.ParserError:
        print(f"Error parsing file: {file}. Please check the file format.")

# Plot total expenses across years
plot_expenses(all_data)

# Compare specific metrics across years
metrics_to_compare = ['Total revenues', 'Total expenses', 'Surplus (deficit) from operations']

for metric in metrics_to_compare:
    print(f"\n{metric} across years:")
    for year, data in all_data.items():
        if 'STATEMENT OF ACTIVITIES' in data and metric in data['STATEMENT OF ACTIVITIES']:
            value = data['STATEMENT OF ACTIVITIES'][metric]
            print(f"{year}: ${value:,.2f}")
        else:
            print(f"{year}: Data not available")