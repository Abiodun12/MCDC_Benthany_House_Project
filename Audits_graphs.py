import pandas as pd
import glob


def parse_financial_data(file_path):
    data = {}
    current_section = None

    with open(file_path, 'r') as file:
        lines = file.readlines()
        for line in lines:
            print(f"Reading line: {line.strip()}")  # Debugging statement
            parts = line.strip().split(',')
            if len(parts) > 1:
                description = parts[0].strip()
                if 'STATEMENT OF' in description:
                    current_section = description
                    data[current_section] = {}
                elif any(char.isdigit() for char in description):
                    value = parts[-1].replace(',', '').replace('$', '').strip()
                    try:
                        value = float(value)
                        data[current_section][description] = value
                    except ValueError:
                        print(f"ValueError for line: {line.strip()}")  # Debugging statement
                        pass  # Skip if value can't be converted to float
                else:
                    print(f"Skipping line: {line.strip()}")  # Debugging statement for non-numeric lines
            else:
                print(f"Skipping line: {line.strip()}")  # Debugging statement for short lines
    return data


def extract_key_metrics(data, year):
    metrics = {
        "Year": year,
        "Total Revenues": None,
        "Total Expenses": None,
        "Net Assets Beginning of Year": None,
        "Net Assets End of Year": None
    }

    if 'STATEMENT OF ACTIVITIES' in data:
        activities = data['STATEMENT OF ACTIVITIES']
        for key, value in activities.items():
            if 'Total revenues' in key:
                metrics['Total Revenues'] = value
            elif 'Total expenses' in key:
                metrics['Total Expenses'] = value
    if 'STATEMENT OF FINANCIAL POSITION' in data:
        position = data['STATEMENT OF FINANCIAL POSITION']
        for key, value in position.items():
            if 'Net assets-beginning of year' in key:
                metrics['Net Assets Beginning of Year'] = value
            elif 'Net assets-end of year' in key:
                metrics['Net Assets End of Year'] = value

    return metrics


def main():
    # List of CSV files
    files = glob.glob("*_fin_audit.csv")

    # Extract data for each file
    summary_data = []
    for file in files:
        year = file.split('_')[0]
        try:
            parsed_data = parse_financial_data(file)
            print(f"Parsed data for {file}: {parsed_data}")  # Debugging statement
            metrics = extract_key_metrics(parsed_data, year)
            summary_data.append(metrics)
        except FileNotFoundError:
            print(f"File not found: {file}")
        except pd.errors.EmptyDataError:
            print(f"No data in file: {file}")
        except Exception as e:
            print(f"Error parsing file: {file}. Please check the file format. Error: {e}")

    # Convert to DataFrame for better visualization
    df_summary = pd.DataFrame(summary_data)
    print(df_summary)


if __name__ == "__main__":
    main()