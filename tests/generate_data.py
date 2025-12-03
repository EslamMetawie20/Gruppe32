import json
import random

def generate_data(filename, count):
    data = []
    for i in range(1, count + 1):
        record = {
            "id": i,
            "name": f"Item_{i}",
            "value": round(random.uniform(1.0, 1000.0), 2)
        }
        data.append(record)
    
    with open(filename, 'w') as f:
        json.dump(data, f, indent=2)
    
    print(f"Successfully generated {count} records in '{filename}'.")

if __name__ == "__main__":
    generate_data("large_data.json", 10000)
