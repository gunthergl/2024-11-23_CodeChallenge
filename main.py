import sys
import os

raw_data_dir = sys.argv[1]
print(raw_data_dir)

# now do stuff with the data inside raw_data_dir
os.makedirs("res", exist_ok=True)