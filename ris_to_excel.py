import sys
import subprocess

# Install dependencies if missing
def install_if_missing(package):
    try:
        __import__(package)
    except ImportError:
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])

install_if_missing("rispy")
install_if_missing("pandas")
install_if_missing("openpyxl")

import rispy
import pandas as pd

###put the .ris as the input file and the desired .xlsx as the output file, then manually change to csv file if needed###
input_file = r"D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\OVID.ris"
output_file = r"D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\ovid3.xlsx"

with open(input_file, "r", encoding="utf-8") as ris_fp:
    entries = rispy.load(ris_fp)

df = pd.DataFrame(entries)
df.to_excel(output_file, index=False)

print(f"✅ Converted '{input_file}' → '{output_file}' successfully!")


