import csv

with open('cpu-short.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    data_before =[]
    for row in reader:
        data_before.append(row)

with open('cpu-clean.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    data_after =[]
    for row in reader:
        data_after.append(row)

print(data_before[4][10])
print(data_after[4][10])
for row in range(len(data_after)):
    for col in range(len(data_after[row])):
        if(data_after[row][col] == 'NA' or data_after[row][col] == '') :
            if(data_before[row][col] == 'N/A' or data_before[row][col] == '' or data_before[row][col] == 'NA') :
                continue
            else :
                print(f"lost data {data_before[row][col]}, data after ={data_after[row][col]} at row = {row} and col = {col}")
# for row in before:
#     for col in data_before:

