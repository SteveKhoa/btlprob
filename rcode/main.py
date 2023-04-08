import csv

with open('cpu-before.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    data_before =[]
    for row in reader:
        data_before.append(row)

with open('cpu after.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    data_after =[]
    for row in reader:
        data_after.append(row)

print(data_before[0][6])
print(data_after[0][6])
for row in range(len(data_after)):
    for col in range(len(data_after[row])):
        if(data_after[row][col] == 'NA') :
            if(data_before[row][col] == 'N/A' or data_before[row][col] == '' or data_before[row][col] == 'NA') :
                continue
            else :
                print(f"lost data {data_before[row][col]}, data after ={data_after[row][col]} at row = {row} and col = {col}")
# for row in before:
#     for col in data_before:

