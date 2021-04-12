with open('D:\parse.txt', 'r') as reader:
    while True:
        line1 = reader.readline()
        if not line1:
                break
        line2 = reader.readline()
        line3 = reader.readline()
        line4 = reader.readline()
        with open('D:\parse.csv', 'a') as writer:
            string = line1+line2+line3+line4
            strings_by_column = [string.split('\n')]
            strings_by_column = strings_by_column [0][:4]
            strings_by_line = '","'.join(strings_by_column)
            strings_by_line = strings_by_line+'\n'
            writer.writelines( strings_by_line )
                
