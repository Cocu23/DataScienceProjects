largest = None
smallest = None
while True:
    try:
        num = raw_input("Enter a number: ")
        if num == 'done':
            break;
        n = int(num)
		# smart way of if-conditions
        largest = num if largest<num or largest == None else largest
        smallest = num if smallest>num or smallest == None else smallest
    except:
        print "Invalid input"
    #print num

print "Maximum is", largest
print "Minimum is", smallest
  