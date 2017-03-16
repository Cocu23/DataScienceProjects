lowercase = 'abcdefghijklmnopqrstuvwxyz'
digits = '0123456789'
#print all possible 2er combinations combined from lowercase + digits
# e.g. aa49, bz02

answer = [lowercase[i]+lowercase[j]for i in range(len(lowercase)) for j in range(len(lowercase))]
dig = [digits[i]+digits[j]for i in range(len(digits)) for j in range(len(digits))]
correct_answer = [answer[i]+dig[j] for i in range(len(answer)) for j in range(len(dig))]
print(len(correct_answer))
print(correct_answer[:50])
# shorter version
correct_answer2 = [a+b+c+d for a in lowercase for b in lowercase for c in digits for d in digits]
print(len(correct_answer2))
print(correct_answer2[:50])



# my_list =  [number for number in range(0,100) if number %2 == 0]
