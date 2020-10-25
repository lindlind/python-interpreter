# compound statement
a = 23 & 26 | 10 ^ 15
'''
23 = "10111"
26 = "11010"
10 = "01100"
15 = "01111"

23 & 26 = "10010"
10 ^ 15 = "01100"

so a should be "11110" = 30
'''
expected = (1 << 5) - 2

# check results
true = a == expected
