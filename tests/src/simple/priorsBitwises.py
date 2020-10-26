# compound statement
a = 23 & 26 | 10 ^ 15
'''
23 = "10111"
26 = "11010"
10 = "01010"
15 = "01111"

23 & 26 = "10010"
10 ^ 15 = "00101"

so a should be "10111" = 23
'''
expected = (1 << 5) - 1 - (1 << 3)

# check results
true = a == expected
