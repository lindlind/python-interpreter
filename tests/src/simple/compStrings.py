smallString = "xyz"
longString = "abacaba"
spaces = "   "

byLen1 = smallString < longString
byLen2 = smallString + spaces == longString
byLex1 = smallString > longString
byLex2 = smallString + spaces != longString

emptyStringDoNothing1 = smallString == smallString + "" + ''
emptyStringDoNothing2 = longString == "" + '' + longString
emptyStringDoNothing3 = '' + spaces + ""
