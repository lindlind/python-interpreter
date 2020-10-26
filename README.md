# Python interpreter

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com//fp-homework/blob/master/hw3/LICENSE)

## Usage example

1. Put your **\*.py** file in **.\\pysrc\\** directory
2. Do **stack run**
3. Follow the instructions

You can find **example.py** in **.\\pysrc\\** directory, which expects a number ***N*** and print all coprime numbers less then ***N***.

To print it, choose appropriate mode and print file name.

```
Print mode:
pretty-print
Print filename:
example.py
```

Result is
```
def gcd(a, b):
    if a < 0:
        a = -a
    if b < 0:
        b = -b
    if b == 0:
        return 0
    while True:
        if a % b == 0:
            break
        t = a
        a = b
        b = t % b
    return b
k = 2
while k < a:
    gcd = gcd(a, k)
    if (gcd == 1):
        print(k)
    k = k + 1
```

To interpret it, choose appropriate mode, print file name, then print ***N***.
```
Print mode:
interpret
Print filename:
example.py
60
```

Result is
```
7
11
13
17
19
23
29
31
37
41
43
47
49
53
59
```
## Supports: 

Supported types:
 * str
 * int
 * float
 * bool

Types can be casted to each other

Supported operations:
  * boolean operations:
    * and
    * or
    * not
  * compare operations over all types:
    * (==)
    * (!=)
    * (<)
    * (>)
    * (<=)
    * (>=)
  * bitwise operations over ints:
    * and
    * or
    * xor
    * left shift
    * right shift
  * numeric operations:
    * (+)
    * (-)
    * (\*)
    * (/)
    * (//)
    * (%)
    * (\*\*)
    * unar plus
    * unar minus
  * string concatenation
  * string slices

Supported constructions
  * if-elif-else statement
  * while cycle
  * custom functions of zero/one/two arguments

Oneline and multiline comments are supported.

## Restrictions

Last line of code should be empty.
All indents should be 4-spaced.
Multiple assigns ```a , b = ...``` are forbidden.
Multiple returns ```return a, b``` are forbidden.

Custom function needs hints of argument types and result type.
Example: 
```
def isEven(x : int) -> bool:
    return x % 2 == 0
```