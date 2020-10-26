a = int(input())

def gcd(a : int, b : int) -> int:
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
