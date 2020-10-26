def f0() -> int:
    return 42

def f1(x : int) -> int:
    return x ^ 42

def f2(x : int, y : str) -> bool:
    q = int(y)
    return x == q

r0 = f0()
r1 = f1(42>>1) + 1
r2 = f2(56, "56")
