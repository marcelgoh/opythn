# Test basic operators, if, and while

## INTS ##
x = int("1")
y = int("2")
z = x - y
print(-123 % -4, -123 % 4, 123 % -4, 123 % 4)
print(-123 // -4, -123 // 4, 123 // -4, 123 // 4)
print(round(-23 ** -2, 4), -123 ** 4, round(23 ** -2, 4), 123 ** 4)
print(-123 / -4, -123 / 4, 123 / -4, 123 / 4)
print(x + y // z)       # -1
print(1 + 3 * x // y)   # 2

## BITWISE OPS ##
print(56 << 4 & (-23 | 89) >> 8 ^ 32)

## FLOATS ##
x = 4.999
print(round(x % 2.3, 6), round(x % -2.3, 6))
y = -2.39
print(round(y % -1.9, 6), round(y % -1.9, 6))

## CONTROL STRUCTURES ##
i = 0
while not i > 20:
    if i % 2 == 0:
        i += 1
        continue
    if i > 10:
        break
    print(i)            # 1,3,5,7,9
    i += 1

## STRING CONCAT ##
x = "hi"
y = "bye"
z = x + y
print(z)                # hibye

