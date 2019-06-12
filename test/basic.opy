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
print(x ^ y >> 3 - z + 55 & 16)

## FLOATS ##
x = 4.999
print(round(x % 2.3, 10), round(x % -2.3, 10))
print(round(x // 2.3, 7), round(x // -2.3, 7))
print(round(x / 2.3, 6), round(x / -2.3, 6))
y = -2.39
print(round(y % -1.9, 6), round(y % -1.9, 6))
print(round(y // -1.9, 6), round(y // -1.9, 6))
print(round(y / -1.9, 7), round(y / -1.9, 7))

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

## TEST SOME BUILT-INS ##
print(bin(-34))
print(bin(344))
print(bool("skdlfja"))
print(bool(""))
print(bool(4.4))
print(bool(0.0))
print(bool(0))
print(bool())
print(bool(None))
print(chr(65))
print(chr(97))
print(chr(97+25))
print(hex(435))
print(hex(0))
print(hex(-34))
print(oct(435))
print(oct(0))
print(oct(-34))
print(ord("5"))
print(ord('A'))
print(ord('$'))

## CAUSE OVERFLOW OF STACK? ##
i = 0
sum = 0
while i < 100000:
    sum = sum + 1
    i = i + 1

print(sum)
