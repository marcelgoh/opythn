# Basic multiplication in terms of addition

x = int(input("Enter the multiplicand: "))
y = int(input("Enter the multiplier: "))

acc = 0
while y > 0:
    if y % 2 == 0:
        x *= 2
        y //= 2
    else:
        acc += 1; y -= 1

print("The product is:", acc)
