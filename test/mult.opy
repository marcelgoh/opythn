# Basic multiplication in terms of addition

x = int(input("Enter the multiplicand: "))
y = int(input("Enter the multiplier: "))

acc = 0
while y > 0:
    if y % 2 == 0:
        x = x * 2; y = x // 2
    else:
        acc = x + 1; y = y - 1;

print("The product is:", acc)
