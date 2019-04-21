# Sums the odd numbers up to input

limit = int(input("Enter a threshold: "))
sum = 0
x = 0

while x < limit:
    if x % 2 == 1:
        sum = sum + x
    x += 1

print("The sum is", sum)

