# Test basic operators, if, and while

x = 1
y = 2
z = x - y
print(x + y // z)       # -1
print(1 + 3 * x // y)   # 2
i = 0
while i < 20:
    if i % 2 == 0:
        i += 1
        continue
    if i > 10:
        break
    print(i)            # 1,3,5,7,9
    i += 1
x = "hi"
y = "bye"
z = x + y
print(z)                # hibye
