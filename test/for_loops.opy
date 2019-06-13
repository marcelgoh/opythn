# For loops and ranges

for i in ["hi", True, 1.618, 13]:
    print(i)

num_os = 0
for c in "topology":
    if c == "o":
        num_os += 1
print(num_os)

for i in range(3):
    print(i)

for i in range(-2, 2):
    print(i)

# Shouldn't print anything
for i in range(6, -2, 16):
    print(i)

for i in range(19, -23, -5):
    print(i)

for i in range(-12, 88, 37):
    print(i)
