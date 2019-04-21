# A program to tell you if it is OK or not.

print("Is it okay?")
your_age = int(input("Enter your age: "))
their_age = int(input("Enter their age: "))
okay = False

if your_age > their_age:
    if their_age >= your_age/2 + 7:
        okay = True
else:
    if your_age >= their_age/2 + 7:
        okay = True
if okay:
    print("OK!")
else:
    print("Eww.")
