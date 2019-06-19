# Test lists, tuples, dictionaries, and strings

s = "0abcdefghijklmnopqrstuvwxyz"
l = [False, 'a', [1, [1]], 3.14, 2 * 2, 'e', 6, 7]
t = (False, 'a', [1, [1]], 3.14, 2 * 2, 'e', 6, 7)
d = { "Knuth" : "USA", "Dijkstra" : "Netherlands",
      "Turing" : "UK", "Hopper" : "USA", "Von Neumann" : "Hungary" }

print(s[2:-8])
print(d["Knuth"], d["Von Neumann"])
d["Von Neumann"] = "USA"
print(d["Von Neumann"])

# This doesn't pass, because order is unspecified.
# for k in d:
#    print(d[k])

print(s[2])
print(l[1:3][1][1][0])

print(t[-4])
print(t[-8:7])

del l[3:6]
print(l)
del l[1]
print(l)

good_stuff = [0,1,2,3,4,5,6,7,8,9,10]

good_stuff[3] = "smetanito"
print(good_stuff)
good_stuff[7:9] = ("mattoni",)
good_stuff[1] = ["coffee", "tea"]
print(good_stuff)
good_stuff[0:1] = ("trdelnik", "hranolky", "pivo")
print(good_stuff)
