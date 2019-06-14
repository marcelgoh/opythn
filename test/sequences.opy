# Test lists, tuples, dictionaries, and strings

s = "0abcdefghijklmnopqrstuvwxyz"
l = [False, 'a', [1, [1]], 3.14, 2 * 2, 'e', 6, 7]
t = (False, 'a', [1, [1]], 3.14, 2 * 2, 'e', 6, 7)
d = { "Knuth" : "USA", "Dijkstra" : "Netherlands",
      "Turing" : "UK", "Hopper" : "USA" }

print(s[2:-8])
print(d["Knuth"])
print(s[2])
print(l[1:3][1][1][0])

print(t[-4])
print(t[-8:7])

del l[3:6]
print(l)
del l[1]
print(l)

