# Test lists, tuples, dictionaries, and strings

s = "0abcdefghijklmnopqrstuvwxyz"
l = [False, 'a', [1, [1]], 3.14, 2 * 2]
d = { "Knuth" : "USA", "Dijkstra" : "Netherlands",
      "Turing" : "UK", "Hopper" : "USA" }

print(s[2:-8])
print(d["Knuth"])
print(s[2])
print(l[1:3][1][1][0])
