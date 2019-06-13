# Gets the longest alphabetical substring of a string

s = input("Enter a string: ")
longest_index = 0
longest_len = 1
this_index = 0
this_len = 1

for i in range(1, len(s)):
    if ord(s[i]) >= ord(s[i-1]):
        this_len += 1
    else:
        this_len = 1
        this_index = i
    if this_len > longest_len:
        longest_len = this_len
        longest_index = this_index
print("Longest substring in alphabetical order:",
      s[longest_index:longest_index + longest_len])
