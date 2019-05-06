# Test functions and lambda expressions

x = 19

def foo():
    y = 100
    print(x + y)
    x = 1

    def bar():
        nonlocal y
        global x
        print(x + y)

        def baz():
            y = 3
            print(y + x)

print((lambda x, y: x * 7 - y)(12, 2))
