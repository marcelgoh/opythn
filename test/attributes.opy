# Testing tricky attribute reference cases

class Test1:
    x = 2

a = Test1()
a.x = 4
print(Test1.x)

class Foo:
    def __init__(self, x):
        self.x = x

    def bar(self, y):
        return self.x + y

f = Foo(2)
b = f.bar
print(b(3))
