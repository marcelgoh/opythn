# Test objects and classes

class Animal:
    message = "There is a house in New Orleans."
    def talk(self):
        print(self.message)

    def print_three():
        print(3)

class Mammal(Animal):
    def talk(self):
        print("I am fuzzy.")

class Dog(Mammal):
    def talk(self, n, adjective):
        while n > 0:
            print(adjective + " woof.")
            n = n - 1

class Bat(Mammal):
    def fly(self):
        print("Whoosh.")

class Human(Animal):
    message = "A human is an animal with a name."
    def __init__(self, name):
        self.name = name

print(Animal.message)
eric_burdon = Animal()
eric_burdon.talk()
clifford = Dog()
clifford.talk(3, "Big")
dracula = Bat()
dracula.talk()
dracula.fly()

j = Human("jill")
print(j.name)
j.talk()

Animal.print_three()
Human.print_three()

def foo():
    x = 12
    class Abc():
        y = x
        def bar(self):
            print(x)
            print(self.y)
    abc = Abc()
    abc.bar()

foo()

