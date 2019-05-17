# Test objects and classes

class Animal:
    message = "There is a house in New Orleans."
    def talk(self):
        print(self.message)

class Mammal(Animal):
    def talk(self):
        print("I am fuzzy.")

class Dog(Mammal):
    def talk(self):
        print("Woof.")

class Bat(Mammal):
    def fly(self):
        print("Whoosh.")

eric_burdon = Animal()
eric_burdon.talk()
clifford = Dog()
clifford.talk()
dracula = Bat()
dracula.talk()
dracula.fly()

def foo():
    x = 12
    class Abc():
        y = x
        def bar(self):
            print(self.y)
    abc = Abc()
    abc.bar()

foo()
