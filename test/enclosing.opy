p = 13
z = 14

def foo(y):
  n = y
  p = 3

  def bar():
    x = 1
    nonlocal n
    global p

    a = x   # defined in this scope       LOCAL(0) **
    b = y   # defined in enclosing scope  LOCAL(1)
    c = z   # defined in global scope     GLOBAL  **
# d = m   # not defined in any scope    GLOBAL  **
    e = n   # explicitly nonlocal         LOCAL(1)
    f = p   # explicitly global           GLOBAL
    print(a, b, c, e, f)

  class Baz:
    x = 1
    nonlocal n
    global p

    a = x   # defined in this scope       NAME  **
    b = y   # defined in enclosing scope  LOCAL(0)
    c = z   # defined in global scope     NAME **
# d = m   # not defined in any scope    NAME **
    e = n   # explicitly nonlocal         LOCAL(0)
    f = p   # explicitly global           GLOBAL

  print(Baz().b)
  print(Baz.a, Baz.b, Baz.c, Baz.e, Baz.f)
  bar()

  y = 7

foo(191)
