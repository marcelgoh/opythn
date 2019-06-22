# Binary trees extended example

# basic stack data structure
class Stack:
    def __init__(self):
        self.list = []
        self.size = 0

    def is_empty(self):
        return self.size == 0

    def push(self, item):
        self.list = [item] + self.list
        self.size += 1

    def pop(self):
        if self.size <= 0:
            print("Cannot pop an empty stack: STACK.POP()")
            return None
        else:
            ret_val = self.list[0]
            self.list = self.list[1:self.size]
            self.size -= 1
            return ret_val

# binary tree node
class Node:
    def __init__(self, info):
        self.info = info
        self.left = None
        self.right = None

    def set_left(self, new_left):
        self.left = new_left

    def set_right(self, new_right):
        self.right = new_right

    def get_info(self):
        return info

    def is_leaf(self):
        self.left == None and self.right == None

    # printable "preorder" representation of a node (recursive)
    def to_string(self):
        left_string = "()" if self.left == None else self.left.to_string()
        right_string = "()" if self.right == None else self.right.to_string()
        return "(" + str(self.info) + " " + left_string + " " + right_string + ")"

    # performs binary tree insertion
    def insert(self, new):
        if new == self.info:
            return     # ignore new value if already in tree
        elif new < self.info:
            if self.left == None:
                self.left = Node(new)
            else:
                self.left.insert(new)
        else:
            if self.right == None:
                self.right = Node(new)
            else:
                self.right.insert(new)


# creates a binary search tree (requires that elements be comparable)
class SearchTree:
    # creates a new empty tree
    def __init__(self):
        self.root = None

    def to_string(self):
        return "()" if self.root == None else self.root.to_string()

    def insert(self, item):
        if self.root == None:
            self.root = Node(item)
        else:
            self.root.insert(item)

# tests
stack = Stack()
stack.push('a')
stack.push(2)
stack.push(False)
print(stack.list)
print(stack.pop())
print(stack.list)

test_tree = SearchTree()
test_tree.insert('k')
test_tree.insert('v')
test_tree.insert('b')
test_tree.insert('j')
test_tree.insert('q')
test_tree.insert('g')
test_tree.insert('k')  # should be ignored
print(test_tree.to_string())
