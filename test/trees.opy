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

    def peek(self):
        if self.size <= 0:
            print("Cannot peek at an empty stack: STACK.PEEK()")
        else:
            return self.list[0]

    def pop(self):
        if self.size <= 0:
            print("Cannot pop an empty stack: STACK.POP()")
            return None
        else:
            ret_val = self.list[0]
            if self.size == 1:
                self.list = []
            else:
                self.list = self.list[1:self.size]
            self.size -= 1
            return ret_val

# binary tree node
class Node:
    def __init__(self, info):
        self.info = info
        self.left = None
        self.right = None

    # since we won't have duplicate info fields, we can just compare info
    def __eq__(self, other):
        if isinstance(other, Node):
            return self.info == other.info
        else:
            return False

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

    def search(self, item):
        if type(item) != type(self.info):
            return False
        if item == self.info:
            return True
        elif item < self.info:
            return self.left != None and self.left.search(item)
        else:
            return self.right != None and self.right.search(item)


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

    # returns True if item is in tree, False otherwise
    def search(self, item):
        return self.root != None and self.root.search(item)

    # traverses tree and prints elements in three orders
    def traverse(self):
        if self.root == None:
            print("Cannot traverse empty tree.")
        else:
            inorder_list = []
            preorder_list = []
            postorder_list = []

            stack = Stack()
            curr = self.root
            last = None
            i = 0
            while True:
                if curr != None:
                    preorder_list = preorder_list + [curr.info]
                    stack.push(curr)
                    curr = curr.left
                    continue
                if stack.is_empty():
                    break
                curr = stack.pop()
                if curr.right == None or curr.right == last:
                    if curr.right != last or last == None:
                        inorder_list += [curr.info]
                    postorder_list += [curr.info]
                    last = curr
                    curr = None
                else:
                    inorder_list += [curr.info]
                    stack.push(curr)
                    curr = curr.right
                i += 1

            print("Preorder:", preorder_list)
            print("Inorder:", inorder_list)
            print("Postorder:", postorder_list)

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
test_tree.insert('a')
test_tree.insert('z')
test_tree.insert('j')
test_tree.insert('q')
test_tree.insert('g')
test_tree.insert('k')  # should be ignored
print(test_tree.to_string())
test_tree.traverse()
print(test_tree.search('c'))
print(test_tree.search('g'))
print(test_tree.search(None))

