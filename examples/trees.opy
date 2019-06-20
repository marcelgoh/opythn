# Binary trees extended example

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

test_tree = SearchTree()
test_tree.insert('k')
print(test_tree.to_string())
test_tree.insert('v')
test_tree.insert('b')
test_tree.insert('j')
test_tree.insert('q')
test_tree.insert('g')
test_tree.insert('k')  # should be ignored
# print(test_tree.to_string())
