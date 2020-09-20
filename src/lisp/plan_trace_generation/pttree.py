# This class was modified from code that I found here 
# https://stackoverflow.com/questions/23595801/how-to-serialize-a-tree-class-object-structure-into-json-file-format
# The point of the class is to store plan trace trees as a tree data structure
# which can be converted into a json object and vice versa. 

class PTTNode(dict):
    def __init__(self, task=None, state=None, children=None):
        super().__init__()
        self.__dict__ = self
        self.state = state if state is not None else "()"
        self.task = task if task is not None else "%"
        self.children = list(children) if children is not None else []

    @staticmethod
    def from_dict(dict_):
        """ Recursively (re)construct TreeNode-based tree from dictionary. """
        node = PTTNode(dict_['task'], dict_['state'], dict_['children'])
        node.children = list(map(PTTNode.from_dict, node.children))
        return node

    @staticmethod
    def from_sexplist(sexplist_):
        """Construct Tree from s-expression list, only gets task structure in
        the case of plan trees"""
        def symbols_to_string(slist):
            task = ""
            for i in slist:
                task = task + " " + i.value()
            return task.strip()

        node = PTTNode(symbols_to_string(sexplist_[0]))
        for i in range(1,len(sexplist_)):
            if isinstance(sexplist_[i][0], list):
                node.children.append(PTTNode.from_sexplist(sexplist_[i]))
            else:
                node.children.append(PTTNode(symbols_to_string(sexplist_[i][1])))
        return node

    def preorder(self):
        nodes = [self]
        for i in self.children:
            nodes = nodes + i.preorder()
        return nodes


