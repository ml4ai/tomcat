import unittest

from base.edge import Edge
from base.node import Node

class TestEdge(unittest.TestCase):
    def test_valid_edge(self):
        node_a = Node('A')
        node_b = Node('B')
        edge = Edge(node_a, node_b, True)
        self.assertEqual(str(edge), 'A -(t)-> B')

    def test_invalid_edge(self):
        with self.assertRaises(TypeError):
            edge = Edge('A', 'B') 

if __name__ == '__main__':
    unittest.main()

			
