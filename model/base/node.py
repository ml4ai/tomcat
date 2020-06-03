class Node:

    """
    Class that represents a concrete node in a PGM. 
    """
    
    def __init__(self, metadata, time_slice=0, prior=False):
        """
        Constructor

        Parameters
        ----------
        metadata: node metadata
        time_slice: time slice of the node
        """
        self.metadata = metadata
        self.time_slice = time_slice
        self.assignment = None
        self.cpd = None

    def get_id(self):
        return (self.metadata.label, self.time_slice)
    
    def __str__(self):
        return '({},{})'.format(self.metadata.label, self.time_slice)
        
    def __repr__(self):
        return str(self)
    
    def __hash__(self):
        return hash(repr(self))
    
    def __eq__(self, other):
        return self.__class__ == other.__class__ and repr(self) == repr(other)   