class NodeMetadata:

    """
    Class that represents metadata of a node in a PGM. 
    """
    
    def __init__(self, label, first_time_slice=0, repeatable=False, extendable=False, cardinality=1, state_names={}, prior=False):
        """
        Constructor

        Parameters
        ----------
        label: uniquely identifier of a node
        first_time_slice: first time slice where the node must show up in the extended model (unrolled in time)
        repeatable: whether the node mus show up in the model in subsequent time slices after it shows up for 
                    the first time in the extented model 
        extendable: the node shows up only once in the extended model (in the assigned time slice) and 
                    connects to other repeatable nodes in successive time slices according to the edges defined
        cardinality: number of discrete states a node may have
        state_names: names of the discrete states a node may have                 
        prior: indicates whether this represents a prior of another node's distribution
        """

        if repeatable and extendable:
            raise TypeError('A node cannot be repeatable and extendable at the same time')  

        self.label = label
        self.first_time_slice = first_time_slice
        self.repeatable = repeatable
        self.extendable = extendable
        self.cardinality = cardinality
        self.state_names = state_names
        self.prior = prior
    
    def __str__(self):
        return '{}'.format(self.label)
        
    def __repr__(self):
        return str(self)
    
    def __hash__(self):
        return hash(repr(self))
    
    def __eq__(self, other):
        return self.__class__ == other.__class__ and repr(self) == repr(other)   