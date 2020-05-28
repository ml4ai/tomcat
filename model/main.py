import numpy as np

if __name__ == '__main__':
    import numpy as np
    from base.node import Node
    from base.edge import Edge
    from distribution.cpd import CPD
    from distribution.discrete.multinomial import Multinomial
    from model.pgm import PGM
    from model.extended_pgm import ExtendedPGM
    from sampling.ancestral_sampling import AncestralSampling
    import networkx as nx

    np.random.seed(42)

    PBAE = Node('PBAE', 0, True, False, 2, state_names={0:'Room 207', 1:'Room 208'})
    S = Node('S', 0, True, False, 2, state_names={0:'Saving a Villager', 1:'Battling'})
    Q = Node('Q', 1, False, True, 2, state_names={0:'Aware about the dog', 1:'Unaware about the dog'})
    T = Node('T', 1, False, False, 2, state_names={0:'Cluster 1', 1:'Cluster 2'})
    POS = Node('XY', 1, True, False, None, state_names={})
    nodes = [PBAE, S, Q, T, POS]

    edge1 = Edge(PBAE, PBAE, True)
    edge2 = Edge(PBAE, S, True)
    edge3 = Edge(S, S, True)
    edge4 = Edge(Q, S, False)
    edge5 = Edge(T, PBAE, False)
    edge6 = Edge(S, POS, False)
    edges = [edge1, edge2, edge3, edge4, edge5, edge6]

    PBAE_prior = CPD(PBAE, [], Multinomial([0.3, 0.7]))
    S_prior = CPD(S, [], Multinomial([0.4, 0.6]))
    Q_prior = CPD(Q, [], Multinomial([0.5, 0.5]))
    T_prior = CPD(T, [], Multinomial([0.2, 0.8]))
    POS_prior = CPD(T, [], Gaussian([0, 0], np.eye(2)))

    S_given_PBAE_Q_S = CPD(S, [PBAE, Q, S], [Multinomial([0.2,0.8]), Multinomial([0.6,0.4]), 
                                             Multinomial([0.3,0.7]), Multinomial([0.5,0.5]),
                                             Multinomial([0.25,0.75]), Multinomial([0.6,0.4]),
                                             Multinomial([0.2,0.8]), Multinomial([0.5,0.5])])
    PBAE_given_PBAE = CPD(PBAE, [PBAE], [Multinomial([0.3,0.7]), Multinomial([0.65,0.35])])
    PBAE_given_PBAE_T = CPD(PBAE, [PBAE, T], [Multinomial([0.2,0.8]), Multinomial([0.4,0.6]), 
                                             Multinomial([0.5,0.5]), Multinomial([0.8,0.2])])
    POS_given_S = CPD(POS, [S], [Gaussian([3, 5], 2*np.eye(2)), Gaussian([10, 15], 1*np.eye(2))])

    cpds = [PBAE_prior, S_prior, Q_prior, T_prior, POS_prior, S_given_PBAE_Q_S, PBAE_given_PBAE, PBAE_given_PBAE_T, POS_given_S]

    pgm = PGM()
    pgm.add_nodes_from(nodes)
    pgm.add_edges_from(edges)
    pgm.add_cpds_from(cpds)

    representation = ExtendedPGM(pgm, 4)
    nx.draw_shell(representation, with_labels=True)

    sampling = AncestralSampling(representation)
    sample = sampling.generate_sample()
    print(sample)
