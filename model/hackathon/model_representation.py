from hackathon import evidence_extraction as evidence_extraction, utils as utils
import numpy as np


class CPDTables:

    def __init__(self, theta_s, pi_lt, theta_rm, pi_tg, pi_ty):
        # Fixed cpd_tables
        self.theta_rm = theta_rm
        self.pi_tg = pi_tg
        self.pi_ty = pi_ty

        # Trainable cpd_tables
        self.theta_s = theta_s
        self.pi_lt = pi_lt


class ParameterPriors:

    def __init__(self, theta_s_priors, pi_lt_priors):
        self.theta_s_priors = theta_s_priors
        self.pi_lt_priors = pi_lt_priors


class Model:

    def __init__(self, mission_map_id):
        self.mimission_map_id = mission_map_id
        self.number_of_rooms = evidence_extraction.get_number_of_areas(mission_map_id)

        if mission_map_id == evidence_extraction.MissionMap.SINGLEPLAYER:
            self.number_of_states = 115
            self.number_of_hallways = 7
        elif mission_map_id == evidence_extraction.MissionMap.SPARKY:
            self.number_of_states = 116
            self.number_of_hallways = 8
        else:
            self.number_of_states = 166
            self.number_of_hallways = 10

        self.cpd_tables = CPDTables(None, None, self.get_theta_rm(), self.get_pi_tg(), self.get_pi_ty())
        self.parameter_priors = ParameterPriors(self.get_theta_s_priors(), self.get_pi_lt_priors())

    def get_theta_rm(self):
        """
        This method returns the pre-defined distribution for theta_rm, because we can always tell the room by the state
        """

        # In the hallways, the number of the room in the number of the state
        theta_rm = np.zeros((self.number_of_states, self.number_of_rooms))
        for state in range(self.number_of_hallways):
            theta_rm[state] = self.one_hot_encode(self.number_of_rooms, [state])

        for state in range(self.number_of_hallways, self.number_of_states):
            room_index = int((state - self.number_of_hallways) / 6) + self.number_of_hallways
            theta_rm[state] = self.one_hot_encode(self.number_of_rooms, [room_index])

        return theta_rm

    def one_hot_encode(self, size, one_indices, zero=utils.ZERO):
        """
        This method returns a one hot encode vector with ones in the positions indicated by the parameter one_indices
        """
        vector = zero * np.ones(size)
        for index in one_indices:
            vector[index] = 1

        return vector

    def get_pi_tg(self):
        """
        This method returns the pre-defined distribution for pi_tg, because we can always tell if the player is
        triaging a victim by the state he's in
        """
        pi_tg = np.array([[1, utils.ZERO]]).repeat(self.number_of_states, axis=0)
        # Dark and light room triaging of green victims
        pi_tg[(self.number_of_hallways + 1):self.number_of_states:3] = [utils.ZERO, 1]

        return pi_tg

    def get_pi_ty(self):
        """
        This method returns the pre-defined distribution for pi_tg, because we can always tell if the player is
        triaging a victim by the state he's in
        """
        pi_ty = np.array([[1, utils.ZERO]]).repeat(self.number_of_states, axis=0)
        # Dark and light room triaging of yellow victims
        pi_ty[(self.number_of_hallways + 2):self.number_of_states:3] = [utils.ZERO, 1]
        return pi_ty

    def get_pi_lt_priors(self):
        """
        This method returns a matrix with the priors for each one of the pi_lt nodes in the model
        """
        priors = np.zeros((self.number_of_states, 2))

        # Lights in the hallway states (exception below) are always on
        priors[0:self.number_of_hallways, 0] = 1
        priors[0:self.number_of_hallways, 1] = utils.ZERO

        # Lights in the staging area can be on or off
        priors[1] = [1, 1]

        for state in range(self.number_of_hallways, self.number_of_states):
            if int((state - self.number_of_hallways) / 3) % 2 == 0:
                # States which the light is on
                priors[state] = [1, utils.ZERO]
            else:
                # States which the light is off
                priors[state] = [utils.ZERO, 1]

        return priors

    def get_theta_s_priors(self):
        """
        This method returns a matrix with the priors for each one of the theta_s nodes in the model
        """
        if self.mimission_map_id == evidence_extraction.MissionMap.SINGLEPLAYER:
            return self.get_theta_s_priors_for_singleplayer_map()
        elif self.mimission_map_id == evidence_extraction.MissionMap.SPARKY:
            return self.get_theta_s_priors_for_sparky_map()
        else:
            return self.get_theta_s_priors_for_falcon_map()

    def get_theta_s_priors_for_singleplayer_map(self):
        priors = np.zeros((self.number_of_states, self.number_of_states))

        priors[0] = self.one_hot_encode(self.number_of_states, [0, 1])
        priors[1] = self.one_hot_encode(self.number_of_states, [range(3), range(7, 31, 3)])
        priors[2] = self.one_hot_encode(self.number_of_states, [range(1, 5), range(55, 97, 3)])

        priors[3] = self.one_hot_encode(self.number_of_states,
                                        [2, 3, 5, range(13, 19, 3), range(31, 37, 3), range(55, 61, 3)])
        priors[4] = self.one_hot_encode(self.number_of_states, [2, 4, 6, range(73, 79, 3), range(97, 103, 3)])
        priors[5] = self.one_hot_encode(self.number_of_states, [3, 5, range(31, 55, 3)])
        priors[6] = self.one_hot_encode(self.number_of_states, [4, 6, range(97, 115, 3)])

        # P(theta_s | s_t-1 = state_some_room)
        priors[7:13] = self.__get_theta_s_prior_per_room_state(7, [range(13, 19, 3)], [1])
        priors[13:19] = self.__get_theta_s_prior_per_room_state(13, [range(7, 13, 3)], [1, 3, 5])
        priors[19:25] = self.__get_theta_s_prior_per_room_state(19, [range(25, 31, 3)], [1])
        priors[25:31] = self.__get_theta_s_prior_per_room_state(25, [range(19, 25, 3)], [1])
        priors[31:37] = self.__get_theta_s_prior_per_room_state(31, [range(37, 43, 3), range(55, 67, 3)], [3, 5])
        priors[37:43] = self.__get_theta_s_prior_per_room_state(37,
                                                                [range(31, 37, 3), range(43, 49, 3), range(61, 67, 3)],
                                                                [5])
        priors[43:49] = self.__get_theta_s_prior_per_room_state(43,
                                                                [range(37, 43, 3), range(49, 55, 3), range(61, 67, 3)],
                                                                [5])
        priors[49:55] = self.__get_theta_s_prior_per_room_state(49, [range(43, 49, 3), range(67, 73, 3)], [5])
        priors[55:61] = self.__get_theta_s_prior_per_room_state(55, [range(31, 37, 3), range(61, 67, 3)], [2, 3])
        priors[61:67] = self.__get_theta_s_prior_per_room_state(61,
                                                                [range(31, 49, 3), range(55, 61, 3), range(67, 73, 3)],
                                                                [2])
        priors[67:73] = self.__get_theta_s_prior_per_room_state(67, [range(49, 55, 3), range(61, 67, 3)], [2])
        priors[73:79] = self.__get_theta_s_prior_per_room_state(73, [range(79, 85, 3), range(97, 103, 3)], [2, 4])
        priors[79:85] = self.__get_theta_s_prior_per_room_state(79,
                                                                [range(73, 79, 3), range(85, 91, 3), range(97, 103, 3)],
                                                                [2])
        priors[85:91] = self.__get_theta_s_prior_per_room_state(85, [range(79, 85, 3), range(91, 97, 3),
                                                                     range(103, 109, 3)], [2])
        priors[91:97] = self.__get_theta_s_prior_per_room_state(91, [range(85, 91, 3), range(109, 115, 3)], [2])
        priors[97:103] = self.__get_theta_s_prior_per_room_state(97, [range(73, 85, 3), range(103, 109, 3)], [4, 6])
        priors[103:109] = self.__get_theta_s_prior_per_room_state(103, [range(85, 91, 3), range(97, 103, 3),
                                                                        range(109, 115, 3)], [6])
        priors[109:115] = self.__get_theta_s_prior_per_room_state(109, [range(91, 97, 3), range(103, 109, 3)], [6])

        return priors

    def __get_theta_s_prior_per_room_state(self, initial_state_number, adjacent_room_walk_state_numbers,
                                           adjacent_hallways_state_numbers):
        priors = np.zeros((6, self.number_of_states))

        # LRW to TGVLR, TYVLR, DRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [range(initial_state_number,
                                   initial_state_number + 4)] + adjacent_room_walk_state_numbers + adjacent_hallways_state_numbers
        priors[0] = self.one_hot_encode(self.number_of_states, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 1]
        priors[1] = self.one_hot_encode(self.number_of_states, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 2]
        priors[2] = self.one_hot_encode(self.number_of_states, valid_transitions)

        # DRW to TGVDR, TYVDR, LRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [initial_state_number, range(initial_state_number + 3,
                                                         initial_state_number + 6)] + adjacent_room_walk_state_numbers + adjacent_hallways_state_numbers
        priors[3] = self.one_hot_encode(self.number_of_states, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 4]
        priors[4] = self.one_hot_encode(self.number_of_states, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 5]
        priors[5] = self.one_hot_encode(self.number_of_states, valid_transitions)

        return priors

    def get_theta_s_priors_for_sparky_map(self):
        priors = np.zeros((self.number_of_states, self.number_of_states))

        priors[0] = self.one_hot_encode(self.number_of_states, [0, 1, 2])
        priors[1] = self.one_hot_encode(self.number_of_states, [range(6), range(8, 32, 3)])
        priors[2] = self.one_hot_encode(self.number_of_states, [range(3), range(8, 20, 3)])
        priors[3] = self.one_hot_encode(self.number_of_states, [1, 3, 4, 5, range(56, 98, 3)])

        priors[4] = self.one_hot_encode(self.number_of_states,
                                        [1, 3, 6, range(14, 20, 3), range(32, 38, 3), range(56, 62, 3)])
        priors[5] = self.one_hot_encode(self.number_of_states, [1, 3, 5, 7, range(74, 80, 3), range(98, 104, 3)])
        priors[6] = self.one_hot_encode(self.number_of_states, [4, 6, range(32, 56, 3)])
        priors[7] = self.one_hot_encode(self.number_of_states, [5, 7, range(98, 116, 3)])

        # P(theta_s | s_t-1 = state_some_room)
        priors[8:14] = self.__get_theta_s_prior_per_room_state(8, [range(14, 20, 3)], [1, 2])
        priors[14:20] = self.__get_theta_s_prior_per_room_state(14, [range(8, 14, 3)], [1, 2, 4])
        priors[20:26] = self.__get_theta_s_prior_per_room_state(20, [range(26, 32, 3)], [1])
        priors[26:32] = self.__get_theta_s_prior_per_room_state(26, [range(20, 26, 3)], [1])
        priors[32:38] = self.__get_theta_s_prior_per_room_state(32, [range(38, 44, 3), range(56, 68, 3)], [4, 6])
        priors[38:44] = self.__get_theta_s_prior_per_room_state(38,
                                                                [range(32, 38, 3), range(44, 50, 3), range(62, 68, 3)],
                                                                [6])
        priors[44:50] = self.__get_theta_s_prior_per_room_state(44,
                                                                [range(38, 44, 3), range(50, 56, 3), range(62, 68, 3)],
                                                                [6])
        priors[50:56] = self.__get_theta_s_prior_per_room_state(50, [range(44, 50, 3), range(68, 74, 3)], [6])
        priors[56:62] = self.__get_theta_s_prior_per_room_state(56, [range(32, 38, 3), range(62, 68, 3)], [3, 4])
        priors[62:68] = self.__get_theta_s_prior_per_room_state(62,
                                                                [range(32, 50, 3), range(56, 62, 3), range(68, 74, 3)],
                                                                [3])
        priors[68:74] = self.__get_theta_s_prior_per_room_state(68, [range(50, 56, 3), range(62, 68, 3)], [3])
        priors[74:80] = self.__get_theta_s_prior_per_room_state(74, [range(80, 86, 3), range(98, 104, 3)], [3, 5])
        priors[80:86] = self.__get_theta_s_prior_per_room_state(80,
                                                                [range(74, 80, 3), range(86, 92, 3), range(98, 104, 3)],
                                                                [3])
        priors[86:92] = self.__get_theta_s_prior_per_room_state(86, [range(80, 86, 3), range(92, 98, 3),
                                                                     range(104, 110, 3)], [3])
        priors[92:98] = self.__get_theta_s_prior_per_room_state(92, [range(86, 92, 3), range(110, 116, 3)], [3])
        priors[98:104] = self.__get_theta_s_prior_per_room_state(98, [range(74, 86, 3), range(104, 110, 3)], [5, 7])
        priors[104:110] = self.__get_theta_s_prior_per_room_state(104, [range(86, 92, 3), range(98, 104, 3),
                                                                        range(110, 116, 3)], [7])
        priors[110:116] = self.__get_theta_s_prior_per_room_state(109, [range(92, 98, 3), range(104, 110, 3)], [7])

        return priors

    def get_theta_s_priors_for_falcon_map(self):
        priors = np.zeros((self.number_of_states, self.number_of_states))

        priors[0] = self.one_hot_encode(self.number_of_states, [range(4)])
        priors[1] = self.one_hot_encode(self.number_of_states, [range(3)])
        priors[2] = self.one_hot_encode(self.number_of_states, [1, 2, range(118, 124, 3)])
        priors[3] = self.one_hot_encode(self.number_of_states, [0, 3, 4, range(10, 16, 3)])

        priors[4] = self.one_hot_encode(self.number_of_states, [3, 4, 5, range(124, 130, 3)])
        priors[5] = self.one_hot_encode(self.number_of_states, [4, 5, 6, 8, 9, range(10, 52, 3), range(124, 166, 3)])
        priors[6] = self.one_hot_encode(self.number_of_states,
                                        [5, 7, range(46, 52, 3), range(58, 70, 3), range(160, 166, 3)])
        priors[7] = self.one_hot_encode(self.number_of_states, [6, 7, 8, 9, range(64, 166, 3)])
        priors[8] = self.one_hot_encode(self.number_of_states, [5, 7, 8, range(124, 148, 3)])
        priors[9] = self.one_hot_encode(self.number_of_states, [5, 7, 9, range(148, 166, 3)])

        # P(theta_s | s_t-1 = state_some_room)
        priors[10:16] = self.__get_theta_s_prior_per_room_state(10, [range(16, 22, 3)], [3, 5])
        priors[16:22] = self.__get_theta_s_prior_per_room_state(16, [range(10, 16, 3), range(22, 28, 3)], [5])
        priors[22:28] = self.__get_theta_s_prior_per_room_state(22, [range(16, 22, 3), range(28, 34, 3)], [5])
        priors[28:34] = self.__get_theta_s_prior_per_room_state(28, [range(22, 28, 3), range(34, 40, 3)], [5])
        priors[34:40] = self.__get_theta_s_prior_per_room_state(34, [range(28, 34, 3), range(40, 46, 3)], [5])
        priors[40:46] = self.__get_theta_s_prior_per_room_state(40,
                                                                [range(34, 40, 3), range(46, 52, 3), range(62, 68, 3)],
                                                                [5])
        priors[46:52] = self.__get_theta_s_prior_per_room_state(46,
                                                                [range(40, 46, 3), range(52, 64, 3)], [5, 6])
        priors[52:58] = self.__get_theta_s_prior_per_room_state(52, [range(46, 52, 3)], [])
        priors[58:64] = self.__get_theta_s_prior_per_room_state(58, [range(46, 52, 3), range(64, 70, 3)], [6])
        priors[64:70] = self.__get_theta_s_prior_per_room_state(64, [range(58, 64, 3)], [6, 7])
        priors[70:76] = self.__get_theta_s_prior_per_room_state(70, [range(76, 82, 3)], [7])
        priors[76:82] = self.__get_theta_s_prior_per_room_state(76, [range(70, 76, 3), range(82, 88, 3)], [7])
        priors[82:88] = self.__get_theta_s_prior_per_room_state(82, [range(76, 82, 3), range(88, 94, 3)], [7])
        priors[88:94] = self.__get_theta_s_prior_per_room_state(88, [range(82, 88, 3), range(94, 100, 3)], [7])
        priors[94:100] = self.__get_theta_s_prior_per_room_state(94, [range(88, 94, 3), range(100, 106, 3)], [7])
        priors[100:106] = self.__get_theta_s_prior_per_room_state(100, [range(94, 100, 3), range(106, 112, 3)], [7])
        priors[106:112] = self.__get_theta_s_prior_per_room_state(106, [range(100, 112, 3), range(112, 118, 3)], [7])
        priors[112:118] = self.__get_theta_s_prior_per_room_state(112, [range(106, 112, 3), range(118, 124, 3)], [7])
        priors[118:124] = self.__get_theta_s_prior_per_room_state(118, [range(112, 118, 3)], [2, 7])
        priors[124:130] = self.__get_theta_s_prior_per_room_state(124, [], [4, 5, 7, 8])
        priors[130:136] = self.__get_theta_s_prior_per_room_state(130, [range(136, 142, 3), range(148, 154, 3)], [5, 8])
        priors[136:142] = self.__get_theta_s_prior_per_room_state(136, [range(130, 136, 3), range(142, 160, 3)], [8])
        priors[142:148] = self.__get_theta_s_prior_per_room_state(142, [range(136, 142, 3), range(154, 160, 3)], [7, 8])
        priors[148:154] = self.__get_theta_s_prior_per_room_state(148, [range(130, 142, 3), range(154, 160, 3)], [5, 7, 9])
        priors[154:160] = self.__get_theta_s_prior_per_room_state(154, [range(136, 154, 3), range(104, 110, 3)], [7, 9])
        priors[160:166] = self.__get_theta_s_prior_per_room_state(160, [], [5, 6, 7, 9])

        return priors
