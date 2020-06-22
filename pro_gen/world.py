from pos import Pos
from aabb import AABB
from block import Block
import json
from datetime import datetime


class World:
    """
    This class represents a world in Minecraft as a collection of AABBs.
    """

    def __init__(self):
        """
        Conctructor requires no arguments.
        """
        self._aabb_list = list()
        self._block_list = list()



    def add_AABB(self, aabb):
        """
        Adds the given AABB to the list of AABB the world object knows about.

        Args:
            aabb (AABB): The AABB to add
        """
        self._aabb_list.append(aabb)



    def add_block(self, block):
        """
        Adds the given Block object to the World

        Args:
            block (Block): The block to add
        """
        self._block_list.append(block)



    def generate_N_AABB_grid(self, N, sep, AABB_size=10):
        """
        This method initializes this World into a grid of NxN AABB structures such that each AABB is a cube of size = AABB_size.
        In the grid the separation between two adjacent AABBs along the cardinal directions are equal to the number of separation units specified.
        The AABB material defaults to wood.

        Args:
            N (int): The number of AABB along each axis
            sep (int): The unit separation between adjacent AABB in the cardinal directions.
            AABB_size (int, optional): The size of the AABB cube. Defaults to 10.
        """

        # Create the 1st AABB at the top left of the grid. We call this AABB's
        # top left = 0,0,0
        id_ctr = 1
        top_left = Pos(0, 0, 0)
        bottom_right = Pos(AABB_size, AABB_size, AABB_size)

        prev_aabb = AABB(id_ctr, top_left, bottom_right)
        self.add_AABB(prev_aabb)

        # Add the remaining N^2 -1 AABB
        while id_ctr <= N * N - 1:
            cur_aabb = None
            id_ctr += 1
            if (id_ctr - 1) % N == 0:
                # If a row is complete, then stage coordinates to create a room directly below the last AABB in the first columnv
                # Only Z needs to be changed in the X-Z plane to paralelly
                # shift "down"
                top_left = Pos(0, 0, 0)
                top_left.set_z(prev_aabb.get_bottom_right().get_z() + sep)

                # Again, only Z changes so we can calculate the top_right of
                # the AABB using the bottom left
                bottom_right = Pos(
                    AABB_size,
                    AABB_size,
                    top_left.get_z() +
                    AABB_size)

                cur_aabb = AABB(id_ctr, top_left, bottom_right)

            else:
                # It is along the same row so only X changes and is further by
                # the separation amount from the "right" extreme of the
                # previous AABB
                top_left = prev_aabb.get_top_left()
                top_left.set_x(prev_aabb.get_bottom_right().get_x() + sep)

                bottom_right = prev_aabb.get_bottom_right()
                bottom_right.set_x(top_left.get_x() + AABB_size)

                cur_aabb = AABB(id_ctr, top_left, bottom_right)

            self.add_AABB(cur_aabb)
            prev_aabb = cur_aabb  # Add the AABB to the list of AABB and set the previous value = current so the next AABB can be built at the correct relative position



    def to_JSON(self, filename=None):
        """
        Creates a JSON from the World object where each AABB has an entry in an output dictionary such that they key value pairs are:

        AABB <id number> : (x1,y1,z1) (x2,y2,z2) (material)

        where x1,y1,z1 is the tuple for the top left and x2,y2,z2 is the bottom right

        Args:
            filename (str, optional): Name of the file to store output the JSON to. If nothing is given the filename will be:
                                      grid_world_mm_dd_yyyy_hh_mm_ss.JSON
        """
        output_dict = dict()
        for aabb in self._aabb_list:
            key = "AABB " + str(aabb.get_id())
            value = "{} {} {}".format(str(aabb.get_top_left()), str(
                aabb.get_bottom_right()), aabb.get_material())
            output_dict[key] = value

        j_object = json.dumps(output_dict)

        if filename is None:
            now = datetime.now()
            filename = "grid_world" + \
                str(now.strftime("_%d_%m_%Y_%H_%M_%S")) + ".JSON"

        JSON_file = open(filename, "w")
        JSON_file.write(j_object)
        JSON_file.close()



    def debug_print_AABB(self, N):
        """
        A debug print function to see if the AABB were added correctly. You have to correctly specify the grid size.

        Args:
            N ([int]): The number of AABB in each axis. This should match what was given to the generator.
        """

        for i in range(len(self._aabb_list)):
            if ((i + 1) % N) == 0:
                print(str(self._aabb_list[i]))
            else:
                print(str(self._aabb_list[i]), end="\t")


# Sample code which creates a world, initializes it to a grid of size 2x2
# Each AABB is of size 10 and is separated by 15 units from the adjacent AABBs
world = World()
world.generate_N_AABB_grid(2, 15)
# world.debug_print_AABB(2)
world.to_JSON()
