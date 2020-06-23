from world import World
from pos import Pos
from aabb import AABB
from block import Block
import random


class ProceduralGenerator:
    """
    This class represents a procedural generator.
    """

    def generate_grid_world(self, N, sep=10, AABB_size=10, filename = None):
        """
        This method uses the procedural generation objectto create a grid world and output the generated world to a JSON file.

        Args:
            N (int): The number of AABB on each axis of the grid world
            sep (int, optional): The separation between AABB on the grid in cardinal directions. Defaults to 10.
            AABB_size (int, optional): The size of the cubic AABB. Defaults to 10.
            filename (str, optional): The name of the output JSON. Defaults to grid_world_mm_dd_yyyy_hh_mm_ss.json.
        """
        world = World()
        world = self._generate_N_AABB_grid(world,N,sep,AABB_size)
        world = self._generate_blocks(world)
        world.to_JSON(filename)


    def get_random_victim(self, pos):
        """
        This method gets a random victim. Maybe this method should be moved outside this class?

        Args:
            pos (Pos): The position at which to create the victim block

        Returns:
            Block: The victim block
        """
        randInt = random.randint(1,2)
        if randInt == 1:
            victim = Block("victim", pos, "prismarine")
        else:
            victim = Block("victim", pos, "gold")
        return victim


    def _generate_N_AABB_grid(self, world,  N, sep, AABB_size=10):
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
        world.add_AABB(prev_aabb)

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

            world.add_AABB(cur_aabb)
            prev_aabb = cur_aabb  # Add the AABB to the list of AABB and set the previous value = current so the next AABB can be built at the correct relative position
        return world

    def _generate_blocks(self, world):
        for aabb in world._aabb_list:
            middle_x = aabb.get_top_left().get_x() + (aabb.get_bottom_right().get_x() - aabb.get_top_left().get_x())//2
            middle_z = aabb.get_top_left().get_z() + (aabb.get_bottom_right().get_z() - aabb.get_top_left().get_z())//2
            middle = Pos(middle_x, 0 , middle_z)
            
            top_edge_mid = aabb.get_top_left()
            top_edge_mid.set_x(middle_x)
            top_edge_mid.set_y(0)  # Door in the middle of top edge

            bottom_edge_mid = aabb.get_bottom_right()
            bottom_edge_mid.set_x(middle_x)
            bottom_edge_mid.set_y(0)  # Door in the middle of bottom edge

            left_edge_mid = aabb.get_top_left()
            left_edge_mid.set_z(middle_z)
            left_edge_mid.set_y(0)  # Door in the middle of left edge

            right_edge_mid = aabb.get_bottom_right()
            right_edge_mid.set_z(middle_z)
            right_edge_mid.set_y(0)  # Door in the middle of right edge

            top_door = Block("door", top_edge_mid, "wood")
            bottom_door = Block("door", bottom_edge_mid, "wood")
            left_door = Block("door", left_edge_mid, "wood")
            right_door = Block("door", right_edge_mid, "wood")  # Doors are wooden
            victim = self.get_random_victim(middle)  # Add a victim in every AABB

            world._block_list.extend([top_door, bottom_door, left_door, right_door])
            world._block_list.append(victim)
        return world

a_cool_generator =  ProceduralGenerator()
a_cool_generator.generate_grid_world(1, filename="out.json")
