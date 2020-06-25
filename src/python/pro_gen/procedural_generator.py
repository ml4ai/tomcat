#!/usr/bin/env python3

from world import World
from pos import Pos
from aabb import AABB
from block import Block
import random
import sys


class ProceduralGenerator:
    """
    This class represents a procedural generator.
    """

    def generate_grid_world(self, N, sep, AABB_size, filename=None):
        """
        This method uses the procedural generation objectto create a grid world and output the generated world to a JSON file.

        Args:
            N (int): The number of AABB on each axis of the grid world
            sep (int, optional): The separation between AABB on the grid in cardinal directions.
            AABB_size (int, optional): The size of the cubic AABB.
            filename (str, optional): The name of the output JSON. Defaults to grid_world_mm_dd_yyyy_hh_mm_ss.json in current directory.
        """
        world = World()
        world = self._generate_N_AABB_grid(world, N, sep, AABB_size)
        world = self._generate_blocks(world)
        world.to_JSON(filename)

    def get_random_victim(self, pos, green_bias = 0.5):
        """
        This method gets a random victim. Maybe this method should be moved outside this class?

        Args:
            pos (Pos): The position at which to create the victim block
            green_bias (float): Enter a value between 0 and 1 for the percentage of victims that should be green. Defaults to 0.5.

        Returns:
            Block: The victim block
        """
        green_favour = green_bias * 100

        randInt = random.randint(0, 100)
        if randInt <= green_favour:
            victim = Block("victim", pos, "prismarine")
        else:
            victim = Block("victim", pos, "gold")
        return victim

    def _generate_N_AABB_grid(self, world, N, sep, AABB_size):
        """
        This method initializes this World into a grid of NxN AABB structures such that each AABB is a cube of size = AABB_size.
        In the grid the separation between two adjacent AABBs along the cardinal directions are equal to the number of separation units specified.
        The AABB material defaults to wood.

        Args:
            N (int): The number of AABB along each axis
            sep (int): The unit separation between adjacent AABB in the cardinal directions.
            AABB_size (int, optional): The size of the AABB cube.
        """

        # Create the 1st AABB at the top left of the grid. We call this AABB's
        # top left = 0,0,0
        id_ctr = 1
        top_left = Pos(1, 0, 1)
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
                top_left = Pos(1, 0, 1)
                top_left.set_z(prev_aabb.get_bottom_right().get_z() + sep)

                # Again, only Z changes so we can calculate the top_right of
                # the AABB using the bottom left
                bottom_right = Pos(
                    AABB_size, AABB_size, top_left.get_z() + AABB_size
                )

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
            self._gen_all_doors_in_aabb(world, aabb)
            self._gen_victim_in_aabb(world,aabb)
     

        return world

    def _gen_victim_in_aabb(self, world, aabb):
        # offset by 2 in all directions
        rand_pos = aabb.get_random_pos_at_base(2,2,2,2)
        
        # Add victims to random positions in roos. Randomise the rooms to which victims are added (50% in this case)
        rand= random.randint(0, 100)
        if rand <= 75:
            victim = self.get_random_victim(rand_pos, green_bias = 0.60)
            world._block_list.append(victim)
        else:
            pass
    
    def _gen_all_doors_in_aabb(self, world, aabb):
        edges = aabb.get_edges_at_base()
        top_edge_mid = edges[0]
        right_edge_mid = edges[1]
        bottom_edge_mid = edges[2]
        left_edge_mid = edges[3]  # All of these are at y = 0 ad we want it that way for the doors

        top_door = Block("door", top_edge_mid, "oak_door")
        bottom_door = Block("door", bottom_edge_mid, "oak_door")
        left_door = Block("door", left_edge_mid, "oak_door")
        right_door = Block("door", right_edge_mid, "oak_door")  

        world._block_list.extend(
                [top_door, bottom_door, left_door, right_door]
                )


def main():
    """
    This function accepts arguments from the command line and uses them to procedurally generate the world. The generated json is saved
    in ../external/malmo/Minecraft/run/procedural.json

    This file savng behvaior overrides the normal default of a timestamped file being generated in the current working directory.

    N is a required argument but separation and AABB size are optional. 
    """
    args = sys.argv
    if len(args) > 4 or len(args) < 2:
        print("Invalid number of arguments")
    else:
        generator = ProceduralGenerator()
        sep = 0
        AABB_size = 10  # Default values
        filename = "../external/malmo/Minecraft/run/procedural.json"

        if len(args) >= 2:
            N = (int)(args[1])
        if len(args) >= 3:
            sep = (int)(args[2])
        if len(args) == 4:
            AABB_size = (int)(args[3])

        generator.generate_grid_world(N, sep, AABB_size, filename)


if __name__ == "__main__":
    main()
