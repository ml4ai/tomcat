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

    def to_JSON(self, filename=None):
        """
        Creates a JSON from the World object where the the aabb_list and block_list id are a list of dictionaries for each aabb and block.
        An example of the data:

        {
            "aabb_list": [
                {
                    "id": "1",
                    "x1": "0",
                    "y1": "0",
                    "z1": "0",
                    "x2": "10",
                    "y2": "10",
                    "z2": "10",
                    "material": "wood"
                }, for every AABB
            ],
            "block_list": [
                {
                    "name": "door",
                    "x": 5,
                    "y": 0,
                    "z": 0,
                    "material": "wood"
                }, for every Block
            ]
        }

        The x1,y1,z1 and x2,y2,z2 in each aab are the top left and bottom right coordinates from a top view of the X-Z plane. 
        The x,y,z in a block is simply the block's coordinates.

        Args:
            filename (str, optional): Name of the file to store output the JSON to. If nothing is given the filename will be:
                                      grid_world_mm_dd_yyyy_hh_mm_ss.json in current directory
        """

        # Add output to a dictionary
        output_dict = dict()

        output_dict["aabb_list"] = list()
        output_dict["block_list"] = list()

        self._add_AABB_dict_to_output(output_dict)
        self._add_blocks_dict_to_output(output_dict)

        # Write the output to file
        if filename is None:
            now = datetime.now()
            filename = (
                "grid_world"
                + str(now.strftime("_%d_%m_%Y_%H_%M_%S"))
                + ".json"
            )

        with open(filename, "w") as file_out:
            json.dump(output_dict, file_out, indent=4)

    def _add_AABB_dict_to_output(self, output_dict):
        """
        This function adds all the AABBs the world knows about as dictionary objects to the output dictionary's
        aabb_list key entry.

        Args:
            output_dict (dict): The main dictionary to write to JSON
        """
        for aabb in self._aabb_list:
            cur_aabb = dict()
            cur_aabb["id"] = str(aabb.get_id())

            cur_aabb["x1"] = str(aabb.get_top_left().get_x())
            cur_aabb["y1"] = str(aabb.get_top_left().get_y())
            cur_aabb["z1"] = str(aabb.get_top_left().get_z())

            cur_aabb["x2"] = str(aabb.get_bottom_right().get_x())
            cur_aabb["y2"] = str(aabb.get_bottom_right().get_y())
            cur_aabb["z2"] = str(aabb.get_bottom_right().get_z())

            cur_aabb["material"] = aabb.get_material()

            output_dict["aabb_list"].append(cur_aabb)

    def _add_blocks_dict_to_output(self, output_dict):
        """
        This function adds all the blocks the world knows about as dictionary objects to the output dictionary's
        aabb_list key entry.

        Args:
            output_dict (dict): The main dictionary to write to JSON
        """
        for block in self._block_list:
            cur_block = dict()
            cur_block["name"] = block.get_name()

            cur_block["x"] = str(block.get_x())
            cur_block["y"] = str(block.get_y())
            cur_block["z"] = str(block.get_z())

            cur_block["material"] = block.get_material()

            output_dict["block_list"].append(cur_block)
