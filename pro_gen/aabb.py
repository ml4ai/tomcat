import copy


class AABB:

    def __init__(self, aabb_id, top_left, bottom_right, material="wood"):
        """
        Constructor to initialize the AABB.

        Args:
            aabb_id (int): The id associated with this AABB
            top_left (Pos): The 3D coordinates for the top left of the AABB from the topview. It is on the X-Z plane.
            bottom_right (Pos): The 3D coordinates for the top left of the AABB from the top view. It is on the X-Z plane.
            material (str, optional): The name of the material to build this AABB out of. Defaults to "wood".
        """
        self._aabb_id = aabb_id
        self._top_left = top_left
        self._bottom_right = bottom_right
        self._material = material.lower()

    def get_id(self):
        """
        This method returns the id associated with the AABB.

        Returns:
            int: The AABB id.
        """
        return self._aabb_id

    def get_top_left(self):
        """
        Returns a copy of the Pos object for the top left.

        Returns:
            Pos: Top left coordinates. It is a copy of the Pos object so aliasing isn't an issue.
        """
        return copy.deepcopy(self._top_left)

    def get_bottom_right(self):
        """
        Returns a copy of the Pos object for the bottom right.

        Returns:
            Pos: Bottom right coordinates. It is a copy of the Pos object so aliasing isn't an issue.
        """
        return copy.deepcopy(self._bottom_right)

    def get_material(self):
        """
        Returns the material the AABB is build out of

        Returns:
            str: The material the AABB is built of.
        """
        return self._material

    def __str__(self):
        """
        Returns a string representation of the AABB object which looks like:

        id: (x1,y1,z1) (x2,y2,z2) material

        where x1,y1.z1 are the coordinates of the top left and x2,y2,z2 are coordinates for the bottom right.

        Returns:
            str: The string representation
        """
        return "{}: {} {} {}".format(
            self._aabb_id, str(
                self._top_left), str(
                self._bottom_right), self._material)

    def __eq__(self, other):
        """
        Checks if two AABB are equal. They are considered equal if they have the same id or if their top left or bottom right coordinates match. This is done to
        prevent exact overlaps between AABB. (WIP)

        Args:
            other (AABB): The other AABB

        Returns:
            boolean: True or False if equal or not
        """
        if isinstance(other, AABB):
            return self.get_id() == other.get_id() or self.get_top_left(
            ) == other.get_top_left() or self.get_bottom_right() == other.get_bottom_right()
        else:
            return False

    def __hash__(self):
        """
        It hashes to the id. TODO: Use the top left coordinates in hash generation here before creating graph representation.

        Returns:
            int: The AABB id
        """
        return self.get_id()
