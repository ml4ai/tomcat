class Block:
    """
    This class represents a Minecraft block.
    """

    def __init__(self, name, pos, material):
        """
        This is the consturctor for a block

        Args:
            name (str): The block's name, not to be confused with the material type. This is for semantic reasons only.
            pos (Pos): The position in 3D coordinates for the block
            material (str): The material used in the block
        """
        self._name = name
        self._pos = pos
        self._material = material

    def get_name(self):
        """
        This method returns the block's name

        Returns:
            str: The block's name
        """
        return self._name

    def get_material(self):
        """
        This method returns the block's material

        Returns:
            str: The block's material
        """
        return self._material

    def get_x(self):
        """
        This method returns the block's x coordinate

        Returns:
            int: The block's x coordiate
        """
        return self._pos.get_x()

    def get_y(self):
        """
        This method returns the block's y coordinate

        Returns:
            int: The block's y coordiate
        """
        return self._pos.get_y()

    def get_z(self):
        """
        This method returns the block's z coordinate

        Returns:
            int: The block's z coordiate
        """
        return self._pos.get_z()
