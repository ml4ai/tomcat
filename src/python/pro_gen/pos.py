class Pos:
    """
    This class represents a 3D coordinate.
    """

    def __init__(self, x, y, z):
        """
        Default constructor requiree x,y,z values

        Args:
            x (int): X coordinate
            y (int): Y coordinate
            z (int): Z coordinate
        """
        self._x = x
        self._y = y
        self._z = z

    def get_x(self):
        """
        This method returns the x value of this object.

        Returns:
            int: The x coordinate value
        """
        return self._x

    def get_y(self):
        """
        This method returns the y value of this object.

        Returns:
            int: The y coordinate value
        """
        return self._y

    def get_z(self):
        """
        This method returns the z value of this object.

        Returns:
            int: The z coordinate value
        """
        return self._z

    def set_x(self, x):
        """
        This method sets the x coordinate of this object

        Args:
            x (int): x coordinate value
        """
        self._x = x

    def set_y(self, y):
        """
        This method sets the x coordinate of this object

        Args:
            x (int): x coordinate value
        """
        self._y = y

    def set_z(self, z):
        """
        This method sets the x coordinate of this object

        Args:
            x (int): x coordinate value
        """
        self._z = z

    def __str__(self):
        """
        Retruns the string representation of this object which looks like: (x,y,z)

        Returns:
            str: The string representation
        """
        return "({},{},{})".format(self._x, self._y, self._z)
