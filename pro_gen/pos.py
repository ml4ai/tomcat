class Pos:
    """
    This class represents a #D coordinate.
    """

    def __init__(self, x, y, z):
        """
        Default constructor requiree x,y,z values

        Args:
            x (int): X coordinate
            y (itn ): Y coordinate
            z (int ): Z coordinate
        """
        self._x = x
        self._y = y
        self._z = z

    def get_x(self):
        return self._x

    def get_y(self):
        return self._y

    def get_z(self):
        return self._z

    def set_x(self, x):
        self._x = x

    def set_y(self, y):
        self._y = y

    def set_z(self, z):
        self._z = z

    def __str__(self):
        """
        Retruns the string representation of this object which looks like: (x,y,z)

        Returns:
            str: The string representation
        """
        return "({},{},{})".format(self._x, self._y, self._z)
