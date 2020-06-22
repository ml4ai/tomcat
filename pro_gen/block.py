class Block:

    def __init__(self, name, pos, material):
        self._name = name
        self._pos = pos
        self._material = material
    
    def get_name(self):
        return self._name
    
    def get_material(self):
        return self._material
    
    def get_x(self):
        return self._pos.get_x()
    
    def get_y(self):
        return self._pos.get_y()
    
    def get_z(self):
        return self._pos.get_z()