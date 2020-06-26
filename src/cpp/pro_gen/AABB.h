#pragma once

#include "Pos.h"
#include <string>

class AABB {

  private:
    int id;
    std::string material;
    Pos topLeft;
    Pos bottomRight;

  public:
    int getID();
    std::string getMaterial();
    Pos getTopLeft();
    Pos getBottomRight();
    int getMidpointX();
    int getMidpointY();
    int getMidpointZ();
    Pos getRandomPosAtBase(int, int, int, int);
    std::string toString();
    AABB(int, std::string, Pos *, Pos *);
    ~AABB();
};
