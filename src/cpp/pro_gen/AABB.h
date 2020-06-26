/**
 * @brief This file defines the members and methods
 * implemented as part of the AABB class
 *
 */
#pragma once

#include "Pos.h"
#include <string>

/**
 * @brief This class represents an Axis Aligned Bounding Box
 * as seen from the top-view of the Minecraft X-Z plane.
 *
 */
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
    AABB(int, std::string, Pos*, Pos*);
    ~AABB();
};
