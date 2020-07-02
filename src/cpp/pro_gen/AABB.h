/**
 * @brief This file defines the members and methods
 * implemented as part of the AABB class
 *
 */
#pragma once

#include "Block.h"
#include <string>
#include <vector>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

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
    bool isHollow;
    bool hasRoof;
    std::vector<Block> blockList;

  public:
    int getID();
    std::string getMaterial();
    Pos getTopLeft();
    Pos getBottomRight();
    int getMidpointX();
    int getMidpointY();
    int getMidpointZ();
    Pos getRandomPosAtBase(boost::random::mt19937*, int, int, int, int);
    void addBlock(Block *);
    std::string toTSV();
    AABB(int, std::string, Pos*, Pos*, bool, bool);
    ~AABB();
};
