/**
 * @brief This file defines the members and methods
 * implemented as part of the AABB class
 *
 */
#pragma once

#include "Block.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <string>
#include <vector>

/**
 * @brief This class represents an Axis Aligned Bounding Box
 * as seen from the top-view of the Minecraft X-Z plane.
 *
 */
class AABB {

  private:
    int id;
    std::string material;
    std::string type;
    Pos topLeft;
    Pos bottomRight;
    bool isHollow;
    bool hasRoof;
    std::vector<Block> blockList;

  public:
    int getID();
    std::string getMaterial();
    std::string getType();
    Pos getTopLeft();
    Pos getBottomRight();
    std::vector<Block>* getBlockList();
    int getMidpointX();
    int getMidpointY();
    int getMidpointZ();
    Pos getRandomPosAtBase(boost::random::mt19937*,
                           int offsetPosX = 1,
                           int offsetNegX = 1,
                           int offsetPosZ = 1,
                           int offsetNegZ = 1);
    std::vector<Pos> getEdgeMidpointAtBase();
    void setTopLeft(Pos*);
    void setBottomRight(Pos*);
    void addBlock(Block*);
    bool isOverlapping(AABB *);
    void generateBox(std::string,
                     int offsetPosX = 0,
                     int offsetNegX = 0,
                     int offsetPosY = 0,
                     int offsetNegY = 0,
                     int offsetPosZ = 0,
                     int offsetNegZ = 0,
                     std::string type = "normal");
    void addRandomBlocks(int,
                         std::string,
                         boost::random::mt19937*,
                         int offsetPosX = 0,
                         int offsetNegX = 0,
                         int offsetPosY = 0,
                         int offsetNegY = 0,
                         int offsetPosZ = 0,
                         int offsetNegZ = 0,
                         std::string type = "normal");
    std::string toTSV();
    AABB(int,
         std::string,
         std::string,
         Pos*,
         Pos*,
         bool isHollow = true,
         bool hasRoof = false);
    ~AABB();
};
