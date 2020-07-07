/**
 * @file AABB.cpp
 * @brief This file implements the methods in the AABB class.
 */
#include "AABB.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <string>

using namespace std;

/**
 * @brief Construct a new AABB::AABB object
 *
 * @param AABBid The id associated with this AABB
 * @param type A semantic name describing the type and/or purpose of the AABB
 * @param AABBmaterial The material this AABB is built out of
 * @param topLeftPos The coordinates of the top left of the AABB from the top
 * view of the X-Z plane. Y coordinate should be lowest here.
 * @param bottomRightPos The coordinates of the bottom right of the AABB from
 * the top view of the X-Z plane. Y coordinate should be maximum here.
 */
AABB::AABB(int id,
           string type,
           string material,
           Pos* topLeft,
           Pos* bottomRight,
           bool isHollow,
           bool hasRoof)
    : id{id}, type{type}, material{material}, topLeft{*topLeft},
      bottomRight{*bottomRight}, isHollow{isHollow}, hasRoof{hasRoof} {}

/**
 * @brief Get the AABB's id
 *
 * @return int The id
 */
int AABB::getID() { return this->id; }

/**
 * @brief Get the AABB's material
 *
 * @return string The material name
 */
string AABB::getMaterial() { return this->material; }

/**
 * @brief Get the AABB type
 *
 * @return string The type
 */
string AABB::getType() { return this->type; }

/**
 * @brief Returns a copy of the Pos object used to represent
 * the top left of the AABB from the top view of the X-Z plane
 *
 * @return Pos The copy of the top left coordinate
 */
Pos AABB::getTopLeft() { return this->topLeft; }

/**
 * @brief Returns a copy of the Pos object used to represent
 * the bottom right of the AABB from the top view of the X-Z plane
 *
 * @return Pos The copy of the bottom right coordinate
 */
Pos AABB::getBottomRight() { return this->bottomRight; }

/**
 * @brief Set the top left coordinate of the AABB
 *
 * @param topLeft Pointer to the pos object top left is to be set to
 */
void AABB::setTopLeft(Pos* topLeft) { this->topLeft = *topLeft; }

/**
 * @brief Set the bottom right coordinate of the AABB
 *
 * @param topLeft Pointer to the pos object bottom right is to be set to
 */
void AABB::setBottomRight(Pos* bottomRight) {
    this->bottomRight = *bottomRight;
}

/**
 * @brief Get the midpoint X value calculated between
 * the top left and bottom right x values
 *
 * @return int The midpoint X coordinate
 */
int AABB::getMidpointX() {
    int mid_x = ((this->topLeft).getX() +
                 ((this->bottomRight).getX() - (this->topLeft).getX()) / 2);
    return mid_x;
}

/**
 * @brief Get the midpoint Y value calculated between
 * the top left and bottom right y values
 *
 * @return int The midpoint Y coordinate
 */
int AABB::getMidpointY() {
    int mid_y = ((this->topLeft).getY() +
                 ((this->bottomRight).getY() - (this->topLeft).getY()) / 2);
    return mid_y;
}

/**
 * @brief Get the midpoint Z value calculated between
 * the top left and bottom right z values
 *
 * @return int The midpoint Z coordinate
 */
int AABB::getMidpointZ() {
    int mid_z = ((this->topLeft).getZ() +
                 ((this->bottomRight).getZ() - (this->topLeft).getZ()) / 2);
    return mid_z;
}

/**
 * @brief Checks to see if two AABBs overlapp on any of the axes
 *
 * @param other The AABB to compare to
 * @return true When the AABBs do overlap
 * @return false When the AABBs don't overlap
 */
bool AABB::isOverlapping(AABB* other) {
    int xRange = (this->bottomRight.getX()) - (this->topLeft.getX());
    int yRange = (this->bottomRight.getY()) - (this->topLeft.getY());
    int zRange = (this->bottomRight.getZ()) - (this->topLeft.getZ());

    if ((abs(other->topLeft.getX() - this->topLeft.getX()) < xRange) ||
        (abs(other->topLeft.getY() - this->topLeft.getY()) < yRange) ||
        (abs(other->topLeft.getZ() - this->topLeft.getZ()) < zRange)) {

        return true;
    }
    else {
        return false;
    }
}

/**
 * @brief Gets a random position in the AABB such that
 * the y coordinate of the returned value is set to
 * the top left y value which is considered the base
 *
 * @param gen The boost generation object to generate the distributions
 * @param offsetPosX How far away from the left wall should the position be.
 * Defaults to 1
 * @param offsetNegX How far away from the right wall should the position be.
 * Defaults to 1
 * @param offsetPosZ How far away from the bottom wall should the position be.
 * Defaults to 1
 * @param offsetNegZ How far away from the top wall should the position be.
 * Defaults to 1
 * @return Pos
 */
Pos AABB::getRandomPosAtBase(boost::random::mt19937* gen,
                             int offsetPosX,
                             int offsetNegX,
                             int offsetPosZ,
                             int offsetNegZ) {

    int startX = (this->topLeft).getX() + offsetPosX;
    int startZ = (this->topLeft).getZ() + offsetPosZ;

    int endX = (this->bottomRight).getX() - offsetNegX;
    int endZ = (this->bottomRight).getZ() - offsetNegZ;

    boost::random::uniform_int_distribution<> randXGen(startX, endX);
    boost::random::uniform_int_distribution<> randZGen(startZ, endZ);

    int randX = randXGen(*gen);
    int randZ = randZGen(*gen);

    int base = (this->topLeft).getY();
    Pos pos(randX, base, randZ);

    return pos;
}

/**
 * @brief Get the block list specific to this AABB
 *
 * @return vector<Block>* The reference to the block list
 */
vector<Block>* AABB::getBlockList() { return &(this->blockList); }

/**
 * @brief Add a specific block for this AABB to keep track of. Ideally this
 * should be related to the AABB. No checks are implicitly performed within this
 * method.
 *
 * @param block Pointer to the block to be added
 */
void AABB::addBlock(Block* block) { (this->blockList).push_back(*block); }

/**
 * @brief Get a list of the positions of the edge midpoints for this AABB. The Y
 * value for all these Pos objects is equal to the Y value of the AABB's top
 * left field which is considered the base.
 *
 * @return vector<Pos> The list of coordinates as: top, right, bottom and left
 * edge midpoints.
 */
vector<Pos> AABB::getEdgeMidpointAtBase() {
    int midX = this->getMidpointX();
    int midZ = this->getMidpointZ();
    int base = this->getTopLeft().getY();

    Pos topEdgeMid(this->getTopLeft());
    topEdgeMid.setX(midX);
    topEdgeMid.setY(base);

    Pos bottomEdgeMid(this->getBottomRight());
    bottomEdgeMid.setX(midX);
    bottomEdgeMid.setY(base);

    Pos leftEdgeMid(this->getTopLeft());
    leftEdgeMid.setZ(midZ);
    leftEdgeMid.setY(base);

    Pos rightEdgeMid(this->getBottomRight());
    rightEdgeMid.setZ(midZ);
    rightEdgeMid.setY(base);

    vector<Pos> midEdgesAtBase;
    midEdgesAtBase.push_back(topEdgeMid);
    midEdgesAtBase.push_back(rightEdgeMid);
    midEdgesAtBase.push_back(bottomEdgeMid);
    midEdgesAtBase.push_back(leftEdgeMid);

    return midEdgesAtBase;
}

/**
 * @brief Generate a box made of s specific material inside the AABB with the
 * ability to specify offsets.
 *
 * @param type The semantic name to give the block
 * @param material The material to make this box out of
 * @param offsetPosX How far away from the left wall should the position be.
 * Defaults to 0
 * @param offsetNegX How far away from the right wall should the position be.
 * Defaults to 0
 * @param offsetPosY How far away from the left wall should the position be.
 * Defaults to 0
 * @param offsetNegY How far away from the right wall should the position be.
 * Defaults to 0
 * @param offsetPosZ How far away from the bottom wall should the position be.
 * Defaults to 0
 * @param offsetNegZ How far away from the top wall should the position be.
 * Defaults to 0
 */
void AABB::generateBox(string material,
                       int offsetPosX,
                       int offsetNegX,
                       int offsetPosY,
                       int offsetNegY,
                       int offsetPosZ,
                       int offsetNegZ,
                       string type) {

    int startX = this->getTopLeft().getX() + offsetPosX;
    int startY = this->getTopLeft().getY() + offsetPosY;
    int startZ = this->getTopLeft().getZ() + offsetPosZ;

    int endX = this->getBottomRight().getX() - offsetNegX;
    int endY = this->getBottomRight().getY() - offsetNegY;
    int endZ = this->getBottomRight().getZ() - offsetNegZ;

    for (int x = startX; x <= endX; x++) {
        for (int y = startY; y <= endY; y++) {

            for (int z = startZ; z <= endZ; z++) {
                Pos pos(x, y, z);
                Block block(material, &pos, type);
                this->addBlock(&block);
            }
        }
    }
}

/**
 * @brief Add n random blocks of the given type and material inside the AABB
 * within the offset parameters
 *
 * @param n The number of blocks to add
 * @param type The semantic name to give this block
 * @param material The block'smaterial type
 * @param gen THe boost generation object to generate distributions from
 * @param offsetPosX How far away from the left wall should the position be.
 * Defaults to 0
 * @param offsetNegX How far away from the right wall should the position be.
 * Defaults to 0
 * @param offsetPosY How far away from the left wall should the position be.
 * Defaults to 0
 * @param offsetNegY How far away from the right wall should the position be.
 * Defaults to 0
 * @param offsetPosZ How far away from the bottom wall should the position be.
 * Defaults to 0
 * @param offsetNegZ How far away from the top wall should the position be.
 * Defaults to 0
 */
void AABB::addRandomBlocks(int n,
                           string material,
                           boost::random::mt19937* gen,
                           int offsetPosX,
                           int offsetNegX,
                           int offsetPosY,
                           int offsetNegY,
                           int offsetPosZ,
                           int offsetNegZ,
                           string type) {

    int startX = this->getTopLeft().getX() + offsetPosX;
    int startY = this->getTopLeft().getY() + offsetPosY;
    int startZ = this->getTopLeft().getZ() + offsetPosZ;

    int endX = this->getBottomRight().getX() - offsetNegX;
    int endY = this->getBottomRight().getY() - offsetNegY;
    int endZ = this->getBottomRight().getZ() - offsetNegZ;

    while (n > 0) {
        boost::random::uniform_int_distribution<> randX(startX, endX);
        boost::random::uniform_int_distribution<> randY(startY, endY);
        boost::random::uniform_int_distribution<> randZ(startZ, endZ);

        int x = randX(*gen);
        int y = randY(*gen);
        int z = randZ(*gen);
        Pos pos(x, y, z);
        Block block(material, &pos, type);
        this->addBlock(&block);
        n--;
    }
}

/**
 * @brief Gets a string representation of the various
 * fields and values stores in an instance as a TSV.
 *
 * @return string The TSV representation
 */
string AABB::toTSV() {
    string retval = "";
    for (int x = (this->topLeft).getX(); x <= (this->bottomRight).getX(); x++) {
        for (int y = (this->topLeft).getY(); y <= (this->bottomRight).getY();
             y++) {
            for (int z = (this->topLeft).getZ();
                 z <= (this->bottomRight).getZ();
                 z++) {
                bool addCurrent = true;

                if (isHollow) {
                    if (x > (this->topLeft).getX() &&
                        x < (this->bottomRight).getX() &&
                        z > (this->topLeft).getZ() &&
                        z < (this->bottomRight).getZ()) {
                        if (y != (this->topLeft).getY() &&
                            y != (this->bottomRight).getY()) {
                            addCurrent = false;
                        }
                    }
                }

                if (!hasRoof) {
                    if (y == (this->bottomRight).getY()) {
                        addCurrent = false;
                    }
                }

                if (addCurrent) {
                    retval += to_string(x) + "\t" + to_string(y) + "\t" +
                              to_string(z) + "\t" + (this->material) + "\n";
                }
            }
        }
    }

    for (auto block : (this->blockList)) {
        retval += block.toTSV() + "\n";
    }

    return retval;
}

/**
 * @brief Destroy the AABB::AABB object
 */
AABB::~AABB() {}
