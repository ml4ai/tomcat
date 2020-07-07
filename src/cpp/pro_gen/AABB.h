/**
 * @brief This file defines the members and methods
 * implemented as part of the AABB class
 */
#pragma once

#include "Block.h"
#include <random>
#include <string>
#include <vector>

/**
 * @brief This class represents an Axis Aligned Bounding Box
 * as seen from the top-view of the Minecraft X-Z plane.
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
    /**
     * @brief Get the AABB's id
     *
     * @return int The id
     */
    int getID();

    /**
     * @brief Get the AABB's material
     *
     * @return string The material name
     */
    std::string getMaterial();

    /**
     * @brief Get the AABB type
     *
     * @return string The type
     */
    std::string getType();

    /**
     * @brief Returns a copy of the Pos object used to represent
     * the top left of the AABB from the top view of the X-Z plane
     *
     * @return Pos The copy of the top left coordinate
     */
    Pos getTopLeft();

    /**
     * @brief Returns a copy of the Pos object used to represent
     * the bottom right of the AABB from the top view of the X-Z plane
     *
     * @return Pos The copy of the bottom right coordinate
     */
    Pos getBottomRight();

    /**
     * @brief Get the block list specific to this AABB
     *
     * @return vector<Block>* The reference to the block list
     */
    std::vector<Block>* getBlockList();

    /**
     * @brief Get the midpoint X value calculated between
     * the top left and bottom right x values
     *
     * @return int The midpoint X coordinate
     */
    int getMidpointX();

    /**
     * @brief Get the midpoint Y value calculated between
     * the top left and bottom right y values
     *
     * @return int The midpoint Y coordinate
     */
    int getMidpointY();

    /**
     * @brief Get the midpoint Z value calculated between
     * the top left and bottom right z values
     *
     * @return int The midpoint Z coordinate
     */
    int getMidpointZ();

    /**
     * @brief Gets a random position in the AABB such that
     * the y coordinate of the returned value is set to
     * the top left y value which is considered the base
     *
     * @param gen The boost generation object to generate the distributions
     * @param offsetPosX How far away from the left wall should the position be.
     * Defaults to 1
     * @param offsetNegX How far away from the right wall should the position
     * be. Defaults to 1
     * @param offsetPosZ How far away from the bottom wall should the position
     * be. Defaults to 1
     * @param offsetNegZ How far away from the top wall should the position be.
     * Defaults to 1
     * @return Pos
     */
    Pos getRandomPosAtBase(std::mt19937_64* gen,
                           int offsetPosX = 1,
                           int offsetNegX = 1,
                           int offsetPosZ = 1,
                           int offsetNegZ = 1);

    /**
     * @brief Get a list of the positions of the edge midpoints for this AABB.
     * The Y value for all these Pos objects is equal to the Y value of the
     * AABB's top left field which is considered the base.
     *
     * @return vector<Pos> The list of coordinates as: top, right, bottom and
     * left edge midpoints.
     */
    std::vector<Pos> getEdgeMidpointAtBase();

    /**
     * @brief Set the top left coordinate of the AABB
     *
     * @param topLeft Pointer to the pos object top left is to be set to
     */
    void setTopLeft(Pos* topLeft);

    /**
     * @brief Set the bottom right coordinate of the AABB
     *
     * @param bottomRight Pointer to the pos object bottom right is to be set to
     */
    void setBottomRight(Pos* bottomRight);

    /**
     * @brief Add a specific block for this AABB to keep track of. Ideally this
     * should be related to the AABB. No checks are implicitly performed within
     * this method.
     *
     * @param block Pointer to the block to be added
     */
    void addBlock(Block* block);

    /**
     * @brief Checks to see if two AABBs overlapp on any of the axes
     *
     * @param other The AABB to compare to
     * @return true When the AABBs do overlap
     * @return false When the AABBs don't overlap
     */
    bool isOverlapping(AABB* other);

    /**
     * @brief Generate a box made of s specific material inside the AABB with
     * the ability to specify offsets.
     *
     * @param material The material to make this box out of
     * @param offsetPosX How far away from the left wall should the position be.
     * Defaults to 0
     * @param offsetNegX How far away from the right wall should the position
     * be. Defaults to 0
     * @param offsetPosY How far away from the left wall should the position be.
     * Defaults to 0
     * @param offsetNegY How far away from the right wall should the position
     * be. Defaults to 0
     * @param offsetPosZ How far away from the bottom wall should the position
     * be. Defaults to 0
     * @param offsetNegZ How far away from the top wall should the position be.
     * Defaults to 0
     * @param type The semantic name to give the block. Defaults to "normal".
     */
    void generateBox(std::string material,
                     int offsetPosX = 0,
                     int offsetNegX = 0,
                     int offsetPosY = 0,
                     int offsetNegY = 0,
                     int offsetPosZ = 0,
                     int offsetNegZ = 0,
                     std::string type = "normal");

    /**
     * @brief Add n random blocks of the given type and material inside the AABB
     * within the offset parameters
     *
     * @param n The number of blocks to add
     * @param material The block'smaterial type
     * @param gen THe boost generation object to generate distributions from
     * @param offsetPosX How far away from the left wall should the position be.
     * Defaults to 0
     * @param offsetNegX How far away from the right wall should the position
     * be. Defaults to 0
     * @param offsetPosY How far away from the left wall should the position be.
     * Defaults to 0
     * @param offsetNegY How far away from the right wall should the position
     * be. Defaults to 0
     * @param offsetPosZ How far away from the bottom wall should the position
     * be. Defaults to 0
     * @param offsetNegZ How far away from the top wall should the position be.
     * Defaults to 0
     * @param type The semantic name to give this block. Defaults to "normal".
     */

    void addRandomBlocks(int n,
                         std::string material,
                         std::mt19937_64* gen,
                         int offsetPosX = 0,
                         int offsetNegX = 0,
                         int offsetPosY = 0,
                         int offsetNegY = 0,
                         int offsetPosZ = 0,
                         int offsetNegZ = 0,
                         std::string type = "normal");

    /**
     * @brief Gets a string representation of the various
     * fields and values stores in an instance as a TSV.
     *
     * @return string The TSV representation
     */
    std::string toTSV();

    /**
     * @brief Construct a new AABB object
     *
     * @param id The id associated with this AABB
     * @param type A semantic name describing the type and/or purpose of the
     * AABB
     * @param material The material this AABB is built out of
     * @param topLeft The coordinates of the top left of the AABB from the
     * top view of the X-Z plane. Y coordinate should be lowest here.
     * @param bottomRight The coordinates of the bottom right of the AABB
     * from the top view of the X-Z plane. Y coordinate should be maximum here.
     * @param isHollow Specify wether the AABB should be hollow or not. Defaults
     * to true.
     * @param hasRoof specify wether the AABB should have a roof or not.
     * Defaults to false.
     */
    AABB(int id,
         std::string type,
         std::string material,
         Pos* topLeft,
         Pos* bottomRight,
         bool isHollow = true,
         bool hasRoof = false);

    /**
     * @brief Destroy the AABB object
     */
    ~AABB();
};
