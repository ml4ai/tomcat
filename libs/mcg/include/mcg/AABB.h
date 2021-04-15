/**
 * @brief This file defines the members and methods
 *        implemented as part of the AABB class
 */
#pragma once

#include "Block.h"
#include "Connection.h"
#include "Door.h"
#include "Entity.h"
#include "Object.h"
#include <memory>
#include <random>
#include <vector>

/**
 * @brief This class represents an Axis Aligned Bounding Box
 *        as seen from the top-view of the Minecraft X-Z plane.
 */
class AABB {

  protected:
    std::string id;
    std::string material;
    std::string type;
    Pos topLeft;
    Pos bottomRight;
    bool isHollow;
    bool hasRoof;
    bool autoAdjust;
    std::vector<std::unique_ptr<Block>> blockList;
    std::vector<std::unique_ptr<Entity>> entityList;
    std::vector<std::unique_ptr<Object>> objectList;
    std::vector<std::unique_ptr<AABB>> aabbList;
    std::vector<std::unique_ptr<Connection>> connectionList;

    void recalculateOverallBoundary();

  public:
    /**
     * @brief Get the AABB's id.
     *
     * @return string The id.
     */
    std::string getID();

    /**
     * @brief Get the AABB's material.
     *
     * @return string The material name.
     */
    std::string getMaterial();

    /**
     * @brief Get the AABB type.
     *
     * @return string The type.
     */
    std::string getType();

    /**
     * @brief Returns a copy of the Pos object used to represent
     *        the top left of the AABB from the top view of the X-Z plane.
     *
     * @return Pos The copy of the top left coordinate.
     */
    Pos getTopLeft();

    /**
     * @brief Returns a copy of the Pos object used to represent
     *        the bottom right of the AABB from the top view of the X-Z plane.
     *
     * @return Pos The copy of the bottom right coordinate.
     */
    Pos getBottomRight();

    /**
     * @brief Get the block list specific to this AABB. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return The reference to the block list.
     */
    std::vector<std::unique_ptr<Block>>& getBlockList();

    /**
     * @brief Get the entity list specific to this AABB. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return The reference to the entity list.
     */
    std::vector<std::unique_ptr<Entity>>& getEntityList();

    /**
     * @brief Get the object list specific to this AABB. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return The reference to the object list.
     */
    std::vector<std::unique_ptr<Object>>& getObjectList();

    /**
     * @brief Gets the list of AABBs this AABB is the parent of. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return std::vector<AABB*>& Reference to the list of children AABBs.
     */
    std::vector<std::unique_ptr<AABB>>& getAABBList();

    /**
     * @brief Returns the Connection vector for this AABB. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return std::vector<Connection*>&  The connection list.
     */
    std::vector<std::unique_ptr<Connection>>& getConnectionList();

    /**
     * @brief Get the midpoint X value calculated between
     *        the top left and bottom right x values.
     *
     * @return int The midpoint X coordinate.
     */
    int getMidpointX();

    /**
     * @brief Get the midpoint Y value calculated between
     *        the top left and bottom right y values
     *
     * @return int The midpoint Y coordinate
     */
    int getMidpointY();

    /**
     * @brief Get the midpoint Z value calculated between
     *        the top left and bottom right z values
     *
     * @return int The midpoint Z coordinate
     */
    int getMidpointZ();

    /**
     * @brief Get the difference of the extreme x coordinates for this AABB
     *
     * @return int The difference
     */
    int getSizeX();

    /**
     * @brief Get the difference of the extreme y coordinates for this AABB
     *
     * @return int The difference
     */
    int getSizeY();

    /**
     * @brief Get the difference of the extreme z coordinates for this AABB
     *
     * @return int The difference
     */
    int getSizeZ();

    /**
     * @brief Get a random position in the AABB within the given offsets
     *
     * @param gen THe boost generation object to generate distributions from
     * @param offsetPosX How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegX How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosY How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegY How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosZ How far away from the bottom wall should the position
     *        be. Defaults to 0
     * @param offsetNegZ How far away from the top wall should the position be.
     *        Defaults to 0
     * @return Pos The random position
     */
    Pos virtual getRandomPos(std::mt19937_64& gen,
                             int offsetPosX = 0,
                             int offsetNegX = 0,
                             int offsetPosY = 0,
                             int offsetNegY = 0,
                             int offsetPosZ = 0,
                             int offsetNegZ = 0);

    /**
     * @brief Get a list of the positions of the edge midpoints for this AABB.
     *        The Y value for all these Pos objects is equal to the Y value of
     * the AABB's top left field which is considered the base.
     *
     * @return vector<Pos> The list of coordinates as: top, right, bottom and
     *         left edge midpoints.
     */
    std::vector<Pos> virtual getEdgeMidpointAtBase();

    /**
     * @brief Get a particular AABB contained by this AABB. The AABB can be
     *        identified by its ID. A unique pointer already
     *        owns this, so do not assign new ownership.
     *
     * @param id The id of the AABB to find.
     * @return AABB* Pointer to the relevant AABB or nullptr if it doesn't
     *         exist.
     */
    AABB* getSubAABB(std::string id);

    /**
     * @brief Set the top left coordinate of the AABB
     *
     * @param topLeft Pos object top left is to be set to
     */
    void setTopLeft(Pos& topLeft);

    /**
     * @brief Set the bottom right coordinate of the AABB
     *
     * @param bottomRight Pos object bottom right is to be set to
     */
    void setBottomRight(Pos& bottomRight);

    /**
     * @brief Set the base building material
     *
     * @param material  The base material
     */
    void setMaterial(std::string material);

    /**
     * @brief Add a specific block for this AABB to keep track of. Ideally this
     *        should be related to the AABB. No checks are implicitly performed
     *        within this method.
     *
     * @param block Block to be added
     */
    void addBlock(std::unique_ptr<Block> block);

    /**
     * @brief Add a specific entity for this AABB to keep track of. Ideally this
     *        should be related to the AABB. No checks are implicitly performed
     *        within this method.
     *
     * @param entity Entity to be added
     */
    void addEntity(std::unique_ptr<Entity> entity);

    /**
     * @brief Add a specific object for this AABB to keep track of. Ideally this
     *        should be related to the AABB. No checks are implicitly performed
     * within this method.
     *
     * @param object Object to be added
     */
    void addObject(std::unique_ptr<Object> object);

    /**
     * @brief Adds an AABB that will be part of this AABB's child list.
     *
     * @param aabb The AABB to add.
     */
    void addAABB(std::unique_ptr<AABB> aabb);

    /**
     * @brief Add an connection to the vector of connection held inside the
     *        aabb.
     *
     * @param connection The connection to add.
     */
    void addConnection(std::unique_ptr<Connection> connection);

    /**
     * @brief Checks to see if two AABBs overlap with each other (including
     * the case where one AABB is fully contained within the other.
     *
     * @param other The AABB to compare to
     * @return true When the AABBs do overlap
     * @return false When the AABBs don't overlap
     */
    bool virtual intersects(AABB& other);

    /**
     * @brief Generate a box made of a specific material inside the AABB with
     *        the ability to specify offsets.
     *
     * @param material The material to make this box out of
     * @param offsetPosX How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegX How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosY How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegY How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosZ How far away from the bottom wall should the position
     *        be. Defaults to 0
     * @param offsetNegZ How far away from the top wall should the position be.
     *        Defaults to 0
     */
    void virtual generateBox(std::string material,
                             int offsetPosX = 0,
                             int offsetNegX = 0,
                             int offsetPosY = 0,
                             int offsetNegY = 0,
                             int offsetPosZ = 0,
                             int offsetNegZ = 0);

    /**
     * @brief Add n random blocks of the given type and material inside the AABB
     *        within the offset parameters
     *
     * @param n The number of blocks to add
     * @param material The block's material type
     * @param gen THe boost generation object to generate distributions from
     * @param offsetPosX How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegX How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosY How far away from the left wall should the position be.
     *        Defaults to 0
     * @param offsetNegY How far away from the right wall should the position
     *        be. Defaults to 0
     * @param offsetPosZ How far away from the bottom wall should the position
     *        be. Defaults to 0
     * @param offsetNegZ How far away from the top wall should the position be.
     *        Defaults to 0
     */
    void virtual addRandomBlocks(int n,
                                 std::string material,
                                 std::mt19937_64& gen,
                                 int offsetPosX = 0,
                                 int offsetNegX = 0,
                                 int offsetPosY = 0,
                                 int offsetNegY = 0,
                                 int offsetPosZ = 0,
                                 int offsetNegZ = 0);

    /**
     * @brief Generate 4 doors for the AABB at the midpoint.
     */
    void virtual generateAllDoorsInAABB();

    /**
     * @brief Translate the AABB by the given amount in the X axis
     * 
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftX(int shift);

        /**
     * @brief Translate the AABB by the given amount in the Y axis
     * 
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftY(int shift);

        /**
     * @brief Translate the AABB by the given amount in the Z axis
     * 
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftZ(int shift);

    /**
     * @brief Translates the AABB by the given amounts in the X, Y and Z axes
     * 
     * @param shiftX The amount to shift by in X which may be positive or negative
     * @param shiftY The amount to shift by in Y which may be positive or negative
     * @param shiftZ The amount to shift by in Z which may be positive or negative
     */
    void shift(int shiftX, int shiftY, int shiftZ);

    /**
     * @brief Adds the JSON representation of this object to the
     *        "locations" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the alternate block by block JSON representation of this
     *        object to the "blocks" list of the base json.
     *
     * @return nlohmann::json The base json
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new AABB object
     *
     * @param id The id associated with this AABB
     * @param type A semantic name describing the type and/or purpose of the
     *        AABB
     * @param material The material this AABB is built out of
     * @param topLeft The coordinates of the top left of the AABB from the
     *        top view of the X-Z plane. Y coordinate should be lowest here.
     * @param bottomRight The coordinates of the bottom right of the AABB
     *        from the top view of the X-Z plane. Y coordinate should be maximum
     *        here.
     * @param isHollow Specify whether the AABB should be hollow or not.
     *        Defaults to true.
     * @param hasRoof specify whether the AABB should have a roof or not.
     *        Defaults to false.
     * @param autoAdjust Whether or not you want the boundary to auto adjust to
     *        its children. Defaults to true.
     */
    AABB(std::string id,
         std::string type,
         std::string material,
         Pos& topLeft,
         Pos& bottomRight,
         bool isHollow = true,
         bool hasRoof = false,
         bool autoAdjust = true);

    /**
     * @brief Construct a new AABB object. Use this contructor when you know the
     *        top left and bottom right positions will change in the future.
     *        Here, since the autoAdjust defaults to true and the base material is
     *        "blank", it is effectively a blank canvas. It will resize when children
     *        AABB are added to it, and since it's base material is blank, the
     *        WorldBuilder java class won't place anything at locations marked with a
     *        material of type "blank." All of this can of course be achieved with the
     *        other constructor, but this one makes the process more convenient.
     *
     * @param id The id associated with this AABB
     */
    AABB(std::string id);

    /**
     * @brief Destroy the AABB object
     */
    virtual ~AABB();
};
