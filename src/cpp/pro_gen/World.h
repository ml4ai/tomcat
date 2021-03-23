/**
 * @brief This file defines the members and methods
 *        implemented as part of the World class.
 */
#pragma once

#include "AABB.h"
#include "Connection.h"
#include <memory>

/**
 * @brief This class represents a Minecraft world as a
 *        list of axis aligned bounding boxes and blocks.
 */
class World {
  private:
    std::mt19937_64 gen;
    std::vector<std::unique_ptr<AABB>> aabbList;
    std::vector<std::unique_ptr<Block>> blockList;
    std::vector<std::unique_ptr<Entity>> entityList;
    std::vector<std::unique_ptr<Object>> objectList;
    std::vector<std::unique_ptr<Connection>> connectionList;

  public:
    /**
     * @brief Returns the vector that holds the AABBs. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return vector<AABB*>& The AABB list.
     */
    std::vector<std::unique_ptr<AABB>>& getAABBList();

    /**
     * @brief Returns the vector that holds the Blocks. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return vector<Block*>& The Block list.
     */
    std::vector<std::unique_ptr<Block>>& getBlockList();

    /**
     * @brief Returns the Entity vector for this World. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return std::vector<Entity*>&  The Entity list.
     */
    std::vector<std::unique_ptr<Entity>>& getEntityList();

    /**
     * @brief Returns the Object vector for this World. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return std::vector<Object*>&  The object list.
     */
    std::vector<std::unique_ptr<Object>>& getObjectList();

    /**
     * @brief Returns the Connection vector for this World. Do not transfer
     *        ownership  of any unique_ptr as it may cause scope issues.
     *
     * @return std::vector<Connection*>&  The connection list.
     */
    std::vector<std::unique_ptr<Connection>>& getConnectionList();

    /**
     * @brief Add an AABB to the vector of AABB held inside the world.
     *
     * @param aabb The AABB to add.
     */
    void addAABB(std::unique_ptr<AABB> aabb);

    /**
     * @brief Add an Entity for this world to keep track of.
     *
     * @param entity The Entity object.
     */
    void addEntity(std::unique_ptr<Entity> entity);

    /**
     * @brief Add a Block to the vector of Block held inside the world.
     *
     * @param block The Block to add.
     */
    void addBlock(std::unique_ptr<Block> block);

    /**
     * @brief Add an object to the vector of object held inside the world.
     *
     * @param object The object to add.
     */
    void addObject(std::unique_ptr<Object> object);

    /**
     * @brief Add an connection to the vector of connection held inside the
     *        world.
     *
     * @param connection The connection to add.
     */
    void addConnection(std::unique_ptr<Connection> connection);

    /**
     * @brief Converts the world into its alternate JSON representation with
     *        each entry indented by 4 and returns the string representation of
     * it.
     *
     * @return string The JSON as a string.
     */
    std::string virtual toLowLevelMapJSON();

    /**
     * @brief Converts the world into a JSON representation with
     *        each entry indented by 4 and returns the string representation of
     * it.
     *
     * @return string The JSON as a string.
     */
    std::string virtual toSemanticMapJSON();

    /**
     * @brief Writes the world's JSON and alternate JSON output to the given
     *        filepaths.
     *
     * @param jsonPath Path to store json.
     * @param lowLevelMapJSONPath Path to store the alternate json
     *        representation.
     */
    void writeToFile(std::string jsonPath, std::string lowLevelMapJSONPath);

    /**
     * @brief Construct a new World object.
     */
    World();

    /**
     * @brief Get the random object used by this generator.
     *
     * @return std::mt19937_64& The random object.
     */
    std::mt19937_64& getRandom();

    /**
     * @brief Set the seed the random object used by this class is
     *        initialized with.
     *
     * @param seed The seed to use.
     */
    void setRandom(int seed);

    /**
     * @brief Destroy the World object
     */
    virtual ~World();
};
