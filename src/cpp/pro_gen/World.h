/**
 * @brief This file defines the members and methods
 * implemented as part of the World class
 */
#pragma once

#include "AABB.h"
#include "Group.h"
#include "Connection.h"

/**
 * @brief This class represents a Minecraft world as a
 * list of axis aligned bounding boxes and blocks.
 */
class World {
  private:
    std::mt19937_64 gen;
    std::vector<AABB*> aabbList;
    std::vector<Block*> blockList;
    std::vector<Entity*> entityList;
    std::vector<Object*> objectList;
    std::vector<Connection*> connectionList;

  public:
    /**
     * @brief Returns the vector that holds the AABBs
     *
     * @return vector<AABB*>& The AABB list
     */
    std::vector<AABB*>& getAABBList();

    /**
     * @brief Returns the vector that holds the Blocks
     *
     * @return vector<Block*>& The Block list
     */
    std::vector<Block*>& getBlockList();

    /**
     * @brief Returns the Entity vector for this World
     *
     * @return std::vector<Entity*>&  The Entity list
     */
    std::vector<Entity*>& getEntityList();

    /**
     * @brief Returns the Object vector for this World
     *
     * @return std::vector<Object*>&  The object list
     */
    std::vector<Object*>& getObjectList();

    /**
     * @brief Returns the Connection vector for this World
     *
     * @return std::vector<Connection*>&  The connection list
     */
    std::vector<Connection*>& getConnectionList();

    /**
     * @brief Add an AABB to the vector of AABB held inside the world
     *
     * @param aabb The AABB to add
     */
    void addAABB(AABB& aabb);

    /**
     * @brief Add an Entity for this world to keep track of
     *
     * @param entity The Entity object
     */
    void addEntity(Entity& entity);

    /**
     * @brief Add a Block to the vector of Block held inside the world
     *
     * @param block The Block to add
     */
    void addBlock(Block& block);

    /**
     * @brief Add an object to the vector of object held inside the world
     *
     * @param object The object to add
     */
    void addObject(Object& object);

    /**
     * @brief Add an connection to the vector of connection held inside the world
     *
     * @param connection The connection to add
     */
    void addConnection(Connection& connection);

    /**
     * @brief Converts the world into its alternate JSON representation with
     * each entry indented by 4 and returns the string representation of it.
     *
     * @return string The JSON as a string
     */
    std::string virtual toAltJSON();

    /**
     * @brief Converts the world into a JSON representation with
     * each entry indented by 4 and returns the string representation of it.
     *
     * @return string The JSON as a string
     */
    std::string virtual toJSON();

    /**
     * @brief Writes the world's JSON and alternate JSON output to the given
     * filepaths.
     *
     * @param jsonPath Path to store json
     * @param altJSONPath Path to store the alternate json representation
     */
    void writeToFile(std::string jsonPath, std::string altJSONPath);

    /**
     * @brief Construct a new World object
     */
    World();

    /**
     * @brief Get the random object used by this generator
     *
     * @return std::mt19937_64& The random object
     */
    std::mt19937_64& getRandom();

    /**
     * @brief Set the seed the random object used by this class is
     * initialized with.
     *
     * @param seed The seed to use.
     */
    void setRandom(int seed);

    /**
     * @brief Destroy the World object
     */
    virtual ~World();
};
