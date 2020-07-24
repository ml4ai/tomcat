/**
 * @brief This file defines the members and methods
 * implemented as part of the World class
 */
#pragma once

#include "AABB.h"
#include <nlohmann/json.hpp>

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

  public:
    /**
     * @brief Returns the vector that holds the AABBs
     *
     * @return vector<AABB>& The AABB list
     */
    std::vector<AABB*>& getAABBList();

    /**
     * @brief Returns the vector that holds the Blocks
     *
     * @return vector<Block>& The Block list
     */
    std::vector<Block*>& getBlockList();

    std::vector<Entity*>& getEntityList();

    /**
     * @brief Add an AABB to the vector of AABB held inside the world
     *
     * @param aabb The AABB to add
     */
    void addAABB(AABB& aabb);

    void addEntity(Entity& entity);

    /**
     * @brief Add a Block to the vector of Block held inside the world
     *
     * @param block The Block to add
     */
    void addBlock(Block& block);

    /**
     * @brief Gets a string representation of the various
     * fields and values stores in an instance as a TSV
     *
     * @return string The TSV representation
     */
    std::string virtual toTSV();

    /**
     * @brief Converts the world into a JSON representation with
     * each entry indented by 4 and returns the string representation of it.
     *
     * @return string The JSON as a string
     */
    std::string virtual toJSON();

    /**
     * @brief Writes the world's JSON and TSV output to the given filepaths.
     *
     * @param jsonPath Path to store json
     * @param tsvPath Path to store tsv
     */
    void writeToFile(std::string jsonPath, std::string tsvPath);

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
