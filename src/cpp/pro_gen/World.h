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
    std::vector<AABB*> aabbList;
    std::vector<Block*> blockList;

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

    /**
     * @brief Add an AABB to the vector of AABB held inside the world
     *
     * @param aabb The AABB to add
     */
    void addAABB(AABB& aabb);

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
     * @brief Construct a new World object
     */
    World();

    /**
     * @brief Destroy the World object
     */
    virtual ~World();
};
