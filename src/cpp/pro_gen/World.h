/**
 * @brief This file defines the members and methods
 * implemented as part of the World class
 */
#pragma once

#include "AABB.h"
#include "Block.h"
#include <nlohmann/json.hpp>
#include <string>

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
     * @brief Returns a pointer to the vector that holds the AABBs
     *
     * @return vector<AABB>* The AABB list
     */
    std::vector<AABB*>& getAABBList();

    /**
     * @brief Returns a pointer to the vector that holds the Blocks
     *
     * @return vector<Block>* The Block list
     */
    std::vector<Block*>& getBlockList();

    /**
     * @brief Add an AABB to the vector of AABB held inside the world
     *
     * @param aabb Address of the AABB to add
     */
    void addAABB(AABB& aabb);

    /**
     * @brief Add a Block to the vector of Block held inside the world
     *
     * @param block Address of the Block to add
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
