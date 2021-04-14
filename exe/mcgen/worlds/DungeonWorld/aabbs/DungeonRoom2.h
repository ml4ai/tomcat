#pragma once

#include "mcglib/AABB.h"

/**
 * @brief This class represents a very specific building that is used in the
 *        DungeonWorld
 */
class DungeonRoom2 : public AABB {
  private:
    std::mt19937_64 gen;

    /**
     * @brief Set various fields for this room once we know where the top left
     * is
     *
     * @param topLeft The top left. Remember the y coordinate for this is the
     * lowest
     */
    void setFields(Pos& topLeft);

    /**
     * @brief Decorates the room with some blocks
     */
    void decorate();

    /**
     * @brief Add some reward blocks (diamond) to the room
     */
    void addRewards();

    /**
     * @brief Add some air blocks at random positions to give the illusion of
     * broken walls
     */
    void addBrokenWalls();

    /**
     * @brief Add some enemies for the player to fight
     */
    void addEntities();

  public:
    /**
     * @brief Construct a new Dungeon Room 2 object
     *
     * @param id The id for this room (keep it unique for each instance)
     * @param topLeft The top left. Remember the y coordinate for this is the
     * lowest
     */
    DungeonRoom2(std::string id, Pos& topLeft);

    /**
     * @brief Destroy the Dungeon Room 2 object
     */
    ~DungeonRoom2();
};
