#pragma once

#include "Group.h"
#include "Lever.h"

/**
 * @brief This class represents a very specific building that is used in the
 * Zombie mission
 */
class ZombieworldGroup : public Group {
  private:
    std::mt19937_64 gen;

    /**
     * @brief Create the Group object with either 1 or 2 rooms depending on the
     * id number
     * @param firstTopLeft The top left coordinate for the first AABB in the
     * group
     * @param firstBottomRight The bottom right coordinate for the first AABB in
     * the group
     */
    void createAABB(Pos& firstTopLeft, Pos& firstBottomRight);

    /**
     * @brief Add lights to the building
     */
    void addLights();

    /**
     * @brief Add levers to the buildings with two rooms such that the lever is
     * at the entrance to the second room.
     */
    void addLevers();

    /**
     * @brief Add mobs to this building. A single room building gets zombies
     * while double room buildings get one villager and one zombie.
     */
    void addEntities();

    /**
     * @brief Global directive method that creates the AABB and then decorates
     * them.
     *
     * @param firstTopLeft The top left coordinate for the first AABB in the
     * group
     * @param firstBottomRight The bottom right coordinate for the first AABB in
     * the group
     */
    void decorate(Pos& firstTopLeft, Pos& firstBottomRight);

  public:
    /**
     * @brief Construct a new Zombieworld Group object
     *
     * @param id The id to give this group
     * @param firstTopLeft The top left coordinate for the first AABB in the
     * group
     * @param firstBottomRight The bottom right coordinate for the first AABB in
     * the group
     */
    ZombieworldGroup(std::string id, Pos& firstTopLeft, Pos& firstBottomRight);

    /**
     * @brief Destroy the Zombieworld Group object
     */
    ~ZombieworldGroup();
};
