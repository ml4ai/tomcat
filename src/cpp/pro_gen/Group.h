#pragma once

#include "AABB.h"

class Group : public AABB {

  private:
    std::vector<AABB*> aabbList;
    void recalculateGroupBoundaries();

  public:
    /**
     * @brief Adds an AABB that will be part of this AABB group.
     *
     * @param aabb The AABB to add
     */
    void addAABB(AABB& aabb);

    /**
     * @brief Generates all doors in each AABB of this group
     */
    void generateAllDoorsInAABB();

    /**
     * @brief Gets the list of AABBs this group keeps track of.
     *
     * @return std::vector<AABB*>& Reference to the AABB group kept track of.
     */
    std::vector<AABB*>& getAABBList();

    /**
     * @brief Get a particular AABB contained by this group. The AABB can be
     * identified by its ID.
     *
     * @param id The id of the AABB to find
     * @return AABB* Pointer to the relevant AABB or nullptr if it doesn't exist
     */
    AABB* getAABB(std::string id);

    void virtual toJSON(nlohmann::json& json_base);

    /**
     * @brief Creates the TSV representation of the instance.
     *
     * @return std::string The TSV representation as a string
     */
    void virtual toAltJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Group object.
     *
     * @param id The id to give the Group object.
     */
    Group(std::string id);

    /**
     * @brief Destroy the Group object
     */
    virtual ~Group();
};