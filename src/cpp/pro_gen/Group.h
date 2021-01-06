#pragma once

#include "AABB.h"
#include "Connection.h"
#include <memory>

class Group : public AABB {

  private:
    std::vector<std::unique_ptr<AABB>> aabbList;
    std::vector<std::unique_ptr<Connection>> connectionList;

    /**
     * @brief Recalculates the extremes of the group as a whole when a new AABB
     * is added to the group.
     */
    void recalculateGroupBoundaries();

  public:
    /**
     * @brief Adds an AABB that will be part of this AABB group.
     *
     * @param aabb The AABB to add
     */
    void addAABB(std::unique_ptr<AABB> aabb);

    /**
     * @brief Add an connection to the vector of connection held inside the
     * group
     *
     * @param connection The connection to add
     */
    void addConnection(std::unique_ptr<Connection> connection);

    /**
     * @brief Generates all doors in each AABB of this group
     */
    void generateAllDoorsInAABB();

    /**
     * @brief Gets the list of AABBs this group keeps track of.
     *
     * @return std::vector<AABB*>& Reference to the AABB group kept track of.
     */
    std::vector<std::unique_ptr<AABB>>& getAABBList();

    /**
     * @brief Returns the Connection vector for this Group
     *
     * @return std::vector<Connection*>&  The connection list
     */
    std::vector<std::unique_ptr<Connection>>& getConnectionList();

    /**
     * @brief Get a particular AABB contained by this group. The AABB can be
     * identified by its ID.
     *
     * @param id The id of the AABB to find
     * @return AABB* Pointer to the relevant AABB or nullptr if it doesn't exist
     */
    AABB* getAABB(std::string id);

    /**
     * @brief Adds the JSON representation of this object to the
     *        relevant lists of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the Alternate JSON representation of this object to the
     *        relevant lists of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

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
