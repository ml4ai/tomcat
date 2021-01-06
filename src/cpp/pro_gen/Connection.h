#pragma once

#include "Pos.h"

/**
 * @brief This class represents a Connection for semantically representing
 * doorways etc in the higher level JSON
 */
class Connection {

  private:
    std::string id;
    std::string name;
    std::string type;
    std::string boundType;
    std::vector<std::string> connectedLocations;
    std::vector<Pos> coordinates;

  public:
    /**
     * @brief Returns the id of this Connection
     *
     * @return std::string the id
     */
    std::string getID();

    /**
     * @brief Returns the name of this Connection
     *
     * @return std::string the name
     */
    std::string getName();

    /**
     * @brief Returns the type of this Connection
     *
     * @return std::string the type
     */
    std::string getType();

    /**
     * @brief Returns the bound type of this Connection
     *
     * @return std::string The bound type
     */
    std::string getBoundType();

    /**
     * @brief Get the vector of ids of the connected locations this object
     * represents
     *
     * @return std::vector<std::string>& The vector of connected locations
     */
    std::vector<std::string>& getConnectedLocations();

    /**
     * @brief Get the coordinates of this connection as a vector. The number of
     * elements in the vector depends on the type of bound set by the user.
     *
     * @return std::vector<Pos>& The coordinates vector
     */
    std::vector<Pos>& getCoordinates();

    /**
     * @brief Set the id of this connection to a new id
     *
     * @param newID The new id to set to
     */
    void setID(std::string newID);

    /**
     * @brief Set the name of this connection to a new name
     *
     * @param newName The new name to set to
     */
    void setName(std::string newName);

    /**
     * @brief Set the type of this connection to a new type
     *
     * @param newType The new type to set to
     */
    void setType(std::string newType);

    /**
     * @brief Set the bound type of this connection to a new type. Remember to
     * update the coordinates of the bound assocaited accordingly since no
     * implicit checks are performed.
     *
     * @param newBoundType The new bound type to set to
     */
    void setBoundType(std::string newBoundType);

    /**
     * @brief Add the id of a connected location
     *
     * @param id The id of the location
     */
    void addConnectedLocation(std::string id);

    /**
     * @brief Add a coordinate to the bounds' coordinate list. Remember to make
     * sure that the number of coordinates you have match the bound type you
     * set.
     *
     * @param pos The coordinate to add
     */
    void addCoordinates(Pos& pos);

    /**
     * @brief Add the ids of many connected locations
     *
     * @param idVector The vector of ids
     */
    void addManyConnectedLocations(std::vector<std::string>& idVector);

    /**
     * @brief Add many coordinates to the bounds' coordinate list. Remember to
     * make sure that the number of coordinates you have match the bound type
     * you set.
     *
     * @param posVector The vector of coordinates
     */
    void addManyCoordinates(std::vector<Pos>& posVector);

    /**
     * @brief Empties the list of connected locations
     */
    void removeAllConnectedLocation();

    /**
     * @brief Empties the list of coordinates
     */
    void removeAllCoordinates();

    /**
     * @brief Adds the JSON representation of this object to the
     *        "connections" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Connection object
     *
     * @param id The id for this object
     * @param name The name for the connection and what it represents
     * @param type The type of connection, ex. door
     * @param boundType The type of bound, ex. rectangle
     */
    Connection(std::string id,
               std::string name,
               std::string type,
               std::string boundType);

    /**
     * @brief Destroy the Connection object
     */
    ~Connection();
};
