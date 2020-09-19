#pragma once

#include "Pos.h"

class Connection {

  private:
    std::string id;
    std::string name;
    std::string type;
    std::string boundType;
    std::vector<std::string> connectedLocations;
    std::vector<Pos> coordinates;

  public:
    std::string getID();
    std::string getName();
    std::string getType();
    std::string getBoundType();
    std::vector<std::string>& getConnectedLocations();
    std::vector<Pos>& getCoordinates();
    void setID(std::string newID);
    void setName(std::string newName);
    void setType(std::string newType);
    void setBoundType(std::string newBoundType);
    void addConnectedLocation(std::string id);
    void addCoordinates(Pos& pos);
    void addManyConnectedLocations(std::vector<std::string>& idVector);
    void addManyCoordinates(std::vector<Pos>& posVector);
    void removeAllConnectedLocation();
    void removeAllCoordinates();

    /**
     * @brief Adds the JSON representation of this object to the
     *        "locations" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toJSON(nlohmann::json& json_base);

    Connection(std::string id,
               std::string name,
               std::string type,
               std::string boundType);
    ~Connection();
};