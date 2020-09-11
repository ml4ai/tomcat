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
    std::string setID(std::string newID);
    std::string setName(std::string newName);
    std::string setType(std::string newType);
    std::string setBoundType(std::string newBoundType);
    void addConnectedLocation(std::string id);
    void addCoordinates(Pos& pos);
    void addManyConnectedLocations(std::vector<std::string>& idVector);
    void addManyCoordinates(std::vector<Pos>& posVector);
    void removeAllConnectedLocation();
    void removeAllCoordinates();
    Connection(std::string id,
               std::string name,
               std::string type,
               std::string boundType);
    ~Connection();
};