#include "mcg/Connection.h"

using namespace std;
using json = nlohmann::json;

Connection::Connection(string id, string name, string type, string boundType)
    : id{id}, name{name}, type{type}, boundType{boundType} {}

string Connection::getID() { return this->id; }

string Connection::getName() { return this->name; }

string Connection::getType() { return this->type; }

string Connection::getBoundType() { return this->boundType; }

vector<string>& Connection::getConnectedLocations() {
    return this->connectedLocations;
}

vector<Pos>& Connection::getCoordinates() { return this->coordinates; }

void Connection::setID(string newID) { this->id = newID; }

void Connection::setName(string newName) { this->name = newName; }

void Connection::setType(string newType) { this->type = newType; }

void Connection::setBoundType(string newBoundType) {
    this->boundType = newBoundType;
}

void Connection::addConnectedLocation(string id) {
    this->connectedLocations.push_back(id);
}

void Connection::addCoordinates(Pos& pos) { this->coordinates.push_back(pos); }

void Connection::addManyConnectedLocations(vector<string>& idVector) {
    for (auto& id : idVector) {
        this->connectedLocations.push_back(id);
    }
}
void Connection::addManyCoordinates(vector<Pos>& posVector) {
    for (auto& pos : posVector) {
        this->coordinates.push_back(pos);
    }
}
void Connection::removeAllConnectedLocation() {
    this->connectedLocations.clear();
}
void Connection::removeAllCoordinates() { this->coordinates.clear(); }

void Connection::toSemanticMapJSON(json& json_base) {
    json connection_json;
    vector<json> coordinate_list;
    for (auto& pos : this->coordinates) {
        coordinate_list.push_back(pos.toSemanticMapJSON());
    }

    connection_json["bounds"] = {{"type", this->getBoundType()},
                                 {"coordinates", coordinate_list}};
    connection_json["id"] = this->getID();
    connection_json["name"] = this->getName();
    connection_json["type"] = this->getType();
    connection_json["connected_locations"] = this->connectedLocations;

    json_base["connections"].push_back(connection_json);
};

Connection::~Connection(){};
