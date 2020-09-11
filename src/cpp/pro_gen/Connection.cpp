#include "Connection.h"

using namespace std;

Connection::Connection(string id, string name, string type, string boundType)
    : id{id}, name{name}, type{type} {}

string Connection::getID() { return this->id; }

string Connection::getName() { return this->name; }

string Connection::getType() { return this->type; }

string Connection::getBoundType() { return this->boundType; }

vector<string>& Connection::getConnectedLocations() {
    return this->connectedLocations;
}

vector<Pos>& Connection::getCoordinates() { return this->coordinates; }

string Connection::setID(string newID) { this->id = newID; }

string Connection::setName(string newName) { this->name = newName; }

string Connection::setType(string newType) { this->type = newType; }

string Connection::setBoundType(string newBoundType) {
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

Connection::~Connection(){};