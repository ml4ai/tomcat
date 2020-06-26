#include "AABB.h"
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <string>

using namespace std;

AABB::AABB(int AABBid,
           string AABBmaterial,
           Pos* topLeftPos,
           Pos* bottomRightPos)
    : id(AABBid), material(AABBmaterial), topLeft(*topLeftPos),
      bottomRight(*bottomRightPos) {}

int AABB::getID() { return this->id; }

string AABB::getMaterial() { return this->material; }

Pos AABB::getTopLeft() { return this->topLeft; }

Pos AABB::getBottomRight() { return this->bottomRight; }

int AABB::getMidpointX() {
    int mid_x = ((this->topLeft).getX() +
                 ((this->bottomRight).getX() - (this->topLeft).getX()) / 2);
    return mid_x;
}

int AABB::getMidpointY() {
    int mid_y = ((this->topLeft).getY() +
                 ((this->bottomRight).getY() - (this->topLeft).getY()) / 2);
    return mid_y;
}

int AABB::getMidpointZ() {
    int mid_z = ((this->topLeft).getZ() +
                 ((this->bottomRight).getZ() - (this->topLeft).getZ()) / 2);
    return mid_z;
}

Pos AABB::getRandomPosAtBase(int offsetPosX = 1,
                             int offsetNegX = 1,
                             int offsetPosZ = 1,
                             int offsetNegZ = 1) {

    int startX = (this->topLeft).getX() + offsetPosX;
    int startZ = (this->topLeft).getZ() + offsetPosZ;

    int endX = (this->bottomRight).getX() - offsetNegX;
    int endZ = (this->bottomRight).getZ() - offsetNegZ;

    boost::random::mt19937 gen;
    boost::random::uniform_int_distribution<> randXGen(startX, endX);
    boost::random::uniform_int_distribution<> randZGen(startZ, endZ);

    int randX = randXGen(gen);
    int randZ = randZGen(gen);

    int base = (this->topLeft).getY();
    Pos pos(randX, base, randZ);

    return pos;
}

string AABB::toString() {
    string retval = "ID: " + to_string(this->id) + "\n" +
                    "Material: " + this->material + "\n" +
                    "Top Left: " + (this->topLeft).toString() +
                    "Bottom Right: " + (this->bottomRight).toString();

    return retval;
}

AABB::~AABB() {}
