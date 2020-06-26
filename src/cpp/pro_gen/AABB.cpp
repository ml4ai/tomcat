#include "AABB.h"

AABB::AABB(int AABBid, string AABBmaterial, Pos topLeftPos, Pos bottomRightPos)
    : id(AABBid), material(AABBmaterial), topLeft(topLeftPos),
      bottomRight(bottomRightPos) {}

int AABB::getID() { return this->id; }

string AABB::getMaterial() { return this->material; }

Pos AABB::getTopLeft() {
    Pos copy(this->topLeft);
    return copy;
}

Pos AABB::getBottomRight() {
    Pos copy(this->bottomRight);
    return copy;
}

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

AABB::~AABB() {}