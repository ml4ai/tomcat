#include "Pos.h"

Pos::Pos() {}

Pos::Pos(int x, int y, int z) : x{x}, y{y}, z{z} {}

Pos::Pos(const Pos& other) : x{other.x}, y{other.y}, z{other.z} {}

int Pos::getX() { return this->x; }
int Pos::getY() { return this->y; }
int Pos::getZ() { return this->z; }
void Pos::setX(int x) { this->x = x; }
void Pos::setY(int y) { this->y = y; }
void Pos::setZ(int z) { this->z = z; }
Pos::~Pos() {}
