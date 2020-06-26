/**
 * @file Pos.cpp
 * @brief This file implements the methods in the Pos class.
 */

#include "Pos.h"
#include <string>
using namespace std;

/**
 * @brief Construct a new Pos:: Pos object
 *
 */
Pos::Pos() {}

/**
 * @brief Construct a new Pos:: Pos object
 *
 * @param x The x coordinate
 * @param y The y coordinate
 * @param z The z coordinate
 */
Pos::Pos(int x, int y, int z) : x{x}, y{y}, z{z} {}

/**
 * @brief Construct a new Pos:: Pos object as a copy
 * of the given Pos object
 *
 * @param other The object whose fields are to be copied
 */
Pos::Pos(const Pos& other) : x{other.x}, y{other.y}, z{other.z} {}

/**
 * @brief Get the X coordinate of this object
 *
 * @return int The x coordinate
 */
int Pos::getX() { return this->x; }

/**
 * @brief Get the Y coordinate of this object
 *
 * @return int The y coordinate
 */
int Pos::getY() { return this->y; }

/**
 * @brief Get the Z coordinate of this object
 *
 * @return int The x coordinate
 */
int Pos::getZ() { return this->z; }

/**
 * @brief Set the x value of this Pos object
 *
 * @param x The value x is to be set to
 */
void Pos::setX(int x) { this->x = x; }

/**
 * @brief Set the y value of this Pos object
 *
 * @param x The value y is to be set to
 */
void Pos::setY(int y) { this->y = y; }

/**
 * @brief Set the z value of this Pos object
 *
 * @param x The value z is to be set to
 */
void Pos::setZ(int z) { this->z = z; }

/**
 * @brief Gets a string representation of the various
 * fields and values stores in an instance
 *
 * @return string The string representation
 */
string Pos::toString() {
    string retval = "(" + to_string(this->x) + "," + to_string(this->y) + "," +
                    to_string(this->z) + ")\n";
    return retval;
}

/**
 * @brief Destroy the Pos:: Pos object
 */
Pos::~Pos() {}
