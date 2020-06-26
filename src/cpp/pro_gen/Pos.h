/**
 * @brief This file defines the members and methods
 * implemented as part of the Pos class
 *
 */
#pragma once
#include <string>

/**
 * @brief This class represents a 3D coordinate
 *
 */
class Pos {

  private:
    int x;
    int y;
    int z;

  public:
    int getX();
    int getY();
    int getZ();
    void setX(int);
    void setY(int);
    void setZ(int);
    std::string toString();
    Pos();
    Pos(int, int, int);
    Pos(const Pos& other);
    ~Pos();
};
