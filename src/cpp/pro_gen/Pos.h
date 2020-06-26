#pragma once

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
    Pos();
    Pos(int, int, int);
    Pos(const Pos& other);
    ~Pos();
};
