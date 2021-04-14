#pragma once

#include "mcglib/AABB.h"
#include "mcglib/Entity.h"

class House : public AABB{

private:
    std::mt19937_64 gen;

public:

    void init();
    House(std::string id, Pos& topLeft);
    ~House();

};
