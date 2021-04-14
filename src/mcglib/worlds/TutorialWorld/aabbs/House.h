#pragma once
#include "../../../core/AABB.h"
#include "../../../core/Entity.h"

class House : public AABB{

private:
    std::mt19937_64 gen;

public:

    void init();
    House(std::string id, Pos& topLeft);
    ~House();

};