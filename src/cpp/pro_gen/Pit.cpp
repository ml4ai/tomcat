#include "Pit.h"
#include <iostream>
using namespace std;

Pit::Pit(int id, string material, Pos& topLeft, Pos& bottomRight)
    : AABB(id, "pit", material, topLeft, bottomRight, false, true) {}

Pit::~Pit() {}