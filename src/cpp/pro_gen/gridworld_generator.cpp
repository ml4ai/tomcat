#include "AABB.h"
#include "Block.h"
#include <iostream>

int main() {
    Pos pos1(0, 0, 0);
    Pos pos2(10, 10, 10);
    Block block("door", "oak_door", pos1);
    AABB aabb(1, "planks", pos1, pos2);
    cout << block.getX() << endl;
    cout << aabb.getID() << endl;
    cout << aabb.getMaterial() << endl;
    cout << aabb.getTopLeft().getZ() << endl;
    cout << aabb.getBottomRight().getZ() << endl;
    return 0;
}