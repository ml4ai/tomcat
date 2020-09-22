#include "ZombieworldPit.h"
using namespace std;

ZombieworldPit::ZombieworldPit(string id, Pos& topLeft, string fluid)
    : AABB(id, "pit", "grass", topLeft, topLeft, false, true) {
    this->fluid = fluid;

    Pos bottomRight(topLeft);
    bottomRight.shiftX(10);
    bottomRight.shiftY(3);
    bottomRight.shiftZ(10);
    this->setBottomRight(bottomRight);

    if (this->fluid == "water") {
        this->decorateWater();
    }
    else if (this->fluid == "lava") {
        this->decorateLava();
    }
    else {
        this->setMaterial("air");
        Pos topLeft = this->getTopLeft();
        topLeft.shiftY(1);
        this->setTopLeft(topLeft);
    }
}

void ZombieworldPit::decorateWater() {
    this->generateBox("air", 0, 0, 3, 0, 0, 0);
    this->generateBox("water", 3, 4, 1, 1, 4, 2);
    this->generateBox("water", 6, 2, 2, 1, 6, 1);
    this->generateBox("gravel", 1, 7, 2, 1, 1, 1);

    this->addRandomBlocks(15, "gravel", this->gen, 10, 0, 2, 1, 1, 0);
    this->addRandomBlocks(15, "sand", this->gen, 8, 1, 2, 1, 1, 0);

    this->addRandomBlocks(15, "gravel", this->gen, 7, 0, 2, 1, 0, 7);
    this->addRandomBlocks(15, "sand", this->gen, 8, 1, 2, 1, 0, 7);

    this->addRandomBlocks(20, "water", this->gen, 2, 2, 2, 1, 2, 1);
    this->addRandomBlocks(5, "waterlily", this->gen, 3, 4, 3, 0, 4, 2);
}

void ZombieworldPit::decorateLava() {
    this->generateBox("air", 0, 0, 3, 0, 0, 0);
    this->generateBox("lava", 3, 4, 1, 1, 4, 2);
    this->generateBox("gravel", 1, 7, 2, 1, 1, 1);

    this->addRandomBlocks(15, "gravel", this->gen, 10, 0, 2, 1, 1, 0);
    this->addRandomBlocks(15, "cobblestone", this->gen, 8, 1, 2, 1, 1, 0);

    this->addRandomBlocks(15, "gravel", this->gen, 7, 0, 2, 1, 0, 7);
    this->addRandomBlocks(15, "cobblestone", this->gen, 8, 1, 2, 1, 0, 7);

    this->addRandomBlocks(20, "lava", this->gen, 2, 2, 2, 1, 2, 1);
}

ZombieworldPit::~ZombieworldPit() {}
