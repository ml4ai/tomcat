#pragma once
#include "Block.h"

class Lever : public Block{
    private:
        bool powered;
        std::string facing;

    public:
        void virtual toJSON(nlohmann::json& json_base);
        Lever(Pos& pos, bool powered = false, std::string facing = "null");
        ~Lever();

};