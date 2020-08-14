#pragma once
#include "Block.h"

class Object{

    private: 
        std::string id;
        std::string type;
        Block block;

    public:
        nlohmann::json virtual toJSON();
        Object(std::string id, std::string type, Block& block);
        ~Object();

};