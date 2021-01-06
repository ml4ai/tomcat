#pragma once
#include "Block.h"
#include <memory>

class Object {

  private:
    std::string id;
    std::string type;
    std::unique_ptr<Block> block;

  public:
    /**
     * @brief Adds the JSON representation of this object to the
     *        "objects" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the block JSON representation of this object to the
     *        "blocks" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Object object
     *
     * @param id  The id to give this object
     * @param type The type for this object
     * @param block The block associated with this object
     */
    Object(std::string id, std::string type, std::unique_ptr<Block> block);

    /**
     * @brief Destroy the Object object
     */
    ~Object();
};
