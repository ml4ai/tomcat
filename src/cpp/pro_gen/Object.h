#pragma once
#include "Block.h"

class Object {

  private:
    std::string id;
    std::string type;
    Block block;

  public:
    /**
     * @brief Adds the JSON representation of this object to the
     *        "objects" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the block JSON representation of this object to the
     *        "blocks" list of the base json
     *
     * @return nlohmann::json The base json
     */
    void virtual toAltJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Object object
     *
     * @param id  The id to give this object
     * @param type The type for this object
     * @param block The block associated with this object
     */
    Object(std::string id, std::string type, Block& block);

    /**
     * @brief Destroy the Object object
     */
    ~Object();
};