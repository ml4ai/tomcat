#pragma once
#include "Block.h"

/**
 * @brief Represents an object. An object contains a block, and so this class is
 * mainly used to faclitate deeper semantic separation than the Block class.
 *
 */
class Object {

  private:
    std::string id;
    std::string type;
    std::unique_ptr<Block> block;

  public:
  /**
     * @brief Shift the x value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftX(int shift);

    /**
     * @brief Shift the y value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftY(int shift);

    /**
     * @brief Shift the z value by a given amount
     *
     * @param shift The amount to shift by which may be positive or negative
     */
    void shiftZ(int shift);

    /**
     * @brief Shift the x,y and z values by given amounts
     *
     * @param shiftX The amount to shift x by which may be positive or negative
     * @param shiftY The amount to shift x by which may be positive or negative
     * @param shiftZ The amount to shift x by which may be positive or negative
     */
    void shift(int shiftX, int shiftY, int shiftZ);

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
