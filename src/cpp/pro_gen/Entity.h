#pragma once
#include "Pos.h"
#include <nlohmann/json.hpp>

/**
 * @brief This class represents an Entity in Minecraft. The type field
 * determines the kind of creature it is and the remaining integer values
 * represent the equipment to give it. Numbers are used to indicate stronger and
 * weaker materials for an entity and 0 should ideally mean nothing is to be put
 * in a certain equipment slot. The actual equpment is placed within the Java
 * code so the numbers are purely symbolic.
 */
class Entity {

  private:
    std::string type;
    std::vector<int> equipment;
    Pos pos;

  public:
    /**
     * @brief Get the mob type
     *
     * @return std::string The type
     */
    std::string getType();

    /**
     * @brief Get the x coordinate
     *
     * @return int The coordinate
     */
    int getX();

    /**
     * @brief Get the y coordinate
     *
     * @return int The coordinate
     */
    int getY();

    /**
     * @brief Get the z coordinate
     *
     * @return int The coordinate
     */
    int getZ();

    /**
     * @brief Get the helmet's material level
     *
     * @return int The material level
     */
    int getHelmet();

    /**
     * @brief Get the chestplate's material level
     *
     * @return int The material level
     */
    int getChestplate();

    /**
     * @brief Get the leggings' material level
     *
     * @return int The material level
     */
    int getLeggings();

    /**
     * @brief Get the boots' material level
     *
     * @return int The material level
     */
    int getBoots();

    /**
     * @brief Get the weapon's material level
     *
     * @return int The material level
     */
    int getWeapon();

    /**
     * @brief Set the helmet's material level
     *
     * @param helmet The new material level
     */
    void setHelmet(int helmet);

    /**
     * @brief Set the chestplate's material level
     *
     * @param chestplate The new material level
     */
    void setChestplate(int chestplate);

    /**
     * @brief Set the leggings' material level
     *
     * @param leggings The new material level
     */
    void setLeggings(int leggings);

    /**
     * @brief Set the boots' material level
     *
     * @param boots The new material level
     */
    void setBoots(int boots);

    /**
     * @brief Set the weapon's material level
     *
     * @param weapon The new material level
     */
    void setWeapon(int weapon);

    /**
     * @brief Set the x coordinate
     *
     * @param x The new coordinate
     */
    void setX(int x);

    /**
     * @brief Set the y coordinate
     *
     * @param y The new coordinate
     */
    void setY(int y);

    /**
     * @brief Set the z coordinate
     *
     * @param z The new coordinate
     */
    void setZ(int z);

    /**
     * @brief Set the mob type
     *
     * @param type The new type
     */
    void setType(std::string type);

    /**
     * @brief Set values for all the equipment. If the size of the input vector
     * is too small or large nothing happens.
     *
     * @param equipment A vector of size 5 with the helmet, chestplate,
     * leggings, boots and weapon values to assign.
     */
    void setAllEquipment(std::vector<int>& equipment);

    /**
     * @brief Gets the JSON representation of the various
     * fields and values stored in an instance
     *
     * @return nlohmann::json The JSON representation
     */
    nlohmann::json virtual toJSON();

    /**
     * @brief Gets a string representation of the various
     * fields and values stored in an instance as a TSV
     *
     * @return string The TSV representation
     */
    std::string virtual toTSV();

    /**
     * @brief Construct a new Entity object. The material level values are
     * interpreted on the Java side.
     *
     * @param type What kind of mob is it
     * @param pos Where the mob should be placed
     * @param helmet Helmet's material level
     * @param chestplate Chestplate's material level
     * @param leggings Leggings's material level
     * @param boots Boots's material level
     * @param weapon Weapons's material level
     */
    Entity(std::string type,
           Pos& pos,
           int helmet = 0,
           int chestplate = 0,
           int leggings = 0,
           int boots = 0,
           int weapon = 0);

    /**
     * @brief Destroy the Entity object
     */
    virtual ~Entity();
};