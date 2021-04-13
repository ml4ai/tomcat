#pragma once
#include "Pos.h"
#include <nlohmann/json.hpp>

/**
 * @brief This class represents an Entity in Minecraft. The equipment is
 *        represented as a vector of size 5 with the helmet, chestplate,
 * leggings, boots and weapon materials to assign.
 */
class Entity {

  private:
    std::string mobType;
    std::vector<std::string> equipment;
    Pos pos;

  public:
    /**
     * @brief Get the mob type.
     *
     * @return std::string The type.
     */
    std::string getMobType();

    /**
     * @brief Get the x coordinate.
     *
     * @return int The coordinate.
     */
    int getX();

    /**
     * @brief Get the y coordinate.
     *
     * @return int The coordinate.
     */
    int getY();

    /**
     * @brief Get the z coordinate.
     *
     * @return int The coordinate.
     */
    int getZ();

    /**
     * @brief Set the x coordinate.
     *
     * @param x The new coordinate.
     */
    void setX(int x);

    /**
     * @brief Set the y coordinate.
     *
     * @param y The new coordinate.
     */
    void setY(int y);

    /**
     * @brief Set the z coordinate.
     *
     * @param z The new coordinate.
     */
    void setZ(int z);


    //===========================================
    // Minecraft entity equipment-related methods
    //===========================================

    /**
     * @brief Get the helmet's material.
     *
     * @return string The material.
     */
    std::string getHelmet();

    /**
     * @brief Get the chestplate's material.
     *
     * @return string The material.
     */
    std::string getChestplate();

    /**
     * @brief Get the leggings' material.
     *
     * @return string The material.
     */
    std::string getLeggings();

    /**
     * @brief Get the boots' material.
     *
     * @return string The material.
     */
    std::string getBoots();

    /**
     * @brief Get the weapon's material.
     *
     * @return string The material.
     */
    std::string getWeapon();

    /**
     * @brief Set the helmet's material.
     *
     * @param helmet The new material.
     */
    void setHelmet(std::string helmet);

    /**
     * @brief Set the chestplate's material.
     *
     * @param chestplate The new material.
     */
    void setChestplate(std::string chestplate);

    /**
     * @brief Set the leggings' material.
     *
     * @param leggings The new material.
     */
    void setLeggings(std::string leggings);

    /**
     * @brief Set the boots' material.
     *
     * @param boots The new material.
     */
    void setBoots(std::string boots);

    /**
     * @brief Set the weapon's material.
     *
     * @param weapon The new material.
     */
    void setWeapon(std::string weapon);


    /**
     * @brief Set the mob type.
     *
     * @param mobType The new type.
     */
    void setMobType(std::string mobType);

    /**
     * @brief Set materials for all the equipment. If the size of the input
     *        vector is too small or large nothing happens.
     *
     * @param equipment A vector of size 5 with the helmet, chestplate,
     *        leggings, boots and weapon materials to assign.
     */
    void setAllEquipment(std::vector<std::string>& equipment);

    /**
     * @brief Adds the JSON representation of this object to the
     *        "entities" list of the base json.
     *
     * @return nlohmann::json The base json.
     */
    void virtual toSemanticMapJSON(nlohmann::json& json_base);

    /**
     * @brief Adds the alternate JSON representation of this
     *        object to the "entities" list of the base json.
     *
     * @return nlohmann::json The base json.
     */
    void virtual toLowLevelMapJSON(nlohmann::json& json_base);

    /**
     * @brief Construct a new Entity object. The material level values are
     *        interpreted on the Java side.
     *
     * @param mobType What kind of mob is it.
     * @param pos Where the mob should be placed.
     * @param helmet Helmet's material level.
     * @param chestplate Chestplate's material level.
     * @param leggings Leggings's material level.
     * @param boots Boots's material level.
     * @param weapon Weapons's material level.
     */
    Entity(std::string mobType,
           Pos& pos,
           std::string helmet = "none",
           std::string chestplate = "none",
           std::string leggings = "none",
           std::string boots = "none",
           std::string weapon = "none");

    /**
     * @brief Destroy the Entity object.
     */
    virtual ~Entity();
};
