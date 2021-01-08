package edu.arizona.tomcat.Utils;

import com.google.gson.Gson;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.DrawingHandler;
import edu.arizona.tomcat.World.TomcatEntity;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;
import net.minecraft.block.Block;
import net.minecraft.block.BlockHorizontal;
import net.minecraft.block.properties.PropertyBool;
import net.minecraft.block.properties.PropertyDirection;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.inventory.EntityEquipmentSlot;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * This class can be used to build a world from a JSON file
 */
public class WorldBuilder {

    private World world;
    private Map<BlockPos, IBlockState>
        blockMap; // Stores info about the blocks we place
    private Set<UUID>
        entityUUIDSet; // Stores hte UUIDs o all the entities we place

    /**
     * This method is used to build the world represented by the file into the
     * instance of the world given to the method
     *
     * @param filename        The world's JSON file. Must be in Minecraft/run/
     * @param world           - The world to build in
     * @param saveBlockMap    - Should the block map be saved after use (set to
     *     false to save memory)
     * @param saveEntityUUIDs - Should the set of entity UUIDs be saved after
     *     use (set to false to save memory)
     */
    public void build(String filename,
                      World world,
                      boolean saveBlockMap,
                      boolean saveEntityUUIDs) {

        this.world = world;
        this.blockMap = new LinkedHashMap<BlockPos, IBlockState>();
        this.entityUUIDSet = new HashSet<UUID>();

        // Read blueprint from JSON
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(filename));
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        Gson gson = new Gson();
        Map<String, ArrayList<Map<String, String>>> blueprint =
            gson.fromJson(reader, Map.class);

        // Use blueprint to place blocks and entities
        this.placeBlocks(blueprint.get("blocks"));
        if (!saveBlockMap) {
            this.blockMap = null;
        }

        this.placeEntities(blueprint.get("entities"));
        if (!saveEntityUUIDs) {
            this.entityUUIDSet = null;
        }
    }

    /**
     * Get the map of block positions to the block state at those positions for
     * this world builder object.
     *
     * @return The block map of the generated world or null if not saved
     */
    public Map<BlockPos, IBlockState> getBlockMap() { return this.blockMap; }

    /**
     * Get the UUIDs of all the entities placed into the world
     *
     * @return The set of UUIDs or null if not saved
     */
    public Set<UUID> getEntityUUIDs() { return this.entityUUIDSet; }

    /**
     * Place the blocks read from the blueprint into the world
     *
     * @param blockList List of blocks extracted from the low level JSON
     */
    private void placeBlocks(ArrayList<Map<String, String>> blockList) {

        for (Map<String, String> block : blockList) {

            String material = block.get("material").toUpperCase();

            int x = Integer.parseInt(block.get("x")),
                y = Integer.parseInt(block.get("y")),
                z = Integer.parseInt(block.get("z"));
            BlockPos pos = new BlockPos(x, y, z);

            this.blockMap.remove(pos);

            EnumFacing facing = EnumFacing.NORTH;
            Boolean powered = Boolean.valueOf(false);
            Boolean open = Boolean.valueOf(false); // Default values

            try {
                facing = EnumFacing.valueOf(block.get("facing").toUpperCase());
                powered = Boolean.valueOf(block.get("powered"));
                open = Boolean.valueOf(block.get("open"));
            }
            catch (Exception e) {
                ;
            }
            this.registerState(material, pos, facing, powered, open);
        }

        // Place the blocks into the world
        for (Map.Entry<BlockPos, IBlockState> entry :
             this.blockMap.entrySet()) {
            this.world.setBlockState(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Place the entities read from the blueprint into the world
     *
     * @param entityList List of entities extracted from the low level JSON
     */
    private void placeEntities(ArrayList<Map<String, String>> entityList) {
        DrawingHandler drawingHandler = DrawingHandler.getInstance();

        for (Map<String, String> entity : entityList) {

            // Create the relevant entity as a TomcatEntity
            int x = Integer.parseInt(entity.get("x"));
            int y = Integer.parseInt(entity.get("y"));
            int z = Integer.parseInt(entity.get("z"));

            String type = entity.get("mob_type").toUpperCase();

            Map<String, String> equipment = new HashMap<String, String>();
            equipment.put("MAINHAND", entity.get("weapon").toUpperCase());
            equipment.put("HEAD", entity.get("helmet").toUpperCase());
            equipment.put("CHEST", entity.get("chestplate").toUpperCase());
            equipment.put("LEGS", entity.get("leggings").toUpperCase());
            equipment.put("FEET", entity.get("boots").toUpperCase());

            UUID id = UUID.randomUUID();
            this.entityUUIDSet.add(id);
            TomcatEntity thisEntity =
                new TomcatEntity(id, x, y, z, EntityTypes.valueOf(type));

            // Place the entity into the world
            Drawing drawing = new Drawing();
            drawing.addObject(thisEntity);

            try {
                drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            this.equipEntity(
                equipment,
                MinecraftServerHelper.getServer().getEntityFromUuid(id));
        }
    }

    /**
     * Equips the given entity using the information in the equipmentMap
     *
     * @param equipmentMap - A map of what equipment goes where
     * @param entity       - The entity to give the equipment to
     */
    private void equipEntity(Map<String, String> equipmentMap, Entity entity) {
        for (Map.Entry<String, String> entry : equipmentMap.entrySet()) {
            try {
                entity.setItemStackToSlot(
                    EntityEquipmentSlot.valueOf(entry.getKey()),
                    new ItemStack(Item.getByNameOrId(entry.getValue())));
            }
            catch (Exception e) {
                System.out.println("DEBUG: Could not give " + entry.getValue() +
                                   " to " + entry.getKey());
            }
        }
    }

    /**
     * This will add the block of the given material at the given position (into
     * the block map) with the given properties set if applicable. It will try
     * to create the block state with as many properties as applicable. Doors
     * are handled automagically :)
     *
     * @param material - The material of the block
     * @param pos      - Where to place the block
     * @param facing   - Which way is the block facing
     * @param powered  - Is the block powered?
     * @param open     - Is the block open? Only applicable for doors/trapdoors
     */
    private void registerState(String material,
                               BlockPos pos,
                               EnumFacing facing,
                               Boolean powered,
                               Boolean open) {
        PropertyDirection FACING = BlockHorizontal.FACING;
        PropertyBool OPEN = PropertyBool.create("open");
        PropertyBool POWERED = PropertyBool.create("powered");

        if (material.contains("DOOR")) {
            try {
                // Doors are special and we need to place a bottom and top half
                IBlockState doorBottom = Block.getBlockFromName(material)
                                             .getStateFromMeta(0)
                                             .withProperty(FACING, facing)
                                             .withProperty(OPEN, open)
                                             .withProperty(POWERED, powered);

                BlockPos topPos =
                    new BlockPos(pos.getX(), pos.getY() + 1, pos.getZ());
                IBlockState doorTop = Block.getBlockFromName(material)
                                          .getStateFromMeta(9)
                                          .withProperty(FACING, facing)
                                          .withProperty(OPEN, open)
                                          .withProperty(POWERED, powered);

                this.blockMap.remove(
                    topPos); // For a door we need to remove and re add the
                // block above the current as well

                this.blockMap.put(pos, doorBottom);
                this.blockMap.put(topPos, doorTop);
            }
            catch (Exception e) {
                System.out.println(
                    "Oops! Looks like you forgot to specify some properties for this door");
            }
        }
        else {
            IBlockState state;
            // Try to create the block state with as many properties as
            // applicable
            try {
                state = Block.getBlockFromName(material).getDefaultState();
                try {
                    state = state.withProperty(POWERED, powered);
                }
                catch (Exception e) {
                    System.out.println(
                        "DEBUG: Could not apply POWERED property to " +
                        material);
                }

                try {
                    state = state.withProperty(FACING, facing);
                }
                catch (Exception e) {
                    System.out.println(
                        "DEBUG: Could not apply FACING property to " +
                        material);
                }

                try {
                    state = state.withProperty(OPEN, open);
                }
                catch (Exception e) {
                    System.out.println(
                        "DEBUG: Could not apply OPEN property to " + material);
                }
            }
            catch (Exception e) {
                state = Blocks.PLANKS.getDefaultState();
                System.out.println(
                    "DEBUG: Defaulting to PLANKS for unrecognized material " +
                    material);
            }
            this.blockMap.put(pos, state);
        }
    }
}