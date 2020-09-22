package edu.arizona.tomcat.Utils;

import com.google.gson.Gson;
import com.microsoft.Malmo.Schemas.EntityTypes;
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
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;

/**
 * This class can be used to read a JSON file representing a world. The JSON
 * entries are read into a hash map of coordinates and the block at those
 * coordinates. The same applies for entities.
 */
public class WorldReader {
    private Map<BlockPos, IBlockState> blockMap;
    private List<TomcatEntity> entityList;

    /**
     * Constructor for this object. The instance creates the hash map at the
     * time of initialization.
     *
     * @param filename The low level JSON file. Must be in Minecraft/run/
     */
    public WorldReader(String filename) {

        this.blockMap = new LinkedHashMap<BlockPos, IBlockState>();
        this.entityList = new ArrayList<TomcatEntity>();
        this.initMaps(filename);
    }

    /**
     * Get the map this WorldReader object uses to keep track of all the blocks
     * in the world read from file.
     *
     * @return The map of coordinates and the block at those coordinates. The
     * block to be placed is represented as the IBlockState object used
     * for the block type in its default state,
     */
    public Map<BlockPos, IBlockState> getBlocksMap() { return this.blockMap; }

    /**
     * Get the list of entities in the world
     *
     * @return The list of TomcatEntity objects
     */
    public List<TomcatEntity> getEntityList() { return this.entityList; }

    /**
     * This method is use to create the map representation of the world from the
     * low level JSON file.
     *
     * @param filename The alternate JSON file. Must be in Minecraft/run/
     */
    private void initMaps(String filename) {

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

        this.initBlockMap(blueprint.get("blocks"));
        this.initEntityMap(blueprint.get("entities"));
    }

    /**
     * Initializes the blockMap from the given list of blocks
     *
     * @param blockList List of blocks extracted from the low level JSON
     */
    private void initBlockMap(ArrayList<Map<String, String>> blockList) {
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
    }

    /**
     * Initializes the entityMap from the given list of entities
     *
     * @param entityList List of entities extracted from the low level JSON
     */
    private void initEntityMap(ArrayList<Map<String, String>> entityList) {
        for (Map<String, String> entity : entityList) {

            String x_string = entity.get("x");
            String y_string = entity.get("y");
            String z_string = entity.get("z");
            String type = entity.get("mob_type");
            type = type.toUpperCase();

            int x = Integer.parseInt(x_string);
            int y = Integer.parseInt(y_string);
            int z = Integer.parseInt(z_string);

            TomcatEntity thisEntity = new TomcatEntity(
                UUID.randomUUID(), x, y, z, EntityTypes.valueOf(type));
            this.entityList.add(thisEntity);
        }
    }

    /**
     * This will add the block of the given material at the given position (into
     * the block map) with the given properties set if applicable. It will try
     * to create the block state with as many properties as applicable. Doors
     * are handled automagically :)
     *
     * @param material - The material of the block
     * @param pos - Where to place the block
     * @param facing - Which way is the block facing
     * @param powered - Is the block powered?
     * @param open - Is the block open? Only applicable for doors/trapdoors
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
                state = Block.getBlockFromName(material)
                            .getDefaultState()
                            .withProperty(FACING, facing)
                            .withProperty(OPEN, open)
                            .withProperty(POWERED, powered);
            }
            catch (Exception e) {
                try {
                    state = Block.getBlockFromName(material)
                                .getDefaultState()
                                .withProperty(FACING, facing)
                                .withProperty(OPEN, open);
                }
                catch (Exception e2) {
                    try {
                        state = Block.getBlockFromName(material)
                                    .getDefaultState()
                                    .withProperty(FACING, facing);
                    }
                    catch (Exception e3) {
                        try {
                            state = Block.getBlockFromName(material)
                                        .getDefaultState();
                        }
                        catch (Exception e4) {
                            state = Blocks.PLANKS.getDefaultState();
                        }
                    }
                }
            }
            this.blockMap.put(pos, state);
        }
    }
}