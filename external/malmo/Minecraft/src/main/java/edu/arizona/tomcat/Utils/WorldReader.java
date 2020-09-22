package edu.arizona.tomcat.Utils;

import com.google.gson.Gson;
import com.microsoft.Malmo.Schemas.EntityTypes;
import edu.arizona.tomcat.World.TomcatEntity;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;

/**
 * This class can be used to read a TSV file representing a world. The TSV
 * entries are read into a hash map of coordinates and the block at those
 * coordinates.
 */
public class WorldReader {
    private Map<BlockPos, IBlockState> blockMap;
    private List<TomcatEntity> entityList;

    /**
     * Constructor for this object. The instance creates the hash map at the
     * time of initialization.
     *
     * @param filename The TSV file. Must be in Minecraft/run/
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
     * alternate JSON file.
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
     * @param blockList List of blocks extracted from the alternate JSON
     */
    private void initBlockMap(ArrayList<Map<String, String>> blockList) {
        for (Map<String, String> block : blockList) {

            String material = block.get("material");
            String x_string = block.get("x");
            String y_string = block.get("y");
            String z_string = block.get("z");

            int x = Integer.parseInt(x_string);
            int y = Integer.parseInt(y_string);
            int z = Integer.parseInt(z_string);
            BlockPos pos = new BlockPos(x, y, z);

            this.blockMap.remove(pos);

            if (material.equals("door")) {
                // Doors are special and we need to place a bottom and top half
                IBlockState doorBottom = this.getBlockState("door_bottom");

                BlockPos topPos =
                    new BlockPos(pos.getX(), pos.getY() + 1, pos.getZ());
                IBlockState doorTop = this.getBlockState("door_top");

                this.blockMap.remove(
                    topPos); // For a door we need to remove and re add the
                // block above the current as well
                this.blockMap.put(pos, doorBottom);
                this.blockMap.put(topPos, doorTop);
            }
            else {
                IBlockState state = getBlockState(material);
                this.blockMap.put(pos, state);
            }
        }
    }

    /**
     * Initializes the entityMap from the given list of entities
     *
     * @param entityList List of entities extracted from the alternate JSON
     */
    private void initEntityMap(ArrayList<Map<String, String>> entityList) {
        for (Map<String, String> entity : entityList) {

            String x_string = entity.get("x");
            String y_string = entity.get("y");
            String z_string = entity.get("z");
            String type = entity.get("mob_type");

            int x = Integer.parseInt(x_string);
            int y = Integer.parseInt(y_string);
            int z = Integer.parseInt(z_string);

            TomcatEntity thisEntity = this.getTomcatEntity(x, y, z, type);
            this.entityList.add(thisEntity);
        }
    }

    /**
     * Get the right TomcatEntity based on the type given. The UUID assigned is
     * random and the entity is placed at the given coordinates
     *
     * @param x    X coordinate of entity
     * @param y    Y coordinate of entity
     * @param z    Z coordinate of entity
     * @param type The type of entity
     * @return The generated TomcatEntity object
     */
    private TomcatEntity getTomcatEntity(int x, int y, int z, String type) {
        if (type.equals("zombie")) {
            return new TomcatEntity(
                UUID.randomUUID(), x, y, z, EntityTypes.ZOMBIE);
        }
        else if (type.equals("skeleton")) {
            return new TomcatEntity(
                UUID.randomUUID(), x, y, z, EntityTypes.SKELETON);
        }
        else {
            return new TomcatEntity(
                UUID.randomUUID(), x, y, z, EntityTypes.VILLAGER);
        }
    }

    /**
     * Returns the block state relevant to the input string.
     *
     * @param material The material whose block state representation is
     *                 required.
     * @return The block state. Only default states are returned. The default
     * block is Quartz.
     */
    private IBlockState getBlockState(String material) {
        if (material.equals("planks")) {
            return Blocks.PLANKS.getDefaultState();
        }
        else if (material.equals("prismarine")) {
            return Blocks.PRISMARINE.getDefaultState();
        }
        else if (material.equals("gold")) {
            return Blocks.GOLD_BLOCK.getDefaultState();
        }
        else if (material.equals("door_top")) {
            return Blocks.DARK_OAK_DOOR.getStateFromMeta(9);
        }
        else if (material.equals("door_bottom")) {
            return Blocks.DARK_OAK_DOOR.getStateFromMeta(0);
        }
        else if (material.equals("lava")) {
            return Blocks.LAVA.getDefaultState();
        }
        else if (material.equals("water")) {
            return Blocks.WATER.getDefaultState();
        }
        else if (material.equals("grass")) {
            return Blocks.GRASS.getDefaultState();
        }
        else if (material.equals("sand")) {
            return Blocks.SAND.getDefaultState();
        }
        else if (material.equals("cobblestone")) {
            return Blocks.COBBLESTONE.getDefaultState();
        }
        else if (material.equals("fence")) {
            return Blocks.NETHER_BRICK_FENCE.getDefaultState();
        }
        else if (material.equals("lever")) {
            return Blocks.LEVER.getDefaultState();
        }
        else if (material.equals("glowstone")) {
            return Blocks.GLOWSTONE.getDefaultState();
        }
        else if (material.equals("air")) {
            return Blocks.AIR.getDefaultState();
        }
        else if (material.equals("gravel")) {
            return Blocks.GRAVEL.getDefaultState();
        }
        else if (material.equals("waterlily")) {
            return Blocks.WATERLILY.getDefaultState();
        }
        else {
            return Blocks.QUARTZ_BLOCK.getDefaultState(); // For unknown block
        }
    }
}
