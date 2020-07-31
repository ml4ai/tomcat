package edu.arizona.tomcat.Utils;

import com.microsoft.Malmo.Schemas.EntityTypes;
import edu.arizona.tomcat.World.TomcatEntity;
import java.io.File;
import java.io.FileNotFoundException;
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
        this.initMap(filename);
    }

    /**
     * Get the map this WorldReader object uses to keep track of all the blocks
     * in the world read from file.
     *
     * @return The map of coordinates and the block at those coordinates. The
     *     block to be placed is represented as the IBlockState object used
     * for the block type in its default state,
     */
    public Map<BlockPos, IBlockState> getBlocksMap() { return this.blockMap; }

    public List<TomcatEntity> getEntityList() { return this.entityList; }

    /**
     * This method is use to create the map representation of the world from the
     * TSV file.
     *
     * @param filename The TSV file. Must be in Minecraft/run/
     */
    private void initMap(String filename) {

        // Read file
        Scanner file = null;

        try {
            file = new Scanner((new File(filename)));
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        // Use file
        while (file.hasNextLine()) {
            String line = file.nextLine();
            String[] entry = line.split("\t");

            int x = Integer.parseInt(entry[0]);
            int y = Integer.parseInt(entry[1]);
            int z = Integer.parseInt(entry[2]);
            BlockPos pos = new BlockPos(x, y, z);
            String objectType = entry[3];

            if (objectType.equals("block")) {
                String material = entry[4];
                if (this.blockMap.containsKey(pos)) {
                    this.blockMap.remove(pos);
                } // When duplicate coordinates are encountered we remove and
                  // re-add so iteration order is correct

                if (material.equals("door")) {
                    // Doors are special adn we need to place a bottom and top
                    // half
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
            else if (objectType.equals("entity")) {
                String type = entry[4];
                TomcatEntity entity = this.getTomcatEntity(x, y, z, type);
                this.entityList.add(entity);
            }
            else {
                ;
            }
        }
    }

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
     *     required.
     * @return The block state. Only default states are returned. The default
     *     block is Quartz.
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
        else {
            return Blocks.QUARTZ_BLOCK.getDefaultState(); // For unknown block
        }
    }
}
