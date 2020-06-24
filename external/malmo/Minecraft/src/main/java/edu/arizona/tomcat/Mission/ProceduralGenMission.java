package edu.arizona.tomcat.Mission;

import com.google.gson.Gson;
import com.google.gson.internal.LinkedTreeMap;
import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Map;

public class ProceduralGenMission extends Mission {

    public boolean shouldBuild;

    public ProceduralGenMission() {
        super();
        this.id = ID.PROCEDURAL;
        this.shouldBuild = true;
    }

    @Override
    protected void updateScene(World world) {
        this.initializer.init(world);
        this.buildStructures(world);
    }

    /**
     * This method uses the blueprint to generate the structures for the world. Structures includes the AABB and individual blocks in the blueprint.
     *
     * @param world - The world in which structures are to be generated
     */
    private void buildStructures(World world) {
        if (this.shouldBuild == true) {
            // Grab blueprint from the JSON
            Map<String, ArrayList<LinkedTreeMap<String, String>>> blueprint = this.getBlueprintFromJSON("procedural.json");

            // Extract information and use it to build the AABB
            for (LinkedTreeMap<String, String> AABB : blueprint.get("aabb_list")) {
                int x1 = Integer.parseInt(AABB.get("x1"));
                int y1 = 3 + Integer.parseInt(AABB.get("y1")); // Floor should be at ground level
                int z1 = Integer.parseInt(AABB.get("z1"));

                int x2 = Integer.parseInt(AABB.get("x2"));
                int y2 = 3 + Integer.parseInt(AABB.get("y2"));  // Floor should be at ground level
                int z2 = Integer.parseInt(AABB.get("z2"));

                String material = AABB.get("material");
                BlockPos topLeft = new BlockPos(x1, y1, z1);
                BlockPos bottomRight = new BlockPos(x2, y2, z2);

                this.placeAABB(world, topLeft, bottomRight, material, true);
            }

            // Extract information and use it to place blocks
            for (LinkedTreeMap<String, String> block : blueprint.get("block_list")) {
                int x = Integer.parseInt(block.get("x"));
                int y = 4 + Integer.parseInt(block.get("y")); // Above the AABB floor
                int z = Integer.parseInt(block.get("z"));

                String name = block.get("name");
                String material = block.get("material");
                BlockPos pos = new BlockPos(x, y, z);

                this.placeBlock(world, pos, name, material);
            }

            this.shouldBuild = false;
        } else {
            return;
        }

    }

    /**
     * This method places AABBs in the given world.
     *
     * @param world       - The world in which the AABB is to be built
     * @param topLeft     - top left coordinates of the AABB from the top view of the X-Y plane
     * @param bottomRight - bottom right coordinates of the AABB from the top view of the X-Y plane
     * @param material    - The material the AABB is to be built out of
     * @param isHollow    - Should the AABB be hollow or filled completely.
     */
    private void placeAABB(World world, BlockPos topLeft, BlockPos bottomRight, String material, boolean isHollow) {
        int x1 = topLeft.getX(), y1 = topLeft.getY(), z1 = topLeft.getZ();
        int x2 = bottomRight.getX(), y2 = bottomRight.getY(), z2 = bottomRight.getZ();

        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    if (isHollow) {
                        if (x == x1 || x == x2 || z == z1 || z == z2 || y == 3) { // Places along x-z edges and floor
                            BlockPos pos = new BlockPos(x, y, z);
                            world.setBlockState(pos, this.getBlockState(material));
                        }
                    } else { // Places everywhere
                        BlockPos pos = new BlockPos(x, y, z);
                        world.setBlockState(pos, this.getBlockState(material));
                    }
                }
            }
        }
    }

    /**
     * This method places the block given to it based on the aterial and name
     *
     * @param world    - The world in which the block is to be placed
     * @param pos      - The position to place it in. In case of a door this is the position of the bottom.
     * @param name     - The name of the block. Not the same as the material of the block.
     * @param material - The material of the block.
     */
    private void placeBlock(World world, BlockPos pos, String name, String material) {
        if (name.equals("door")) {
            // Doors are special adn we need to place a bottom and top half
            world.setBlockState(pos, this.getBlockState("door_bottom"));
            BlockPos topPos = new BlockPos(pos.getX(), pos.getY() + 1, pos.getZ());
            world.setBlockState(topPos, this.getBlockState("door_top"));
        } else {
            world.setBlockState(pos, this.getBlockState(material));
        }
    }

    /**
     * This method creates a Map representation of the JSON file from which the world's blueprint is read.
     *
     * @param filename The full path including the filename to the json file in the Minecraft/run directory.
     * @return
     */
    private Map<String, ArrayList<LinkedTreeMap<String, String>>> getBlueprintFromJSON(String filename) {
        // Read File
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(filename));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        // Put into Map
        Gson gson = new Gson();
        Map<String, ArrayList<LinkedTreeMap<String, String>>> blueprint = gson.fromJson(reader, Map.class);
        /*
        In the above map, the top level Map's String key corresponds to aabb_list and block_list while the values of the
        lists are stored in a ArrayList of LinkedTreeMap. Each of these LinkedTreeMap's with string key and value entries represent an
        AABB or a Block with the keys in the LinkedTreeMap corresponding to "x1","y1" etc.
         */

        return blueprint;

    }

    /**
     * Uses the procedural genration JSON nomenclature to return the block state of the right block. If the material key is unkown then
     * the method returns the block state of Quartz as a default.
     *
     * @param material - The name of the block whose default state is to be gotten
     * @return The default block state of the requested block
     */
    private IBlockState getBlockState(String material) {
        if (material.equals("planks")) {
            return Blocks.PLANKS.getDefaultState();
        } else if (material.equals("prismarine")) {
            return Blocks.PRISMARINE.getDefaultState();
        } else if (material.equals("gold")) {
            return Blocks.GOLD_BLOCK.getDefaultState();
        } else if (material.equals("door_top")) {
            return Blocks.DARK_OAK_DOOR.getStateFromMeta(9);
        } else if (material.equals("door_bottom")) {
            return Blocks.DARK_OAK_DOOR.getStateFromMeta(0);
        } else if (material.equals("air")) {
            return Blocks.AIR.getDefaultState();
        } else {
            return Blocks.QUARTZ_BLOCK.getDefaultState();  // For unknown block
        }
    }

    @Override
    protected void createPhases() {
        // No action to be taken
    }

    @Override
    public void init(World world) {
        super.init(world);
    }


    @Override
    protected void beforePhaseTrasition() {
        // No action to be taken
    }

    @Override
    public void setTimeLimitInSeconds(long timeLimitInSeconds) {
        this.timeLimitInSeconds = -1;
    }

    @Override
    public PosAndDirection
    getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
        PosAndDirection positionAndDirection = new PosAndDirection();
        positionAndDirection.setX(new BigDecimal(0));
        positionAndDirection.setY(new BigDecimal(4));
        positionAndDirection.setZ(new BigDecimal(0));
        return positionAndDirection;
    }

    @Override
    protected boolean hasSelfReport() {
        return false;
    }

    @Override
    protected SelfReportContent getSelfReportContent(EntityPlayerMP player,
                                                     World world) {
        return null;
    }

    @Override
    public void goalAchieved(World world, MissionGoal goal) {
        // No action to be taken
    }

    @Override
    protected void onPlayerDeath(EntityPlayer player) {
        this.onTimeOut();
    }
}
