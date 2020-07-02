package edu.arizona.tomcat.Utils;

import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

public class WorldReader {
   private Map<BlockPos, IBlockState> map;

    public WorldReader(String filename){

        this.map = new LinkedHashMap<BlockPos, IBlockState>();
        this.initMap(filename);
    }

    public Map<BlockPos, IBlockState> getMap(){
        return this.map;
    }

    private void initMap(String filename){

        // Read file
        Scanner file = null;

        try {
            file = new Scanner((new File(filename)));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        // Use file
        while(file.hasNextLine()){
          String line = file.nextLine();
          String[] blockEntry = line.split("\t");

          int x= Integer.parseInt(blockEntry[0]);
          int y= Integer.parseInt(blockEntry[1]);
          int z= Integer.parseInt(blockEntry[2]);
          BlockPos pos = new BlockPos(x,y,z);

          String material = blockEntry[3];
          if (material.equals("door")) {
                // Doors are special adn we need to place a bottom and top half
                IBlockState doorBottom = this.getBlockState("door_bottom");

                BlockPos topPos = new BlockPos(pos.getX(), pos.getY() + 1, pos.getZ());
                IBlockState doorTop = this.getBlockState("door_top");

                this.map.put(pos,doorBottom);
                this.map.put(topPos,doorTop);
          }else {
                IBlockState state = getBlockState(material);
                this.map.put(pos, state);
          }
        }
    }

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
}
