package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.DrawBlock;

import net.minecraft.util.math.BlockPos;

public class TomcatBlock extends CompositeDrawingObject {

  /**
   * Constructor
   * @param x - Position of the block in the x axis
   * @param y - Position of the block in the y axis
   * @param z - Position of the block in the z axis
   * @param type - Type of the block
   */
  public TomcatBlock(int x, int y, int z, BlockType type) {
    super();
    this.createBlock(x, y, z, type);
  }

  /**
   * Creates a block and adds it to the list of objects of the drawing
   * @param x - Position of the block in the x axis
   * @param y - Position of the block in the y axis
   * @param z - Position of the block in the z axis
   * @param type - Type of the block
   * @return
   */
  public void createBlock(int x, int y, int z, BlockType type) {
    DrawBlock block = new DrawBlock();
    block.setType(type);
    block.setX(x);
    block.setY(y);
    block.setZ(z);
    this.malmoDrawObjects.add(block);
  }

  /**
   * Retrieves the block position
   * @return
   */
  public BlockPos getPosition() {
    DrawBlock block = (DrawBlock)this.malmoDrawObjects.get(0);
    return new BlockPos(block.getX(), block.getY(), block.getZ());
  }
}
