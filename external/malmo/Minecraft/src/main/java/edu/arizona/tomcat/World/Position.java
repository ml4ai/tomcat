package edu.arizona.tomcat.World;

import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;

/** 
 * A class to represent the position vector of an entity or block in Minecraft
 * */
public class Position {
  private double x;
  private double y;
  private double z;

  /** Constructor that takes an entity as an argument. */
  public Position(Entity entity) {
    this.x = entity.posX;
    this.y = entity.posY;
    this.z = entity.posZ;
  }

  /** Constructor that takes an entity as an argument. */
  public Position(BlockPos pos) {
    this.x = pos.getX();
    this.y = pos.getY();
    this.z = pos.getZ();
  }

}
