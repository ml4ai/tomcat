package edu.arizona.tomcat.World;

import net.minecraft.entity.Entity;

/**
 * A class to represent the velocity vector of an entity or block in Minecraft
 * */
public class Velocity {
    private double x;
    private double y;
    private double z;

    /** Constructor that takes an entity as an argument. */
    public Velocity(Entity entity) {
        this.x = entity.motionX;
        this.y = entity.motionY;
        this.z = entity.motionZ;
    }
}
