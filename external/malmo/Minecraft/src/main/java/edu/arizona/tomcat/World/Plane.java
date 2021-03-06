package edu.arizona.tomcat.World;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.DrawCuboid;

public class Plane extends CompositeDrawingObject {

    /**
     * Constructor
     * @param x - Bottom-left position of the plane in the x axis
     * @param y - Bottom-left position of the plane in the y axis
     * @param z - Bottom-left position of the plane in the z axis
     * @param width - Width of the plane
     * @param height - Height of the plane
     * @param depth - Depth of the plane
     * @param type - Type of the block
     */
    public Plane(
        int x, int y, int z, int width, int height, int depth, BlockType type) {
        super();
        this.createPlane(x, y, z, width, height, depth, type);
    }

    /**
     * Creates a plane and adds it to the list of objects of the drawing
     * @param x - Bottom-left position of the plane in the x axis
     * @param y - Bottom-left position of the plane in the y axis
     * @param z - Bottom-left position of the plane in the z axis
     * @param width - Width of the plane
     * @param height - Height of the plane
     * @param depth - Depth of the plane
     * @param type - Type of the block
     * @return
     */
    public void createPlane(
        int x, int y, int z, int width, int height, int depth, BlockType type) {
        DrawCuboid cuboid = new DrawCuboid();
        cuboid.setX1(x);
        cuboid.setY1(y);
        cuboid.setZ1(z);
        cuboid.setX2(x - width + 1);
        cuboid.setY2(y + height - 1);
        cuboid.setZ2(z + depth - 1);
        cuboid.setType(type);
        this.malmoDrawObjects.add(cuboid);
    }

    public boolean intersects(int x, int y, int z) {
        DrawCuboid cuboid = (DrawCuboid)this.malmoDrawObjects.get(0);
        boolean intersects = false;

        if (Math.abs(x) >= Math.abs(cuboid.getX1()) &&
            Math.abs(x) <= Math.abs(cuboid.getX2())) {
            if (Math.abs(y) >= Math.abs(cuboid.getY1()) &&
                Math.abs(y) <= Math.abs(cuboid.getY2())) {
                if (Math.abs(z) >= Math.abs(cuboid.getZ1()) &&
                    Math.abs(z) <= Math.abs(cuboid.getZ2())) {
                    intersects = true;
                }
            }
        }

        return intersects;
    }
}
