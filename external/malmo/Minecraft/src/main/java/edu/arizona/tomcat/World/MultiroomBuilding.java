package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.List;

public class MultiroomBuilding extends Building {

    private List<int[]> additionalRooms;
    private List<int[]> filledAdditionalRooms;

    public MultiroomBuilding(int x, int y, int z) {
        super(x, y, z);
        this.additionalRooms = new ArrayList<int[]>();
        this.filledAdditionalRooms = new ArrayList<int[]>();
    }

    public void addRoom(int x, int y, int z) {
        int[] newRoom = {x, y, z};
        this.additionalRooms.add(newRoom);
    }

    public List<int[]> getAdditionalRooms() {
        return this.additionalRooms;
    }

    public List<int[]> getFilledAdditionalRooms() {
        return this.filledAdditionalRooms;
    }

    public int getNumberOfAdditionalRooms() {
        return this.additionalRooms.size();
    }

    public void markRoomAsFilled(int[] coordinate) {
        int[] mainRoomCoord = {this.getX(), this.getY(), this.getZ()};
        if (coordinate.equals(mainRoomCoord)) {
            this.setMainRoomStatusToFilled();
        } else {
            this.filledAdditionalRooms.add(coordinate);
        }
    }

}
