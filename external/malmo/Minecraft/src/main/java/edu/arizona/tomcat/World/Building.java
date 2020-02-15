package edu.arizona.tomcat.World;


public class Building {
    private enum RoomStatus {FILLED, EMPTY}

    ;
    private int[] mainRoomCoordinates;
    RoomStatus mainRoomStatus;

    public Building(int x, int y, int z) {
        this.mainRoomCoordinates = new int[3];
        this.mainRoomCoordinates[0] = x;
        this.mainRoomCoordinates[1] = y;
        this.mainRoomCoordinates[2] = z;
        this.mainRoomStatus = RoomStatus.EMPTY;
    }

    public int getX() {
        return this.mainRoomCoordinates[0];
    }

    public int getY() {
        return this.mainRoomCoordinates[1];
    }

    public int getZ() {
        return this.mainRoomCoordinates[2];
    }

    public void setMainRoomStatusToFilled() {
        this.mainRoomStatus = RoomStatus.FILLED;
    }

    public void setMainRoomStatusToEmpty() {
        this.mainRoomStatus = RoomStatus.EMPTY;
    }

    public boolean mainRoomIsFilled() {
        if (this.mainRoomStatus.equals(RoomStatus.FILLED)) {
            return true;
        } else {
            return false;
        }
    }
}
