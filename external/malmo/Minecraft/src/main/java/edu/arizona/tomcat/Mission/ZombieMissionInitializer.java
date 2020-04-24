package edu.arizona.tomcat.Mission;

import com.google.common.base.Predicate;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.Mission.Mission.DIFFICULTY;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Building;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.DrawingHandler;
import edu.arizona.tomcat.World.TomcatEntity;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class ZombieMissionInitializer extends MissionInitializer {

    private static double PROB_SKELETON =
        0.25; // Probability of spawning a skeleton instead of a zombie
    private static ArrayList<Building> singleRoomBuildings;
    private static ArrayList<Building> multiRoomBuildings;

    private UUID[] villagersIds;
    private DrawingHandler drawingHandler;

    double probSingleRoomBuilding;
    double probMainRoom;
    double probArmoredEnemy;
    double probEnemyCloning;

    /**
     * Constructor
     * @param levelOfDifficulty
     */
    public ZombieMissionInitializer(DIFFICULTY levelOfDifficulty,
                                    UUID[] villagersIds,
                                    DrawingHandler drawingHandler) {
        this.setProbabilities(levelOfDifficulty);
        this.villagersIds = villagersIds;
        this.drawingHandler = drawingHandler;
        this.fillBuildingList();
    }

    /**
     * Define probabilities based onf the level of difficulty of the mission
     * @param levelOfDifficulty
     */
    private void setProbabilities(DIFFICULTY levelOfDifficulty) {
        switch (levelOfDifficulty) {
        case EASY:
            probSingleRoomBuilding = 0.5;
            probMainRoom = 0.5;
            probArmoredEnemy = 0;
            probEnemyCloning = 0;
            break;
        case MEDIUM:
            probSingleRoomBuilding = 0.25;
            probMainRoom = 0.25;
            probArmoredEnemy = 0.5;
            probEnemyCloning = 0.5;
            break;
        case HARD:
            probSingleRoomBuilding = 0.05;
            probMainRoom = 0.05;
            probArmoredEnemy = 0.75;
            probEnemyCloning = 0.75;
            break;

        default:
            // There's no default case
            break;
        }
    }

    /**
     * Fills the list of buildings of the Zombie mission
     * @return
     */
    private void fillBuildingList() {
        singleRoomBuildings = new ArrayList<Building>();
        singleRoomBuildings.add(new Building(93, 64, 53));
        singleRoomBuildings.add(new Building(46, 64, 47));
        singleRoomBuildings.add(new Building(52, 67, 89));

        multiRoomBuildings = new ArrayList<Building>();
        Building building = new Building(57, 64, 61);
        building.addRoom(63, 64, 63);
        multiRoomBuildings.add(building);

        building = new Building(72, 64, 75);
        building.addRoom(69, 64, 81);
        multiRoomBuildings.add(building);

        building = new Building(88, 64, 87);
        building.addRoom(94, 64, 90);
        multiRoomBuildings.add(building);
    }

    @Override
    protected void updateWorldUponInit(World world) {
        this.spawnEntities(world);
        this.equipPlayer(world);
        this.equipEnemies(world);
    }

    private void spawnEntities(World world) {
        try {
            Random random = new Random();
            Drawing drawing = new Drawing();

            ArrayList<Building> availableSingleRoomBuildings =
                new ArrayList<Building>(singleRoomBuildings);
            ArrayList<Building> availableMultiRoomBuildings =
                new ArrayList<Building>(multiRoomBuildings);
            ArrayList<Building> remainingAvailableBuildings =
                new ArrayList<Building>();
            Collections.shuffle(availableSingleRoomBuildings);
            Collections.shuffle(availableMultiRoomBuildings);

            for (int i = 0; i < this.villagersIds.length; i++) {
                Building building;
                int room;

                if ((random.nextDouble() < this.probSingleRoomBuilding &&
                     !availableSingleRoomBuildings.isEmpty()) ||
                    availableMultiRoomBuildings.isEmpty()) {
                    building = availableSingleRoomBuildings.remove(0);
                    room = 0;
                }
                else {
                    building = availableMultiRoomBuildings.remove(0);
                    remainingAvailableBuildings.add(building);

                    if (random.nextDouble() < this.probMainRoom) {
                        room = 0;
                    }
                    else {
                        if (building.getNumberOfRooms() > 2) {
                            room = random.nextInt(building.getNumberOfRooms() -
                                                  2) +
                                   1;
                        }
                        else {
                            room = 1;
                        }
                    }
                }

                BlockPos roomCoordinates = building.getRoomCoordinates(room);
                building.setRoomOccupied(room);

                this.spawnVillager(
                    drawing, this.villagersIds[i], roomCoordinates);
            }

            remainingAvailableBuildings.addAll(availableSingleRoomBuildings);
            remainingAvailableBuildings.addAll(availableMultiRoomBuildings);

            // Fill remaining buildings and rooms with enemies
            for (Building building : remainingAvailableBuildings) {
                Iterator<BlockPos> roomsCoordinates =
                    building.getUnoccupiedRooms();
                while (roomsCoordinates.hasNext()) {
                    BlockPos coordinates = roomsCoordinates.next();

                    this.spawnEnemy(drawing, coordinates);

                    // Create another enemy in the same room displaced by one
                    // block in the x direction
                    if (random.nextDouble() < this.probEnemyCloning) {
                        coordinates.add(0, 0, 1);
                        this.spawnEnemy(drawing, coordinates);
                    }
                }
            }

            this.drawingHandler.draw(world, drawing);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void spawnVillager(Drawing drawing, UUID id, BlockPos coordinates) {
        int x = coordinates.getX();
        int y = coordinates.getY();
        int z = coordinates.getZ();

        TomcatEntity villager =
            new TomcatEntity(id, x, y, z, EntityTypes.VILLAGER);
        drawing.addObject(villager);
    }

    private void spawnEnemy(Drawing drawing, BlockPos coordinates) {
        Random random = new Random();
        int x = coordinates.getX();
        int y = coordinates.getY();
        int z = coordinates.getZ();
        EntityTypes type = EntityTypes.ZOMBIE;

        if (random.nextDouble() < PROB_SKELETON) {
            type = EntityTypes.SKELETON;
        }

        TomcatEntity enemy = new TomcatEntity(x, y, z, type);
        drawing.addObject(enemy);
    }

    private void equipPlayer(World world) {
        for (EntityPlayerMP player : MinecraftServerHelper.getPlayers()) {
            InventoryHandler.addItemToMainHand(player, ItemType.STONE_AXE);
        }
    }

    private void equipEnemies(World world) {
        Random random = new Random();
        List<EntityMob> enemies =
            world.getEntities(EntityMob.class, new Predicate<EntityMob>() {
                @Override
                public boolean apply(EntityMob input) {
                    return true;
                }
            });

        for (Entity enemy : enemies) {
            if (random.nextDouble() < probArmoredEnemy) {
                InventoryHandler.addItemToMainHand(enemy, ItemType.STONE_SWORD);
            }
        }
    }
}
