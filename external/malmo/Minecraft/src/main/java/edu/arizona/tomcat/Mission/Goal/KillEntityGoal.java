package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.EntityTypes;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.entity.Entity;
import net.minecraft.world.World;

public class KillEntityGoal extends MissionGoal {
    private UUID uniqueId;
    private EntityTypes entityType;
    private ArrayList<Entity> trackedEntities;

    /**
     * Constructor
     * @param uniqueId - Id that uniquely identifies an entity
     */
    public KillEntityGoal(UUID uniqueId) { this.uniqueId = uniqueId; }

    /**
     * Constructor
     * @param entityType - Type of the entity
     */
    public KillEntityGoal(EntityTypes entityType) {
        this.entityType = entityType;
    }

    @Override
    public void updateGoalStatus(World world) {
        if (this.trackedEntities == null) {
            this.storeObservableEntities(world);
        }
        else {
            this.checkGoalStatus(world);
        }
    }

    /**
     * Add entities of interest to the set of tracked entities. These are the
     * entities alive in the beginning of the phase where this goal is set.
     * @param world - Minecraft world
     */
    private void storeObservableEntities(World world) {
        this.trackedEntities = new ArrayList<Entity>();
        for (Entity entity : world.getLoadedEntityList()) {
            if (this.uniqueId != null && entity.getUniqueID().toString().equals(
                                             this.uniqueId.toString())) {
                this.trackedEntities.add(entity);
            }
            else if (this.entityType != null &&
                     entity.getName().equals(this.entityType.value())) {
                this.trackedEntities.add(entity);
            }
        }
    }

    /**
     * Check whether the goal was accomplished or not
     * @param world - Minecraft world
     */
    private void checkGoalStatus(World world) {
        this.goalAchieved = this.isAnyEntityKilled(world);
    }

    /**
     * Check whether any of the tracked entities was killed
     * @param world - Minecraft world
     */
    private boolean isAnyEntityKilled(World world) {
        boolean someEntityKilled = false;
        Set<Entity> aliveEntities = new HashSet<Entity>();
        aliveEntities.addAll(world.getLoadedEntityList());

        for (Entity entity : this.trackedEntities) {
            if (!aliveEntities.contains(entity)) {
                someEntityKilled = true;
                break;
            }
        }

        return someEntityKilled;
    }
}
