package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.EntityTypes;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import java.util.UUID;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class ApproachEntityGoal extends MissionGoal {

    private UUID uniqueId;
    private EntityTypes entityType;
    private int distance;
    private Entity entity;

    /**
     * Constructor
     * @param uniqueId - Id that uniquely identifies an entity
     * @param range - Distance between the player and the entity to the
     * accomplishment of the goal
     */
    public ApproachEntityGoal(UUID uniqueId, int distance) {
        this.uniqueId = uniqueId;
        this.distance = distance;
    }

    /**
     * Constructor
     * @param entityType - Type of the entity
     * @param range - Distance between the player and the entity to the
     * accomplishment of the goal
     */
    public ApproachEntityGoal(EntityTypes entityType, int distance) {
        this.entityType = entityType;
        this.distance = distance;
    }

    @Override
    public void updateGoalStatus(World world) {
        for (Entity entity : world.getLoadedEntityList()) {
            if (shouldBeChecked(entity)) {
                for (EntityPlayerMP player : MinecraftServerHelper.getServer()
                                                 .getPlayerList()
                                                 .getPlayers()) {
                    this.goalAchieved = player.getDistanceToEntity(entity) <
                                        Math.pow(this.distance, 2);
                    if (this.goalAchieved) {
                        this.player = player;
                        break;
                    }
                }

                if (this.goalAchieved) {
                    this.entity = entity;
                    break;
                }
            }
        }
    }

    /**
     * Check if an entity is of interest for this goal
     * @param entity - Minecraft entity
     * @return
     */
    private boolean shouldBeChecked(Entity entity) {
        boolean shouldBeChecked = false;

        if (this.uniqueId != null &&
            entity.getUniqueID().toString().equals(this.uniqueId.toString())) {
            shouldBeChecked = true;
        }
        else if (this.entityType != null &&
                 entity.getName().equals(this.entityType.value())) {
            shouldBeChecked = true;
        }

        return shouldBeChecked;
    }

    /**
     * Retrieves the entity approached by the player
     * @return
     */
    public Entity getEntity() { return this.entity; }
}
