package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import edu.arizona.tomcat.World.Velocity;
import java.util.UUID;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

public class EntityInteraction extends Event {
    private String playerName;
    private String targetType;

    /** The unique ID of the target entity. */
    private UUID targetId;

    private Position targetPosition;
    private Velocity targetVelocity;

    /** The item that the player was holding at the time of the interaction. */
    private String itemHeld;

    /**
     * A constructor for general entity interaction events (when a player
     * right-clicks an entity).
     */
    public EntityInteraction(PlayerInteractEvent.EntityInteract event) {
        this.playerName = event.getEntityPlayer().getDisplayNameString();
        Entity target = event.getTarget();
        this.targetType = target.getClass().getName();
        this.targetId = target.getUniqueID();
        this.targetPosition = new Position(target);
        this.targetVelocity = new Velocity(target);
        this.itemHeld = event.getItemStack().getDisplayName();
    }
}
