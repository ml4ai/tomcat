package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import edu.arizona.tomcat.World.Velocity;
import java.util.UUID;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraftforge.event.entity.player.AttackEntityEvent;

public class MobAttacked extends Event {

    /** The position of the target mob involved in the event. */
    private Position targetPosition;
    /** The velocity of the target mob involved in the event. */
    private Velocity targetVelocity;
    /** The name of the player involved in the event. */
    private String playerName;
    /** The type of the target mob (for example Skeleton, Zombie, etc.) */
    private String targetType;
    /** The unique ID of the target mob. */
    private UUID targetId;

    /** The health of the target *before* the event. */
    private double targetHealth;

    public MobAttacked(AttackEntityEvent event) {
        EntityMob target = (EntityMob)event.getTarget();
        EntityPlayer player = event.getEntityPlayer();
        this.playerName = player.getDisplayNameString();
        this.targetType = target.getClass().getName();
        this.targetId = target.getUniqueID();
        this.targetHealth = target.getHealth();
        this.targetPosition = new Position(target);
        this.targetVelocity = new Velocity(target);
    }
}
