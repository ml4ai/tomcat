package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import edu.arizona.tomcat.World.Velocity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraftforge.event.entity.player.AttackEntityEvent;

import java.util.UUID;

public class MobAttacked extends Event {

  private String targetName;
  private Position targetPosition;
  private Velocity targetVelocity;
  private String playerName;
  private String targetType;
  private UUID targetId;

  // Note: targetHealth is the health BEFORE the attack event.
  private double targetHealth;

  public MobAttacked(AttackEntityEvent event) {
    EntityMob target = (EntityMob)event.getTarget();
    this.eventType = "mob_attacked";
    EntityPlayer player = event.getEntityPlayer();
    this.playerName = player.getDisplayNameString();
    this.targetType = target.getName();
    this.targetId = target.getUniqueID();
    this.targetHealth = target.getHealth();
    this.targetPosition = new Position(target);
    this.targetVelocity = new Velocity(target);
  }
}
