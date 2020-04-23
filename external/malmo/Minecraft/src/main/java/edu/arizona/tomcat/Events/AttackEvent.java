package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.Events.Event;
import edu.arizona.tomcat.World.*;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class AttackEvent extends Event {

  private String targetName;
  private Position targetPosition;
  private Velocity targetVelocity;
  private String playerName;
  private Class targetType;

  // Adarsh: I think these are the health values of the player and target
  // *after* the attack, but I am not sure. TODO Confirm this.
  private double targetHealth;

  public AttackEvent(AttackEntityEvent event) {
    EntityMob target = (EntityMob)event.getTarget();
    this.eventType = "attack";
    EntityPlayer player = event.getEntityPlayer();
    this.playerName = player.getDisplayNameString();
    this.targetName = target.getName();
    this.targetHealth = target.getHealth();
    this.targetType = target.getClass();
    this.targetPosition = new Position(target);
    this.targetVelocity = new Velocity(target);
  }
}
