package edu.arizona.tomcat.Events;
import edu.arizona.tomcat.Events.Event;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class AttackEvent extends Event {

  private String eventName;
  private String timestamp;
  private String coordinates;
  private String playerHealth;
  private String enemyName;
  private String enemyHealth;

  public AttackEvent(AttackEntityEvent event) {
    EntityPlayer playerIn = event.getEntityPlayer();
    Entity target = event.getTarget();

    if (target instanceof EntityMob) {

      EntityMob enemy = (EntityMob)target;

      BlockPos pos = new BlockPos(
          event.getTarget().posX,
          event.getTarget().posY,
          event.getTarget().posZ); // Event occurrence is location of target

      // Player and Enemy Info
      String playerName = playerIn.getDisplayNameString();
      String playerHealth = playerIn.getHealth() + "/" + playerIn.getMaxHealth();
      String enemyName = enemy.getName();
      String enemyHealth = "0.0"
        + "/" + enemy.getMaxHealth();

      String eventName = "enemy_killed";

      if (enemy.isEntityAlive()) {
        eventName = "enemy_attacked";
        enemyHealth = enemy.getHealth() + "/" + enemy.getMaxHealth();
      }

      // Logistics Info
      DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
      Date date = new Date();

      String timestamp = dateFormat.format(date); // Date and Time
      int x = pos.getX(), y = pos.getY(), z = pos.getZ(); // event coordinates
      String coordinates = "X: " + x + " "
        + "Y: " + y + " "
        + "Z: " + z;

      this.eventName = eventName;
      this.timestamp = timestamp;
      this.coordinates = coordinates;
      this.playerHealth = playerHealth;
      this.enemyName = enemyName;
      this.enemyHealth = enemyHealth;
    }
  }
}
