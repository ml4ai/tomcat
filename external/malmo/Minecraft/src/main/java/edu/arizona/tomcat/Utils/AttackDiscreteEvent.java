package edu.arizona.tomcat.Utils;

public class AttackDiscreteEvent {

  private String eventName;
  private String timestamp;
  private String coordinates;
  private String playerHealth;
  private String enemyName;
  private String enemyHealth;

  public AttackDiscreteEvent(String eventName,
                             String timestamp,
                             String coordinates,
                             String playerHealth,
                             String enemyName,
                             String enemyHealth) {
    this.eventName = eventName;
    this.timestamp = timestamp;
    this.coordinates = coordinates;
    this.playerHealth = playerHealth;
    this.enemyName = enemyName;
    this.enemyHealth = enemyHealth;
  }
}
