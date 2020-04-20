package edu.arizona.tomcat.Utils;

public class BlockDiscreteEvent {

  private String eventName;
  private String timestamp;
  private String coordinates;

  public BlockDiscreteEvent(String eventName,
                            String timestamp,
                            String coordinates) {
    this.eventName = eventName;
    this.timestamp = timestamp;
    this.coordinates = coordinates;
  }
}
