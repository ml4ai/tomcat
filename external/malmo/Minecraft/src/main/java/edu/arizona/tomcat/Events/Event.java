package edu.arizona.tomcat.Events;
import edu.arizona.tomcat.Utils.TimeStamper;

public class Event { 
  private String timestamp;
  protected String eventType = "event";
  public Event() {
    this.timestamp = TimeStamper.getTimeStamp();
  }
}
