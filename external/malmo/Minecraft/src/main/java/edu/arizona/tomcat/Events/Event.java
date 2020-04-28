package edu.arizona.tomcat.Events;
import edu.arizona.tomcat.Utils.TimeStamper;

public class Event {
    private String eventType;
    private String timestamp;
    public Event() {
        this.eventType = this.getClass().getName();
        this.timestamp = TimeStamper.getTimeStamp();
    }
}
