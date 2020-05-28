package edu.arizona.tomcat.Events;
import java.time.Clock;

public class Event {
    private String eventType = this.getClass().getName();
    private String timestamp = Clock.systemUTC().instant().toString();
}
