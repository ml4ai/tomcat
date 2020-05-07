package edu.arizona.tomcat.Events;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.util.Arrays;
import java.util.List;
import net.minecraftforge.event.CommandEvent;

public class DogBarkEvent extends Event {

    /**
     * The parameters of the command that produces the dog bark chat on the
     * screen. We return the raw array of strings for now, postponing any JSON
     * processing.
     */
    private int numberOfBarks;

    public DogBarkEvent(CommandEvent event) {
        String parameters = String.join(" ", event.getParameters());
        this.numberOfBarks = parameters.contains("woof woof") ? 2 : 1;
    }
}
