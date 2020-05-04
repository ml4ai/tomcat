package edu.arizona.tomcat.Events;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import net.minecraftforge.event.CommandEvent;
import java.util.List;
import java.util.Arrays;

public class DogBarkEvent extends Event {

    /** The parameters of the command that produces the dog bark chat on the
     * screen. We return the raw array of strings for now, postponing any JSON
     * processing. */
    private String[] parameters;
    
    public DogBarkEvent(CommandEvent event) {
        this.parameters = event.getParameters();
    }
}
