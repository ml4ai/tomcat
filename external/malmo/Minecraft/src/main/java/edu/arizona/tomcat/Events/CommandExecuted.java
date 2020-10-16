package edu.arizona.tomcat.Events;

import net.minecraftforge.event.CommandEvent;

public class CommandExecuted extends Event {

    /**
     * Constructor grabs the command name, the issuer of the command, and the
     * parameters for the command.
     */
    private String commandName = "";
    private String[] parameters = null;
    private String sender = "";

    public CommandExecuted(CommandEvent event) {
        this.commandName = event.getCommand().getName();
        this.parameters = event.getParameters();
        this.sender = event.getSender().getName();
    }
}
