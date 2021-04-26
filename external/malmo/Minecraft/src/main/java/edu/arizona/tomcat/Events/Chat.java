package edu.arizona.tomcat.Events;

import net.minecraftforge.event.ServerChatEvent;

public class Chat extends Event {

    /** The name of the player involved in the event. */
    private String playerName;
    /** The message typed in the chat box */
    private String message;

    public Chat(ServerChatEvent event) {
        this.playerName = event.getPlayer().getName();
        this.message = event.getMessage();
    }


}
