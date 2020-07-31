package edu.arizona.tomcat.Events;

import net.minecraft.entity.Entity;
import net.minecraftforge.event.entity.PlaySoundAtEntityEvent;

public class SoundPlayingEvent extends Event {

    public String soundName;
    public String soundCategory;

    public SoundPlayingEvent(PlaySoundAtEntityEvent event) {
        soundName = event.getSound().getSoundName().toString();
        soundCategory = event.getCategory().getName();
    }
}