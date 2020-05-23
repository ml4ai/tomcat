package edu.arizona.tomcat.Events;
import net.minecraftforge.event.entity.living.LivingEquipmentChangeEvent;

public class EquipmentChange extends Event {
    private String from;
    private String to;
    private String slot;

    /** A constructor for equipment change events. */
    public EquipmentChange(LivingEquipmentChangeEvent event) {
        this.from = event.getFrom().toString();
        this.to= event.getTo().toString();
        this.slot= event.getSlot().toString();
    }
}
