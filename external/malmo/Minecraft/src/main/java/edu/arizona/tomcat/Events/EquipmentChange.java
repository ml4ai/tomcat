package edu.arizona.tomcat.Events;
import edu.arizona.tomcat.World.ItemStackWrapper;
import net.minecraftforge.event.entity.living.LivingEquipmentChangeEvent;

public class EquipmentChange extends Event {
    private ItemStackWrapper from;
    private ItemStackWrapper to;
    private String slot;
    private String entity_name;

    /** A constructor for equipment change events. */
    public EquipmentChange(LivingEquipmentChangeEvent event) {
        this.from = new ItemStackWrapper(event.getFrom());
        this.to = new ItemStackWrapper(event.getTo());
        this.slot = event.getSlot().toString();
        this.entity_name = event.getEntity().getName();
    }
}
