package edu.arizona.tomcat.World;
import net.minecraft.item.ItemStack;
/** A wrapper class for Minecraft ItemStack objects for nicer Gson
 * serialization */

public class ItemStackWrapper {
    private String item;
    private int count;

    public ItemStackWrapper(ItemStack itemStack) {
        this.item = itemStack.getItem().getUnlocalizedName();
        this.count = itemStack.getCount();
    }
}
