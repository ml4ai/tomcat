package edu.arizona.tomcat.Utils;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.ItemType;
import com.microsoft.Malmo.Utils.MinecraftTypeHelper;

import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public class InventoryHandler {
	
	/**
	 * Adds an item to the player's main inventory
	 * @param type - Item type
	 * @param quantity - Number of items to be stored in the inventory 
	 */
	public static void addItemToInventory(ItemType type, int quantity) {
		Item item = MinecraftTypeHelper.ParseItemType(type.value(), false);
		ItemStack itemStack = new ItemStack(item);
		Minecraft.getMinecraft().player.inventory.addItemStackToInventory(itemStack);
	}
	
	/**
	 * Adds a block to the player's main inventory
	 * @param type - Block type
	 * @param quantity - Number of blocks to be stored in the inventory 
	 */
	public static void addBlockToInventory(BlockType type, int quantity) {
		Block block = MinecraftTypeHelper.ParseBlockType(type.value()).getBlock();
		ItemStack itemStack = new ItemStack(block);
		Minecraft.getMinecraft().player.inventory.addItemStackToInventory(itemStack);		
	}
	
}
