package com.microsoft.Malmo.ASISTBlocks;

import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;
import net.minecraftforge.fml.common.registry.GameRegistry;

/**
 * This file registers all the blocks we want to register for our mod. To register a new block, do the following:
 * <p>
 * 1) Create a new class that extends Block (directly or indirectly) which defines your new Block type
 * 2) Instantiate this block in the init() method below
 * 3) Copy paste the code in the indicated methods and change the instance name in the copied lines to your new block name.
 * <p>
 * Note: you must register the block both as a Block AND as an Item so Minecraft knows that you can hold it in your inventory
 * before placing it into the world.
 * <p>
 * The registration methods used here are called in the MalmoMod PreInit() and MalmoModClient init() methods. Nothing needs to be modified there.
 * <p>
 * Remember to add a line to the en_US.lang file in resources/assets/malmomod/lang folder.
 */
public class ModBlocks {

    public static Block asistButtonBlock;

    /**
     * This method initializes the new block created.
     */
    public static void init() {
        asistButtonBlock = new BlockAsistButton();
    }

    /**
     * This method calls a helper function to register the Minecraft block. You can call this from outside this
     * class, but for convenience, all the registration code should be kept within this class
     */
    public static void register() {
        registerBlock(asistButtonBlock);

        //Add copied lines under this if necessary
    }

    /**
     * This methdd registers the given block instance in Minecraft. It registers it as both a block and an
     * item so it will show up in the inventory. DO NOT use this to register only Items which don't have block counterparts.
     * <p>
     * All blocks have item counterparts but not all Items have blocks= counterparts.
     *
     * @param block - The block object to be registered.
     */
    private static void registerBlock(Block block) {
        GameRegistry.register(block);
        ItemBlock item = new ItemBlock(block);
        item.setRegistryName(block.getRegistryName());
        GameRegistry.register(item);
    }

    /**
     * This method is used in the MalmoModClient to ensure that teh client knows about the block we created and can render it.
     * Remember to add texture JSONs or this will only show up as a purple and black cube in Minecraft.
     */
    public static void registerRenders() {
        registerRender(asistButtonBlock);

        //Add copied lines under this if necessary
    }

    /**
     * The actual method that registers the block renders for the block models on the client side.
     *
     * @param block - The block whose model is to be registered.
     */
    private static void registerRender(Block block) {
        Minecraft.getMinecraft().getRenderItem().getItemModelMesher().register(Item.getItemFromBlock(block), 0, new ModelResourceLocation(block.getRegistryName(), "inventory"));
    }
}
