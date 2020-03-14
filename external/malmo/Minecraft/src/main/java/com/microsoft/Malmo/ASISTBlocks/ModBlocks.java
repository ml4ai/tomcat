package com.microsoft.Malmo.ASISTBlocks;

import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.ModelResourceLocation;
import net.minecraft.item.Item;
import net.minecraftforge.fml.common.registry.GameRegistry;

public class ModBlocks {

    public static Block asistStoneBlock;

    public static void init() {
        asistStoneBlock = new ASISTStoneButton();
    }

    public static void register() {
        GameRegistry.register(asistStoneBlock);
    }

    public static void registerRenders() {
        registerRender(asistStoneBlock);
    }

    private static void registerRender(Block block) {
        Minecraft.getMinecraft().getRenderItem().getItemModelMesher().register(Item.getItemFromBlock(block), 0, new ModelResourceLocation(block.getRegistryName(), "inventory"));
    }
}
