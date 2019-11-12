package edu.arizona.tomcat.Mission.gui;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Mission.Client.SARClientMission;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Gui;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent.ElementType;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

public class GUIOverlayVillagersSaved extends Gui {
	
	private final ResourceLocation bar = new ResourceLocation(MalmoMod.MODID, "textures/gui/hpbar.png");
	private final static int TEXTURE_WIDTH = 102;
	private final static int TEXTURE_HEIGHT = 8;
	
	private int totalNumberOfVillagers;
	
	public GUIOverlayVillagersSaved(int totalNumberOfVillagers) {
		this.totalNumberOfVillagers = totalNumberOfVillagers;
	}
	
	@SubscribeEvent
	public void renderOverlay(RenderGameOverlayEvent event) {
		if (event.getType() == ElementType.TEXT) {
			SARClientMission clientMission = (SARClientMission) MalmoMod.instance.getClient().getTomcatClientMission();
			float fractionOfSavedVillagers = (float) clientMission.getNumberOfSavedVillagers() / (float) this.totalNumberOfVillagers;
			int currentWidth = (int) (fractionOfSavedVillagers * TEXTURE_WIDTH);
			Minecraft.getMinecraft().renderEngine.bindTexture(this.bar);
			drawTexturedModalRect(0, 0, 0, 0, TEXTURE_WIDTH, TEXTURE_HEIGHT);
			drawTexturedModalRect(0, 0, 0, TEXTURE_HEIGHT, currentWidth, TEXTURE_HEIGHT);
		}
	}
}
