package edu.arizona.tomcat.Mission.gui;

import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiLabel;;
public class UnshadowedLabel extends GuiLabel {
	
	public UnshadowedLabel(FontRenderer fontRendererObj, int id, int x, int y, int width, int height, int foregroundColor) {
		super(fontRendererObj, id, x, y, width, height, foregroundColor);
	}
	
	@Override
	public void drawCenteredString(FontRenderer fontRendererIn, String text, int x, int y, int color) {
		fontRendererIn.drawString(text, (float)(x - fontRendererIn.getStringWidth(text) / 2), (float)y, color, false);
	}
	
	@Override
	public void drawString(FontRenderer fontRendererIn, String text, int x, int y, int color) {
		fontRendererIn.drawString(text, (float)x, (float)y, color, false);
	}

}
