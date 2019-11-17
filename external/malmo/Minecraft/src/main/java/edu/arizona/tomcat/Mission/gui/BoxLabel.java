package edu.arizona.tomcat.Mission.gui;

import java.awt.Point;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiLabel;;
public class BoxLabel extends GuiLabel {
	
	public final static int COLOR_WHITE = -1;
	public final static int COLOR_BLACK = 0;
	
	private final static int PADDING = 0; 
	private final static int BORDER_COLOR = COLOR_BLACK;
	private int backgroundColor; 

	public BoxLabel(FontRenderer fontRendererObj, int id, int x, int y, int width, int height, int foregroundColor, int backgroundColor) {
		super(fontRendererObj, id, x, y, width, height, foregroundColor);
		this.backgroundColor = backgroundColor;
	}
	
	@Override
	public void drawCenteredString(FontRenderer fontRendererIn, String text, int x, int y, int color) {
		fontRendererIn.drawString(text, (float)(x - fontRendererIn.getStringWidth(text) / 2), (float)y, color, false);
	}
	
	@Override
	public void drawString(FontRenderer fontRendererIn, String text, int x, int y, int color) {
		fontRendererIn.drawString(text, (float)x, (float)y, color, false);
	}

	@Override
	public void drawLabelBackground(Minecraft mcIn, int p_146160_2_, int p_146160_3)
	{
		Point topLeftCorner = new Point(this.x - PADDING, this.y - PADDING);
		Point bottomRightCorner = new Point((int)topLeftCorner.getX() + this.width + PADDING * 2, (int)topLeftCorner.getY() + this.height + PADDING * 2);		
		drawRect((int)topLeftCorner.getX(), (int)topLeftCorner.getY(), (int)bottomRightCorner.getX(), (int)bottomRightCorner.getY(), this.backgroundColor);
		this.drawHorizontalLine((int)topLeftCorner.getX(), (int)bottomRightCorner.getX(), (int)topLeftCorner.getY(), BORDER_COLOR);
		this.drawHorizontalLine((int)topLeftCorner.getX(), (int)bottomRightCorner.getX(), (int)bottomRightCorner.getY(), BORDER_COLOR);
		this.drawVerticalLine((int)topLeftCorner.getX(), (int)topLeftCorner.getX(), (int)topLeftCorner.getY(), BORDER_COLOR);
		this.drawVerticalLine((int)bottomRightCorner.getX(), (int)bottomRightCorner.getX(), (int)topLeftCorner.getY(), BORDER_COLOR);
	}

}
