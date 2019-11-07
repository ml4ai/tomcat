package edu.arizona.tomcat.Mission.gui;


import java.util.List;

import com.google.common.collect.Lists;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.*;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.resources.I18n;;
public class GuiBackground extends GuiLabel {

	protected int width = 200;
    protected int height = 20;
    public int x;
    public int y;
    private final List<String> labels;
    public int id;
    private boolean centered;
    public boolean visible = true;
    private final int textColor;
    private int backColor;
    private int ulColor;
    private int brColor;
    private final FontRenderer fontRenderer;
    private int border;


	public GuiBackground(FontRenderer fontRendererObj, int p_i45540_2_, int p_i45540_3_, int p_i45540_4_,
			int p_i45540_5_, int p_i45540_6_, int p_i45540_7_) {
		super(fontRendererObj, p_i45540_2_, p_i45540_3_, p_i45540_4_, p_i45540_5_, p_i45540_6_, p_i45540_7_);
        this.fontRenderer = fontRendererObj;
        this.id = p_i45540_2_;                        //fixed
        this.x = p_i45540_3_;                         //fixed
        this.y = p_i45540_4_;                         //fixed
        this.width = p_i45540_5_;                     //fixed
        this.height = p_i45540_6_;                    //fixed
        this.labels = Lists.<String>newArrayList();
        this.centered = false;
        this.textColor = p_i45540_7_;                 //fixed
        this.backColor = -1;                          //-1 represent white
        this.ulColor = -1;
        this.brColor = -1;
        this.border = 0;
	}
	public void addLine(String p_175202_1_)
    {
        this.labels.add(I18n.format(p_175202_1_, new Object[0]));
    }

    /**
     * Sets the Label to be centered
     */
    public GuiLabel setCentered()
    {
        this.centered = true;
        return this;
    }

	
	 public void drawLabel(Minecraft mc, int mouseX, int mouseY)
	    {
	        if (this.visible)
	        {
	            GlStateManager.enableBlend();
	            GlStateManager.tryBlendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);
	            this.drawLabelBackground(mc, mouseX, mouseY, this.labels.size());
	            int i = this.y + this.height / 2 + this.border / 2;
	            int j = i - this.labels.size() * 10 / 2;

	            for (int k = 0; k < this.labels.size(); ++k)
	            {
	                if (this.centered)
	                {
	           
	                    this.drawCenteredString(this.fontRenderer, (String)this.labels.get(k), this.x + this.width / 2, j + k * 10, this.textColor);
	                }
	                else
	                {
	                	
	                    this.drawString(this.fontRenderer, (String)this.labels.get(k), this.x, j + k * 10, this.textColor);
	                }
	            }
	        }
	    }
	  /**
	   * Renders the specified text to the screen, center-aligned. Args : renderer, string, x, y, color
	   */
	 @Override
	 public void drawCenteredString(FontRenderer fontRendererIn, String text, int x, int y, int color)
	 {
		 fontRendererIn.drawString(text, (float)(x - fontRendererIn.getStringWidth(text) / 2), (float)y, color, false);
	 }

	 /**
	  * Renders the specified text to the screen. Args : renderer, string, x, y, color
	  */
	 @Override
	 public void drawString(FontRenderer fontRendererIn, String text, int x, int y, int color)
	 {
		 fontRendererIn.drawString(text, (float)x, (float)y, color, false);
	 }

	 //@Override
	 public void drawLabelBackground(Minecraft mcIn, int p_146160_2_, int p_146160_3_, int oo)
	 {

		 int i = this.width + this.border * 2;
		 int j = this.height + this.border * 2;
		 int k = this.x - this.border;
		int l = this.y - this.border;
		drawRect(k, l - 5 * oo, k + i, l + j + 5 * oo, this.backColor);
		this.drawHorizontalLine(k, k + i, l - 5 * oo, this.ulColor);
		this.drawHorizontalLine(k, k + i, l + j + 5 * oo, this.brColor);
		this.drawVerticalLine(k, l - 5 * oo, l + j + 5 * oo, this.ulColor);
		this.drawVerticalLine(k + i, l - 5 * oo, l + j + 5 * oo, this.brColor);

	}

}
