package edu.arizona.tomcat.World;

import java.util.ArrayList;
import java.util.Iterator;

import javax.xml.bind.JAXBElement;

import com.microsoft.Malmo.Schemas.DrawObjectType;
import com.microsoft.Malmo.Schemas.DrawingDecorator;
import com.microsoft.Malmo.Schemas.ObjectFactory;
import com.microsoft.Malmo.Utils.BlockDrawingHelper;

import edu.arizona.tomcat.Utils.Converter;
import net.minecraft.client.Minecraft;
import net.minecraft.util.text.Style;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.World;

public class DrawingHandler {
	
	private static DrawingHandler drawingHandlerInstance;
	
	/**
	 * Keeps only a single instance of this class active (singleton)
	 */
	private DrawingHandler() {
		
	}
	
	/**
	 * Retrieves the active instance of this class
	 * @return
	 */
	public static DrawingHandler getInstance() {
		if (drawingHandlerInstance == null) {
			drawingHandlerInstance = new DrawingHandler();
		}
		
		return drawingHandlerInstance;
	}
	
	/**
	 * Draw the objects of a drawing in the mission world.
	 * @param world - Mission world
	 * @param drawing - Object comprised by the elements to be drawn 
	 * @throws Exception
	 */
	public void draw(World world, Drawing drawing) throws Exception {
		ArrayList<JAXBElement<DrawObjectType>> jaxbeElements = this.getJAXBElements(drawing);
		DrawingDecorator drawingDecorator = new DrawingDecorator();
		BlockDrawingHelper drawContext = new BlockDrawingHelper();
		
		drawContext.beginDrawing(world);
		drawingDecorator.getDrawObjectType().addAll(jaxbeElements);
		drawContext.Draw(drawingDecorator, world);
		drawContext.endDrawing(world);
	}
	
	/**
	 * Convert each object in the drawing to a JAXElement.
	 * @param drawing - Object comprised by the objects to be drawn
	 * @return
	 */
	private ArrayList<JAXBElement<DrawObjectType>> getJAXBElements(Drawing drawing) {
		Iterator<CompositeDrawingObject> objects = drawing.getObjects();
		Iterator<DrawObjectType> malmoDrawObjects;
		ArrayList<JAXBElement<DrawObjectType>> jaxbeObjects = new ArrayList<JAXBElement<DrawObjectType>>();
		
		while(objects.hasNext()) {
			malmoDrawObjects = objects.next().getMalmoDrawObjects();
			while(malmoDrawObjects.hasNext()) { 
				jaxbeObjects.add(toJAXB(malmoDrawObjects.next()));
			}
		}
		
		return jaxbeObjects;
	}
	
	/**
	 * Convert a drawing object to a JAXB element
	 * @param drawObjectType - Drawing object
	 * @return
	 */
	private JAXBElement<DrawObjectType> toJAXB(DrawObjectType drawObjectType) {
		ObjectFactory jaxbeObjectFactory = new ObjectFactory();
		return jaxbeObjectFactory.createDrawObjectType(drawObjectType);	
	}
	
	/**
	 * Draws a countdown on the screen
	 */
	public void drawCountdown(long remainingSeconds, long remainingSecondsAlert) {
		if (remainingSeconds >= 0) {
			TextComponentString text = new TextComponentString("Mission ends in " + Converter.secondsToString(remainingSeconds, false) + "...");
			Style style = new Style();
			style.setBold(true);
			
			if (remainingSeconds <= remainingSecondsAlert) {
				style.setColor(TextFormatting.RED);
			}
	
			text.setStyle(style);		
			Minecraft.getMinecraft().ingameGUI.getChatGUI().clearChatMessages(true);
			Minecraft.getMinecraft().ingameGUI.getChatGUI().printChatMessage(text);
		}
	}

}
