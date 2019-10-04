package edu.arizona.cs.Tomcat.Mission;

import com.microsoft.Malmo.Schemas.EntityTypes;

import edu.arizona.cs.Tomcat.Emotion.EmotionHandler;
import edu.arizona.cs.Tomcat.Mission.gui.SimpleGUI;
import edu.arizona.cs.Tomcat.World.Drawing;
import edu.arizona.cs.Tomcat.World.Entity;
import net.minecraft.client.Minecraft;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class SARMission extends Mission {
	
	private boolean zombieHordeCreated;
	private boolean feedbackRequested;

	@Override
	public void init(World world) {
		// TODO Create things in the beginning of the mission
		this.zombieHordeCreated = false;
		this.timeLimitInSeconds = 100;
	}

	@Override
	public void update(World world) {
		super.update(world);

		try {
			if (this.getRemainingSeconds() < 90 && !this.zombieHordeCreated) {
				this.createZombieHorde(world);
			}
			
			// Ask for feedback for the first time
			if (this.getRemainingSeconds() > 50 && this.getRemainingSeconds() < 60 && !this.feedbackRequested) {
				this.requestFeedback();
			}
			
			// Allow feedback request for the second time
			if (this.getRemainingSeconds() > 45 && this.getRemainingSeconds() < 50) {
				this.feedbackRequested = false;
			}
			
			// Ask for feedback for the second time
			if (this.getRemainingSeconds() < 45 && !this.feedbackRequested) {
				this.requestFeedback();
			}
			
			if (this.currentEmotion == EmotionHandler.Emotion.CALMNESS) {
				this.createZombieMegaHorde(world);
				this.currentEmotion = null;
				
				world.setBlockToAir(new BlockPos(2, 2, 30));
				world.setBlockToAir(new BlockPos(2, 2, 31));
				world.setBlockToAir(new BlockPos(2, 2, 32));
				world.setBlockToAir(new BlockPos(2, 2, 33));
			}			
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Creates a horde of 3 zombies in front of the player 
	 * @param world - Mission world
	 * @throws Exception 
	 */
	private void createZombieHorde(World world) throws Exception {
		int distance = 10; // Number of voxels apart from the player
		double playersX = Minecraft.getMinecraft().player.posX;
		double playersZ = Minecraft.getMinecraft().player.posZ;
		double playersY	= Minecraft.getMinecraft().player.posY;

		Drawing drawing = new Drawing();

		// Create zombie in front of the player
		Entity zombie1 = new Entity(playersX, playersZ + distance, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the northwest of the player
		Entity zombie2 = new Entity(playersX + distance, playersZ + distance/2, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the northeast of the player
		Entity zombie3 = new Entity(playersX - distance, playersZ + distance/2, playersY, EntityTypes.ZOMBIE);
						
		drawing.addObject(zombie1);
		drawing.addObject(zombie2);
		drawing.addObject(zombie3);
		this.drawingHandler.draw(world, drawing);
		
		this.zombieHordeCreated = true;
	}
	
	/**
	 * Creates a horde of zombies all around the player 
	 * @param world - Mission world
	 * @throws Exception 
	 */
	private void createZombieMegaHorde(World world) throws Exception {
		int distance = 20; // Number of voxels apart from the player
		double playersX = Minecraft.getMinecraft().player.posX;
		double playersZ = Minecraft.getMinecraft().player.posZ;
		double playersY	= Minecraft.getMinecraft().player.posY;

		Drawing drawing = new Drawing();

		// Create zombie in front of the player
		Entity zombie1 = new Entity(playersX, playersZ + distance, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the northwest of the player
		Entity zombie2 = new Entity(playersX + distance, playersZ + distance/2, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the left of the player
		Entity zombie3 = new Entity(playersX + distance, playersZ, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the southwest of the player
		Entity zombie4 = new Entity(playersX + distance, playersZ - distance/2, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie behind the player
		Entity zombie5 = new Entity(playersX, playersZ - distance, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the southeast of the player
		Entity zombie6 = new Entity(playersX - distance, playersZ - distance/2, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the right of the player
		Entity zombie7 = new Entity(playersX - distance, playersZ, playersY, EntityTypes.ZOMBIE);
		
		// Create zombie to the northeast of the player
		Entity zombie8 = new Entity(playersX - distance, playersZ + distance/2, playersY, EntityTypes.ZOMBIE);
		
		drawing.addObject(zombie1);
		drawing.addObject(zombie2);
		drawing.addObject(zombie3);
		drawing.addObject(zombie4);
		drawing.addObject(zombie5);
		drawing.addObject(zombie6);
		drawing.addObject(zombie7);
		drawing.addObject(zombie8);
		this.drawingHandler.draw(world, drawing);
		
		this.zombieHordeCreated = true;
	}
	
	/**
	 * Request for the player's feedback about his emotion
	 */
	private void requestFeedback() {
		SimpleGUI simpleGUI = new SimpleGUI();
		simpleGUI.addListener(this);
		Minecraft.getMinecraft().displayGuiScreen(simpleGUI);
		this.feedbackRequested = true;
	}


}
