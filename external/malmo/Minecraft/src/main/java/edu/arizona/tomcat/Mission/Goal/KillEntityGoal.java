package edu.arizona.tomcat.Mission.Goal;


import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.world.World;


public class KillEntityGoal extends MissionGoal {
	private static boolean isCreate = false;
	@Override
	public void update(World world) 
	{
		if (isCreate == false) {
			int distance = 5; // Number of voxels apart from the player
			int playersX = (int) Minecraft.getMinecraft().player.posX;
			int playersZ = (int) Minecraft.getMinecraft().player.posZ;
			int playersY = (int) Minecraft.getMinecraft().player.posY;	
			EntityZombie n = new EntityZombie(world);
			n.setPosition(playersX + distance, playersY, playersZ);
			//world.onEntityAdded(n); // draw an entity cannot move
			world.spawnEntity(n); // spawn an Entity
			// create only one
			isCreate = true; 
		}
		
		if (!this.goalAchieved) {
			int sum = 0;
			for (Entity n: world.getLoadedEntityList()) {
				if (n.isDead) {
					sum += 1;
					System.out.println(n.getEntityId() + " is Dead!");
				}
			}
			if (sum == 1) {
				this.goalAchieved = true;
			}
		}
		
		
	}
}
