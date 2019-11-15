package edu.arizona.tomcat.Mission.Goal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.world.World;


public class KillEntityGoal extends MissionGoal {
	private static boolean isCreate = false;
	private List<UUID> idList; // record IDs of entities we create in this class
	private List<UUID> willBeKilled; // record IDs of entities we need to kill

	public KillEntityGoal() {
		this.idList = new ArrayList<UUID>(); // initialize
		this.willBeKilled = new ArrayList<UUID>();
	}

	@Override
	public void update(World world) 
	{
		if (isCreate == false) {
			int distance = -15; 
			int playersX = (int) Minecraft.getMinecraft().player.posX;
			int playersZ = (int) Minecraft.getMinecraft().player.posZ;
			int playersY = (int) Minecraft.getMinecraft().player.posY;	
			EntityZombie n = new EntityZombie(world);
			n.setPosition(playersX, playersY, playersZ  + distance);
			UUID id = UUID.randomUUID(); 
			idList.add(id); 
			willBeKilled.add(id);
			n.setUniqueId(id);		
			world.spawnEntity(n);
			isCreate = true; 
		}
		List<UUID> loadedEntityIdList = new ArrayList<UUID>(); // need to refresh every tick to get updated entities on the world
		for (Entity n : world.getLoadedEntityList()) {	
			loadedEntityIdList.add(n.getUniqueID());
		}
		if (!this.goalAchieved) {
			Iterator<UUID> itr= willBeKilled.iterator(); 
			while (itr.hasNext()) {
				UUID id = itr.next();
				if (!loadedEntityIdList.contains(id)) {
					itr.remove(); // remove the entity we killed
					idList.remove(id);
					this.goalAchieved = true;
				}
			}
		}
	}
}
