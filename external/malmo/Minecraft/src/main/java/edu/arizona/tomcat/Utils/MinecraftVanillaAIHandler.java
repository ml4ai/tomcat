package edu.arizona.tomcat.Utils;

import net.minecraft.entity.ai.EntityAIBase;
import net.minecraft.entity.ai.EntityAIFollowGolem;
import net.minecraft.entity.ai.EntityAIMoveIndoors;
import net.minecraft.entity.ai.EntityAIMoveThroughVillage;
import net.minecraft.entity.ai.EntityAIMoveTowardsRestriction;
import net.minecraft.entity.ai.EntityAINearestAttackableTarget;
import net.minecraft.entity.ai.EntityAIOpenDoor;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.ai.EntityAIWanderAvoidWater;
import net.minecraft.entity.ai.EntityAIZombieAttack;
import net.minecraft.entity.monster.EntityIronGolem;
import net.minecraft.entity.monster.EntitySkeleton;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.passive.EntityVillager;
import net.minecraft.entity.player.EntityPlayer;

public class MinecraftVanillaAIHandler {
	
	/**
	 * Removes all kinds of AI that make zombies wander and attack
	 * @param zombie
	 */
	public static void freezeZombie(EntityZombie zombie) {
		for(Object task : zombie.tasks.taskEntries.toArray()) {
			EntityAIBase ai = ((EntityAITaskEntry) task).action;
			
			if (ai instanceof EntityAIZombieAttack || ai instanceof EntityAIMoveTowardsRestriction ||
					ai instanceof EntityAIWanderAvoidWater || ai instanceof EntityAIMoveThroughVillage) {
				zombie.tasks.removeTask(ai);
			}
		}
		for(Object task : zombie.targetTasks.taskEntries.toArray()) {
			EntityAIBase ai = ((EntityAITaskEntry) task).action;
			
			if (ai instanceof EntityAINearestAttackableTarget) {
				zombie.targetTasks.removeTask(ai);
			}
		}
	}
	
	/**
	 * Removes all kinds of AI that make skeletons wander and attack
	 * @param skeleton
	 */
	public static void freezeSkeleton(EntitySkeleton skeleton) {
		for(Object task : skeleton.tasks.taskEntries.toArray()) {
			EntityAIBase ai = ((EntityAITaskEntry) task).action;
			
			if (ai instanceof EntityAIWanderAvoidWater) {
				skeleton.tasks.removeTask(ai);
			}
		}
		for(Object task : skeleton.targetTasks.taskEntries.toArray()) {
			EntityAIBase ai = ((EntityAITaskEntry) task).action;
			
			if (ai instanceof EntityAINearestAttackableTarget) {
				skeleton.targetTasks.removeTask(ai);
			}
		}
	}
	
	/**
	 * Removes all kinds of AI that make villagers wander
	 * @param skeleton
	 */
	public static void freezeVillager(EntityVillager villager) {
		for(Object task : villager.tasks.taskEntries.toArray()) {
			EntityAIBase ai = ((EntityAITaskEntry) task).action;
			
			if (ai instanceof EntityAIMoveIndoors || ai instanceof EntityAIOpenDoor ||
				ai instanceof EntityAIMoveTowardsRestriction || ai instanceof EntityAIFollowGolem ||
				ai instanceof EntityAIWanderAvoidWater) {
				villager.tasks.removeTask(ai);
			}
		}
	}
	
	/**
	 * Restores zombies' abilities to wander and attack
	 * Code copied from the class EntityZombie
	 * @param zombie
	 */
	public static void unfreezeZombie(EntityZombie zombie) {
		zombie.tasks.addTask(2, new EntityAIZombieAttack(zombie, 1.0D, false));
		zombie.tasks.addTask(5, new EntityAIMoveTowardsRestriction(zombie, 1.0D));
		zombie.tasks.addTask(7, new EntityAIWanderAvoidWater(zombie, 1.0D));
		zombie.tasks.addTask(6, new EntityAIMoveThroughVillage(zombie, 1.0D, false));
		zombie.targetTasks.addTask(2, new EntityAINearestAttackableTarget<EntityPlayer>(zombie, EntityPlayer.class, true));
        zombie.targetTasks.addTask(3, new EntityAINearestAttackableTarget<EntityVillager>(zombie, EntityVillager.class, false));
        zombie.targetTasks.addTask(3, new EntityAINearestAttackableTarget<EntityIronGolem>(zombie, EntityIronGolem.class, true));
	}
	
	/**
	 * Restores skeletons' abilities to wander and attack
	 * Code copied from the class AbstractSkeleton
	 * @param skeleton
	 */
	public static void unfreezeSkeleton(EntitySkeleton skeleton) {
		skeleton.tasks.addTask(5, new EntityAIWanderAvoidWater(skeleton, 1.0D));
		skeleton.targetTasks.addTask(2, new EntityAINearestAttackableTarget<EntityPlayer>(skeleton, EntityPlayer.class, true));
		skeleton.targetTasks.addTask(3, new EntityAINearestAttackableTarget<EntityIronGolem>(skeleton, EntityIronGolem.class, true));
	}
	
	/**
	 * Restores villagers' abilities to wander
	 * Code copied from the class EntityVillager
	 * @param skeleton
	 */
	public static void unfreezeVillager(EntityVillager villager) {
		villager.tasks.addTask(2, new EntityAIMoveIndoors(villager));
		villager.tasks.addTask(4, new EntityAIOpenDoor(villager, true));
		villager.tasks.addTask(5, new EntityAIMoveTowardsRestriction(villager, 0.6D));
		villager.tasks.addTask(7, new EntityAIFollowGolem(villager));
		villager.tasks.addTask(9, new EntityAIWanderAvoidWater(villager, 0.6D));
	}

}
