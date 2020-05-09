Events available through the Minecraft Forge event bus (for MC v1.11.2)
=======================================================================

This is a list of events that are available to subscribe to on the Minecraft
Forge event bus. Along with subscribing, we can also handle events as they
occur - e.g. we can 'cancel' events if we want, or trigger other effects.

Not all events and methods are listed here, just the ones that seem like they
might be useful to ToMCAT and ASIST.

The complete JavaDocs (with all the events and their documentation) for this
version of Forge can be found here:
https://skmedix.github.io/ForgeJavaDocs/javadoc/forge/1.11.2-13.20.0.2228/

- player:
    - AchievementEvent: When the player receives an achievement.
    - AnvilRepairEvent: When the player removes a "repaired" item from the Anvil's Output slot.
    - ArrowLooseEvent: When a player stops using a bow.
    - ArrowNockEvent: When a player begins using a bow.
    - AttackEntityEvent: When a player attacks an Entity (includes players and monsters).
    - BonemealEvent: When a player attempts to use Bonemeal on a block.
    - EntityItemPickupEvent: When a player collides with a EntityItem on the ground.
    - FillBucketEvent: When a player attempts to use a Empty bucket.
    - PlayerDestroyItemEvent: When a player destroys an item.
    - PlayerDropsEvent: When a player dies.
    - PlayerEvent: Whenever an event involving Living entities occurs.
    - PlayerFlyableFallEvent: When a player falls, but is able to fly.
    - PlayerInteractEvent: When a player interacts in some way.
        - EntityInteractSpecific: When a player [right/left] clicks an entity (when the part of the entity that is clicked is known).
            - getLocalPos: Returns the local interaction position.
            - getTarget: Returns the target entity.
        - EntityInteract: This event is fired on both sides whenever a player right clicks an entity.
            - getTarget: Returns the target entity.
        - [Right/Left]ClickBlock: This event is fired on both sides whenever the player [right/left] clicks while targeting a block.
            - getHitVec: The hit vector of the click.
            - [get/set]UseBlock
            - [get/set]UseItem
        - [Right/Left]ClickItem: When a player [right/left]-clicks an item.
        - [Right/Left]ClickEmpty: When a player [right/left]-clicks an empty space with [an empty hand/any ItemStack].
            - getHand: The hand involved in this interaction.
            - getItemStack: The itemstack involved in this interaction.
            - getPos:
                - If the interaction was on an entity, will be a BlockPos centered on the entity.
                - If the interaction was on a block, will be the position of that block.
                - Otherwise, will be a BlockPos centered on the player.
                - Will never be null.
    - PlayerPickupXpEvent: This event is called when a player collides with a EntityXPOrb on the ground.
    - PlayerSetSpawnEvent: This event is called before a player's spawn point is changed.
    - PlayerSleepInBedEvent: When a player sleeps in a bed.
    - PlayerWakeUpEvent: When the player is waking up.
    - SleepingLocationCheckEvent: When game checks, if sleeping player should be still considered "in bed".
    - UseHoeEvent: When a player attempts to use a Hoe on a block.
- world: 
    - BlockEvent: 
        - HarvestDropsEvent: When a block is about to drop its harvested items.
        - BreakEvent: When an Block is about to be broken by a player
    - ExplosionEvent: triggers when an explosion happens in the world. 
    - WorldEvent: When an event involving the world occurs
        - WorldEvent.Load is fired when Minecraft loads a world.
- living
    - BabyEntitySpawnEvent: Fired just before a baby entity is about to be spawned.
    - EnderTeleportEvent: When an Enderman/Shulker teleports or an ender pearl is used.
    - LivingAttackEvent: When a living Entity is attacked.
    - LivingDeathEvent: When an Entity dies.
    - LivingDropsEvent: When an Entity's death causes dropped items to appear.
    - LivingEntityUseItemEvent
        - Start: When a player starts 'using' an item, typically when they hold right mouse.
        - Tick: Fired every tick that a player is 'using' an item.
        - Stop: Fired after an item has fully finished being used.
    - LivingEquipmentChangeEvent: When the Equipment of a Entity changes.
    - LivingEvent: Whenever an event involving Living entities occurs.
    - LivingExperienceDropEvent: When an entity drops experience on its death.
    - LivingFallEvent: When an Entity is set to be falling.
    - LivingHealEvent: When an Entity is set to be healed.
    - LivingHurtEvent: When an Entity is set to be hurt.
    - LivingPackSizeEvent: When the spawning system determines the maximum amount of the selected entity that can spawn at the same time.
    - LivingSetAttackTargetEvent: When an Entity sets a target to attack.
    - LivingSpawnEvent: Fired for any events associated with Living Enttnies spawn status.
    - PotionColorCalculationEvent: Fires after Potion Color Calculation.
    - ZombieEvent: Whenever a zombie is spawned for aid.
- ServerChatEvent: Whenever a C01PacketChatMessage is processed.
    - username: contains the username of the player sending the chat message.
    - message: contains the message being sent.
    - player: the instance of EntityPlayerMP for the player sending the chat message.
    - component: contains the instance of ChatComponentTranslation for the sent message.- brewing
    - PlayerBrewedPotionEvent: When a player picks up a potion from a brewing stand.
    - PotionBrewEvent
        - PotionBrewEvent.Pre is fired before vanilla brewing takes place.
        - PotionBrewEvent.Post is fired when a potion is brewed in the brewing stand. 
- enchanting
    - EnchantmentLevelSetEvent: When the enchantment level is set for each of the three potential enchantments in the enchanting table.
- item
    - ItemEvent: Base class for all EntityItem events
    - ItemExpireEvent: When an EntityItem's age has reached its maximum lifespan.
    - ItemTossEvent: Whenever a player tosses (Q) an item or drag-n-drops a stack of items outside the inventory GUI screens.
- minecart
    - MinecartCollisionEvent: When a minecart collides with an Entity.
    - MinecartEvent: Whenever an event involving minecart entities occurs.
    - MinecartInteractEvent: When a player interacts with a minecart.
    - MinecartUpdateEvent: When a minecart is updated.
- EntityEvent: When an event involving any Entity occurs.
- EntityJoinWorldEvent: When an Entity joins the world.
- EntityMountEvent: Whenever a entity mounts/dismounts another entity.
- EntityStruckByLightningEvent: When an Entity is about to be struck by lightening.
- EntityTravelToDimensionEvent: Fired before an Entity travels to a dimension.
- PlaySoundAtEntityEvent: Whenever a sound is set to be played at an Entity
- ThrowableImpactEvent: Fired before an EntityThrowable calls its EntityThrowable.onImpact method.
- AnvilUpdateEvent:  When a player places items in both the left and right slots of a anvil.
- AttachCapabilitiesEvent: Whenever an object with Capabilities support {currently TileEntity/Item/Entity)
- CommandEvent: Whenever a command is scheduled to be executed. 
