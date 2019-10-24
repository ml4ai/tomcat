## Message Exchange between Client and Server in Minecraft Forge

If we need to trigger an action in the client side from the server or
vice-versa, we need to create two message classes with the following structure:

```java
public static class InventoryMessage implements IMessage {

    public ItemStack itemStack; // Any object(s) that we want to serialize
		
	public InventoryMessage() { }

	public InventoryMessage(ItemStack itemStack) {
		this.itemStack = itemStack;
	}

	@Override
	public void fromBytes(ByteBuf buf) {
		this.itemStack = ByteBufUtils.readItemStack(buf);			
	}

	@Override
	public void toBytes(ByteBuf buf) {
		ByteBufUtils.writeItemStack(buf, this.itemStack);			
	}				

}

public static class InventoryMessageHandler implements IMessageHandler<InventoryMessage, IMessage> {
		
  @Override
  public IMessage onMessage(final InventoryMessage message, final MessageContext ctx) {
    IThreadListener mainThread = null;			
    if (ctx.side == Side.CLIENT) {
      mainThread = Minecraft.getMinecraft();
    } else {
      mainThread = (WorldServer)ctx.getServerHandler().playerEntity.world;
    }
    mainThread.addScheduledTask(new Runnable() {
      @Override
      public void run() {
        // Do stuff here
      }
    });    
    return null;
  }
}
```

In the message class we store all the information we need to pass from one side
to the other and in the message handler we put the actions that need to be
performed in the target side.
Those classes must be registered so they can be triggered when needed, this can
be done in the class MalmoMod after other similar registrations with the
following piece of code:

```java
// The target side can be SERVER or CLIENT. If we need both, we just register this function twice, onde for each side.
MalmoMod.network.registerMessage(InventoryMessageHandler.class, InventoryMessage.class, 0, Side.SERVER);
```

Finally, to send a message from the client to the server we use method
MalmoMod.network.sendToServer passing the message object as parameter. On the
other hand, to send a message from the client to the server, we use the method
MalmoMod.network.sendTo passing the message object and the player (there can be
more than one player/client in a mutiplayer game).  
