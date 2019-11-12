package edu.arizona.tomcat.Messaging;

import java.io.IOException;

import com.microsoft.Malmo.MalmoMod;

import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.util.IThreadListener;
import net.minecraft.world.WorldServer;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;

public class TomcatMessaging {

	public enum TomcatMessageType { SHOW_COMPLETION_SCREEN, SHOW_INSTRUCTIONS_SCREEN, VILLAGER_SAVED, INSTRUCTIONS_SCREEN_DISMISSED };


	public static class TomcatMessage implements IMessage {

		private TomcatMessageType messageType;
		private TomcatMessageData data;


		public TomcatMessage() {

		}
		
		/**
		 * Constructor
		 * @param messageType - Message type
		 */
		public TomcatMessage(TomcatMessageType messageType) {
			this.messageType = messageType;
			this.data = new TomcatMessageData();
		}
		
		/**
		 * Constructor
		 * @param messageType - Message type
		 * @param data - Message data
		 */
		public TomcatMessage(TomcatMessageType messageType, TomcatMessageData data) {
			this.messageType = messageType;
			this.data = data;
		}

		@Override
		public void fromBytes(ByteBuf buf) {			
			try {
				this.messageType = TomcatMessageType.values()[buf.readInt()];
				this.data = new TomcatMessageData();
				this.data.readDataFromBuffer(buf);				
			} catch (IOException e) {
				System.out.println("Warning - failed to read message data");
				e.printStackTrace();
			}
		}

		@Override
		public void toBytes(ByteBuf buf) {			
			try {
				buf.writeInt(this.messageType.ordinal());				
				this.data.writeDataToBuffer(buf);				
			} catch (IOException e) {
				System.out.println("Warning - failed to write message data");
				e.printStackTrace();
			}
		}

		/**
		 * Retrieves the message type
		 * @return
		 */
		public TomcatMessageType getMessageType() {
			return this.messageType;
		}	
		
		/**
		 * Retrieves the message data
		 * @return
		 */
		public TomcatMessageData getMessageData() {
			return this.data;
		}	

	}

	public static class TomcatMessageHandler implements IMessageHandler<TomcatMessage, IMessage> {

		@Override
		public IMessage onMessage(final TomcatMessage message, final MessageContext ctx) {
			IThreadListener mainThread = null;			
			if (ctx.side == Side.CLIENT) {
				mainThread = Minecraft.getMinecraft();
				mainThread.addScheduledTask(new Runnable() {
					@Override
					public void run() {
						MalmoMod.instance.getClient().getTomcatClientMission().handleMessageFromServer(message);
					}
				});    
			} else if (ctx.side == Side.SERVER) {
				mainThread = (WorldServer) ctx.getServerHandler().playerEntity.getServerWorld();
				mainThread.addScheduledTask(new Runnable() {
					@Override
					public void run() {
						MalmoMod.instance.getServer().getTomcatServerMission().handleMessageFromClient(message);
					}
				});  
			}
			return null;
		}
	}
}
