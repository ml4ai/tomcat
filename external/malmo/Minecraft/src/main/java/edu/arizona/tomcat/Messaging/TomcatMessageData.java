package edu.arizona.tomcat.Messaging;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import edu.arizona.tomcat.Utils.Converter;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.ByteBufInputStream;
import io.netty.buffer.ByteBufOutputStream;

public class TomcatMessageData {
	
	private static enum Key { MISSION_PHASE_INSTRUCTIONS };

	private Map<String, String> data;
	
	public TomcatMessageData() {
		this.data = new HashMap<String, String>();
	}
	
	/**
	 * Reads data from a ByteBuffer object
	 * @param buffer - ByteBuffer object
	 * @throws IOException
	 */
	public void readDataFromBuffer(ByteBuf buffer) throws IOException {
		this.data = Converter.jsonToMap(this.readStringFromByteBuffer(buffer));
		if (this.data == null) {
			this.data = new HashMap<String, String>();
		}
	}
	
	/**
	 * Reads data to a ByteBuffer object
	 * @param buffer - ByteBuffer object
	 * @throws IOException
	 */
	public void writeDataToBuffer(ByteBuf buffer) throws IOException {
		String json = Converter.mapToJson(this.data);
		this.writeStringToByteBuffer(json, buffer);		
	}
	
	/**
	 * Writes a string to a ByteBuffer object
	 * @param string - String
	 * @param buffer - ByteBuffer object
	 * @throws IOException
	 */
	private void writeStringToByteBuffer(String string, ByteBuf buffer) throws IOException {			
		ByteBufOutputStream byteBufOutputStream = new ByteBufOutputStream(buffer);
		byteBufOutputStream.writeUTF(string);
		byteBufOutputStream.close();			
	}
	
	/**
	 * Reads a string from a ByteBuffer object
	 * @param buffer - ByteBuffer object
	 * @return
	 * @throws IOException
	 */
	private String readStringFromByteBuffer(ByteBuf buffer) throws IOException {			
		ByteBufInputStream byteBufInputStream = new ByteBufInputStream(buffer);
		String string =  byteBufInputStream.readUTF();
		byteBufInputStream.close();
		return string;
	}
	
	/**
	 * Adds a series of mission phase instructions to the data map
	 * @param instructions - Mission phase instructions
	 */
	public void setMissionPhaseInstructions(String instructions) {
		this.data.put(Key.MISSION_PHASE_INSTRUCTIONS.toString(), instructions);
	}
	
	/**
	 * Retrieves a series of mission phase instructions from the data map
	 * @return
	 */
	public String getMissionPhaseInstructions() {
		return this.data.get(Key.MISSION_PHASE_INSTRUCTIONS.toString());
	}
}
