package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;

public abstract class ClientMission {
	
	/**
	 * Handle message from the server side
	 */
	public abstract void handleMessageFromServer(TomcatMessage message);
	
	

}
