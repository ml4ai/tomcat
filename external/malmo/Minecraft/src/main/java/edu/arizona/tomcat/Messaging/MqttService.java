package edu.arizona.tomcat.Messaging;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/** A singleton class to provide a convenient interface to the Eclipse Paho
 * MQTT client library. */
public class MqttService {

	private MqttClient client;
	private Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();
	private static MqttService instance = null;

	private void printException(MqttException me) {
		System.err.println("MqttException:");
		System.err.println("reason " + me.getReasonCode());
		System.err.println("msg " + me.getMessage());
		System.err.println("loc " + me.getLocalizedMessage());
		System.err.println("cause " + me.getCause());
		System.err.println("excep " + me);
	}
	private MqttService() {
		try {
			MemoryPersistence persistence = new MemoryPersistence();
			MqttConnectOptions connectOptions = new MqttConnectOptions();
			// Note: We are currently hard-coding the port number to the default
			// (1883), but we may want to change this to be settable by the end-user
			// later.
			this.client =
					new MqttClient("tcp://127.0.0.1:1883", "TomcatClient", persistence);
			connectOptions.setCleanSession(true);
			this.client.connect(connectOptions);
		}
		catch (MqttException me) {
			this.printException(me);
		}
	}

	public static MqttService getInstance() {
		if (instance == null) {
			instance = new MqttService();
		}
		return instance;
	}

	public void publish(Object object, String topic) {
		this.publish(gson.toJson(object), topic);
	}

	public void publish(String msg, String topic) {
		try {
			MqttMessage message = new MqttMessage(msg.getBytes());
			message.setQos(2);
			this.client.publish(topic, message);
		}
		catch (MqttException me) {
			this.printException(me);
		}
	}

}
