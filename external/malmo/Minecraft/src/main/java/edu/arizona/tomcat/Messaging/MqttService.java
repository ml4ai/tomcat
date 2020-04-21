package edu.arizona.tomcat.Messaging;

import com.google.gson.Gson;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import java.io.IOException;
import edu.arizona.tomcat.events.Event;

/** A class to provide a convenient interface to the Eclipse Paho MQTT client
 * library. */
public class MqttService {

  private MqttClient client;
  private Gson gson = new Gson();

  public MqttService() {
    try {
      MemoryPersistence persistence = new MemoryPersistence();
      MqttConnectOptions connectOptions = new MqttConnectOptions();
      this.client =
          new MqttClient("tcp://127.0.0.1:1883", "TomcatClient", persistence);
      connectOptions.setCleanSession(true);
      this.client.connect(connectOptions);
    }
    catch (MqttException me) {
      System.err.println("MqttException:");
      System.err.println("reason " + me.getReasonCode());
      System.err.println("msg " + me.getMessage());
      System.err.println("loc " + me.getLocalizedMessage());
      System.err.println("cause " + me.getCause());
      System.err.println("excep " + me);
    }
  }

  public void publish(Event event, String topic) {
      MqttMessage message = new MqttMessage(gson.toJson(event).getBytes());
  }
}
