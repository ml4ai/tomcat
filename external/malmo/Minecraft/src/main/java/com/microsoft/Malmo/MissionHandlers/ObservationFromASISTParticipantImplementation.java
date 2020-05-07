// --------------------------------------------------------------------------------------------------
//  Copyright (c) 2016 Microsoft Corporation
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
// --------------------------------------------------------------------------------------------------

package com.microsoft.Malmo.MissionHandlers;

import com.google.gson.JsonObject;
import com.microsoft.Malmo.Schemas.MissionInit;
import com.microsoft.Malmo.Utils.JSONWorldDataHelper;
import edu.arizona.tomcat.Messaging.MqttService;
import edu.arizona.tomcat.Utils.TimeStamper;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;

/**
 * Simple IObservationProducer object that pings out a whole bunch of data.<br>
 */
public class ObservationFromASISTParticipantImplementation
    extends ObservationFromServer {
    public static class ASISTParticipantRequestMessage
        extends ObservationFromServer.ObservationRequestMessage {
        @Override
        void restoreState(ByteBuf buf) {
            // Nothing to do - no context needed.
        }

        @Override
        void persistState(ByteBuf buf) {
            // Nothing to do - no context needed.
        }
    }

    @Override
    public void writeObservationsToJSON(JsonObject json,
                                        MissionInit missionInit) {

        JsonObject data = new JsonObject();
        super.writeObservationsToJSON(data, missionInit);

        if (data != null && data.toString().length() > 2) {
            String timestamp = TimeStamper.getTimeStamp();
            JsonObject header = new JsonObject();
            header.addProperty("timestamp", timestamp);
            header.addProperty("message_type", "observation");
            header.addProperty("version", "0.2");

            JsonObject metadata = new JsonObject();
            metadata.addProperty("trial_id", missionInit.getExperimentUID());
            metadata.addProperty("timestamp", timestamp);
            metadata.addProperty("source", "human");
            metadata.addProperty("sub_type", "state");
            metadata.addProperty("version", "0.2");

            JsonObject message = new JsonObject();
            message.addProperty("name",
                                Minecraft.getMinecraft().player.getName());
            message.add("header", header);
            message.add("msg", metadata);
            message.add("data", data);

            MqttService.getInstance().publish(message.toString(),
                                              "observations/state");
        }
    }

    public static class ASISTParticipantRequestMessageHandler
        extends ObservationFromServer.ObservationRequestMessageHandler
        implements IMessageHandler<ASISTParticipantRequestMessage, IMessage> {
        @Override
        void buildJson(JsonObject json,
                       EntityPlayerMP player,
                       ObservationRequestMessage message) {

            JSONWorldDataHelper.buildPositionStats(json, player);
            JSONWorldDataHelper.buildMotionStats(json, player);
            json.addProperty("life", player.getHealth());
            json.addProperty("id", player.getCachedUniqueIdString());
            json.addProperty("name", player.getName());
            JSONWorldDataHelper.buildEnvironmentStats(json, player);
        }

        @Override
        public IMessage onMessage(ASISTParticipantRequestMessage message,
                                  MessageContext ctx) {
            return processMessage(message, ctx);
        }
    }

    @Override
    public ObservationRequestMessage createObservationRequestMessage() {
        return new ASISTParticipantRequestMessage();
    }
}
