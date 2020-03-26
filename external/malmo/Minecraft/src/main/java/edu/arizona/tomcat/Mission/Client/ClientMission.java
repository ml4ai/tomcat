package edu.arizona.tomcat.Mission.Client;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.gui.MessageScreen;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.RichContentScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Mission.gui.SelfReportScreen;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;

public abstract class ClientMission implements ScreenListener {

  private enum SelfReportPhase {
    SHOWING_SCREEN_BEFORE_QUESTIONS,
    PRESENTING_QUESTIONS,
    SHOWING_SCREEN_AFTER_QUESTIONS,
    NOT_PROCESSING_SELF_REPORT
  }
  ;

  private SelfReportContent currentSelfReport;
  private SelfReportPhase selfReportPhase;

  public ClientMission() {
    this.selfReportPhase = SelfReportPhase.NOT_PROCESSING_SELF_REPORT;
  }

  /**
   * Handle message from the server side
   */
  public void handleMessageFromServer(TomcatMessage message) {
    switch (message.getMessageType()) {
    case SHOW_INSTRUCTIONS_SCREEN:
      RichContentScreen instructionsScreen = new RichContentScreen(
          message.getMessageData().getRichContent(), true, true, "Ok");
      instructionsScreen.addListener(this);
      Minecraft.getMinecraft().displayGuiScreen(instructionsScreen);
      break;

    case SHOW_MESSAGE_SCREEN:
      MessageScreen messageScreen =
          new MessageScreen(message.getMessageData().getMissionPhaseMessage());
      Minecraft.getMinecraft().displayGuiScreen(messageScreen);
      break;

    case SHOW_COMPLETION_SCREEN:
      Minecraft.getMinecraft().displayGuiScreen(new RichContentScreen(
          message.getMessageData().getRichContent(), false, false));
      break;

    case DISMISS_OPEN_SCREEN:
      Minecraft.getMinecraft().player.closeScreen();
      MalmoMod.network.sendToServer(
          new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));
      break;

    case SHOW_SELF_REPORT:
      this.currentSelfReport = message.getMessageData().getSelfReport();
      this.handleNextSelfReportScreen();
      break;

    default:
      break;
    }
  };

  private void handleNextSelfReportScreen() {
    switch (this.selfReportPhase) {
    case NOT_PROCESSING_SELF_REPORT:
      this.selfReportPhase = SelfReportPhase.SHOWING_SCREEN_BEFORE_QUESTIONS;
      if (this.currentSelfReport.getRichContentBeforeQuestions() != null) {
        RichContentScreen selfReportInitialRichContentScreen =
            new RichContentScreen(
                this.currentSelfReport.getRichContentBeforeQuestions(),
                true,
                true,
                "Ok");
        selfReportInitialRichContentScreen.addListener(this);
        Minecraft.getMinecraft().displayGuiScreen(
            selfReportInitialRichContentScreen);
        break;
      }
      else {
        this.handleNextSelfReportScreen();
      }
    case SHOWING_SCREEN_BEFORE_QUESTIONS:
      this.selfReportPhase = SelfReportPhase.PRESENTING_QUESTIONS;
      SelfReportScreen selfReportScreen =
          new SelfReportScreen(this.currentSelfReport, true);
      selfReportScreen.addListener(this);
      Minecraft.getMinecraft().displayGuiScreen(selfReportScreen);
      break;
    case PRESENTING_QUESTIONS:
      this.selfReportPhase = SelfReportPhase.SHOWING_SCREEN_AFTER_QUESTIONS;
      if (this.currentSelfReport.getRichContentAfterQuestions() != null) {
        RichContentScreen selfReportFinalRichContentScreen =
            new RichContentScreen(
                this.currentSelfReport.getRichContentAfterQuestions(),
                true,
                true,
                "Ok");
        selfReportFinalRichContentScreen.addListener(this);
        Minecraft.getMinecraft().displayGuiScreen(
            selfReportFinalRichContentScreen);
      }
      else {
        this.handleNextSelfReportScreen();
      }
      break;
    case SHOWING_SCREEN_AFTER_QUESTIONS:
      this.selfReportPhase = SelfReportPhase.NOT_PROCESSING_SELF_REPORT;
      TomcatMessageData data = new TomcatMessageData(this.currentSelfReport);
      data.setPlayerName(Minecraft.getMinecraft().player.getName());
      MalmoMod.network.sendToServer(
          new TomcatMessage(TomcatMessageType.SELF_REPORT_ANSWERED, data));
      break;

    default:
    }
  }

  @Override
  public void screenDismissed(GuiScreen screen) {
    if (this.selfReportPhase == SelfReportPhase.NOT_PROCESSING_SELF_REPORT) {
      MalmoMod.network.sendToServer(
          new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));
    }
    else {
      this.handleNextSelfReportScreen();
    }
  }

  @Override
  public void screenDismissed(GuiScreen screen, SelfReportContent selfReport) {
    this.currentSelfReport = selfReport;
    this.handleNextSelfReportScreen();
  }

  /**
   * Show screen informing the user connection was lost with malmo client
   */
  public void showScreenConnectionLost() {
    RichContent content = RichContent.createFromJson("connection_lost.json");
    Minecraft.getMinecraft().displayGuiScreen(
        new RichContentScreen(content, false, false));
  }
}
