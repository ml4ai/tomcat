package edu.arizona.tomcat.Mission.gui;

import edu.arizona.tomcat.Emotion.EmotionHandler;
import java.util.ArrayList;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiLabel;
import net.minecraft.client.gui.GuiScreen;

public class SimpleGUI extends GuiScreen {

  private static final int SCARED_BUTTON_ID = 0;
  private static final int CALM_BUTTON_ID = 1;

  private ArrayList<FeedbackListener> listeners;

  /**
   * Constructor
   */
  public SimpleGUI() { this.listeners = new ArrayList<FeedbackListener>(); }

  @Override
  public void initGui() {
    super.initGui();

    GuiLabel label = new GuiLabel(fontRendererObj,
                                  1,
                                  (this.width - 200) / 2,
                                  this.height / 4,
                                  200,
                                  20,
                                  0xFFFFFF);
    label.setCentered();
    label.addLine("How are you feeling?");
    labelList.add(label);

    buttonList.add(new GuiButton(
        SCARED_BUTTON_ID, (width - 220) / 2, height / 2, 100, 20, "Scared"));
    buttonList.add(new GuiButton(
        CALM_BUTTON_ID, (width - 220) / 2 + 120, height / 2, 100, 20, "Calm"));
  }

  @Override
  protected void actionPerformed(GuiButton guiButton) {
    if (guiButton.id == SCARED_BUTTON_ID) {
      this.notifyListeners(EmotionHandler.Emotion.FEAR);
    }
    else {
      this.notifyListeners(EmotionHandler.Emotion.CALMNESS);
    }
  }

  @Override
  public boolean doesGuiPauseGame() {
    return true;
  }

  /**
   * Adds listener to be notified once one of the buttons is pressed
   * @param listener - Object that wants to be notified when one of the buttons
   * is pressed
   */
  public void addListener(FeedbackListener listener) {
    this.listeners.add(listener);
  }

  /**
   * Close the GUI and notify listeners which of the buttons was pressed
   * @param emotion - Emotion informed by the player
   */
  public void notifyListeners(EmotionHandler.Emotion emotion) {
    this.mc.player.closeScreen();
    for (FeedbackListener listener : this.listeners) {
      listener.emotionProvided(emotion);
    }
  }
}
