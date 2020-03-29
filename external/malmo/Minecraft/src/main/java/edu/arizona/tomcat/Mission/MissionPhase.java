package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Messaging.TomcatClientServerHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Utils.Converter;
import java.util.ArrayList;
import net.minecraft.client.Minecraft;
import net.minecraft.world.World;

public class MissionPhase {

  public static enum CompletionStrategy { ANY_GOAL, ALL_GOALS }
  ;
  private static enum Status {
    WAITING_TO_START,
    DISPLAYING_INSTRUCTIONS,
    WAITING_FOR_INSTRUCTIONS_DISMISSAL,
    WAITING_FOR_MESSAGE_DISMISSAL,
    RUNNING,
    COMPLETED
  }
  ;

  private long secondsBeforeStart;
  private long timeOnStatusSet;
  private long worldTimeOnPhaseCompletion;
  private long worldTimeOnMessageDisplay;
  private boolean showCompletionMessage;
  private String messageOnCompletion;
  private int secondsUntilShowMessageScreen;
  private int secondsToDismissMessageScreen;
  private CompletionStrategy completionStrategy;
  private Status status;
  private RichContent instructions;
  private ArrayList<MissionGoal> openGoals;
  private ArrayList<MissionGoal> completedGoals;
  private ArrayList<PhaseListener> listeners;

  /**
   * Default constructor. Sets the completion strategy to ALL_GOALS and defines
   * start as immediate
   */
  public MissionPhase() { this.init(); }

  /**
   * Constructor
   * @param completionStrategy - Indicates if the phase ends after at least one
   * of the goals was achieved or all of them
   * @param secondsBeforeStart - Seconds before the phase starts
   * showCompletionMessage - Flag that indicates the presence of a message
   * screen on the completion of the phase
   * @param messageOnCompletion - Message to be shown on the completion of the
   * phase
   * @param secondsUntilShowMessageScreen - Seconds before the message screen
   * shows up after the phase completion
   * @param secondsToDismissMessageScreen - Seconds to dismiss the message
   * screen once it is open
   */
  public MissionPhase(RichContent instructions,
                      CompletionStrategy completionStrategy,
                      int secondsBeforeStart,
                      boolean showCompletionMessage,
                      String messageOnCompletion,
                      int secondsUntilShowMessageScreen,
                      int secondsToDismissMessageScreen) {
    this.init();
    this.instructions = instructions;
    this.completionStrategy = completionStrategy;
    this.secondsBeforeStart = secondsBeforeStart;
    this.showCompletionMessage = showCompletionMessage;
    this.messageOnCompletion = messageOnCompletion;
    this.secondsUntilShowMessageScreen = secondsUntilShowMessageScreen;
    this.secondsToDismissMessageScreen = secondsToDismissMessageScreen;
  }

  /**
   * Constructor
   * @param completionStrategy - Indicates if the phase ends after at least one
   * of the goals was achieved or all of them
   * @param secondsBeforeStart - Seconds before the phase starts
   * showCompletionMessage - Flag that indicates the presence of a message
   * screen on the completion of the phase
   * @param messageOnCompletion - Message to be shown on the completion of the
   * phase
   * @param secondsUntilShowMessageScreen - Seconds before the message screen
   * shows up after the phase completion
   * @param secondsToDismissMessageScreen - Seconds to dismiss the message
   * screen once it is open
   */
  public MissionPhase(RichContent instructions,
                      CompletionStrategy completionStrategy,
                      int secondsBeforeStart) {
    this.init();
    this.instructions = instructions;
    this.completionStrategy = completionStrategy;
    this.secondsBeforeStart = secondsBeforeStart;
    this.showCompletionMessage = false;
  }

  /**
   * Initialization of the private attributes
   */
  private void init() {
    this.showCompletionMessage = false;
    this.completionStrategy = CompletionStrategy.ALL_GOALS;
    this.secondsBeforeStart = 0;
    this.status = Status.WAITING_TO_START;
    this.openGoals = new ArrayList<MissionGoal>();
    this.completedGoals = new ArrayList<MissionGoal>();
    this.listeners = new ArrayList<PhaseListener>();
    this.messageOnCompletion = "";
  }

  /**
   * Update phase with the current state of the mission world
   * @param world - Mission world
   */
  public void update(World world) {
    switch (this.status) {
    case WAITING_TO_START:
      this.handleStatusWaitingToStart(world);
      break;

    case DISPLAYING_INSTRUCTIONS:
      this.handleStatusDisplayingInstructions(world);
      break;

    case RUNNING:
      this.handleStatusRunning(world);
      break;

    case COMPLETED:
      this.handleStatusComplete(world);
      break;

    case WAITING_FOR_MESSAGE_DISMISSAL:
      this.handleStatusWaitingForMessageDismissal(world);
      break;

    default:
      // Here fall all the status that depend on some player's action. The
      // server can only proceed after the player has provided an answer. e.g.
      // WAITING_FOR_INSTRUCTIONS_DISMISSAL
      break;
    }
  }

  /**
   * Does stuff prior to the phase start
   * @param world
   */
  protected void handleStatusWaitingToStart(World world) {
    long remainingTime =
        this.getRemainingTimeOnStatus(world, this.secondsBeforeStart);

    if (remainingTime <= 0) {
      this.timeOnStatusSet = 0;
      this.status = Status.DISPLAYING_INSTRUCTIONS;
    }
  }

  /**
   * Get remaining time in the current status
   * @return
   */
  private long getRemainingTimeOnStatus(World world, long timeLimit) {
    long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();

    if (this.timeOnStatusSet == 0) {
      this.timeOnStatusSet = currentWorldTime;
    }

    return Converter.getRemainingTimeInSeconds(
        world, this.timeOnStatusSet, timeLimit);
  }

  /**
   * Shows the instructions of the phase
   * @param world
   */
  protected void handleStatusDisplayingInstructions(World world) {
    if (this.instructions == null) {
      this.status = Status.RUNNING;
    }
    else {
      askClientsToShowInstructions();
      this.status = Status.WAITING_FOR_INSTRUCTIONS_DISMISSAL;
    }
  }

  /**
   * Asks all the clients to show instructions
   */
  private void askClientsToShowInstructions() {
    TomcatMessageData messageData = new TomcatMessageData(this.instructions);
    TomcatMessaging.TomcatMessage message = new TomcatMessage(
        TomcatMessageType.SHOW_INSTRUCTIONS_SCREEN, messageData);
    TomcatClientServerHandler.sendMessageToAllClients(message, true);
  }

  /**
   * Keeps checking if the goals were achieved
   * @param world
   */
  protected void handleStatusRunning(World world) {
    ArrayList<MissionGoal> toBeRemoved = new ArrayList<MissionGoal>();
    for (MissionGoal goal : this.openGoals) {
      goal.update(world);
      if (goal.hasBeenAchieved()) {
        toBeRemoved.add(goal);
        this.completedGoals.add(goal);
        this.notifyAllAboutGoalAchievement(world, goal);
      }
    }
    this.openGoals.removeAll(toBeRemoved);
    if ((this.completionStrategy == CompletionStrategy.ANY_GOAL &&
         !toBeRemoved.isEmpty()) ||
        this.openGoals.isEmpty()) {
      this.setCompletion();
    }
  }

  /**
   * Marks the phase as completed
   */
  private void setCompletion() {
    this.timeOnStatusSet = 0;
    this.status = Status.COMPLETED;
  }

  /**
   * Creates a message screen after some seconds and afterwards keep updating it
   * so it can dismiss after the time limit defined
   */
  public void handleStatusComplete(World world) {
    if (this.showCompletionMessage) {
      this.showMessageScreen(world);
    }
    else {
      this.notifyAllAboutPhaseCompletion();
    }
  }

  /**
   * Creates a message screen after some seconds and presents it to the user.
   * Here we define a waiting time before showing the screen because there can
   * only be one active screen at a time. So if the goal is to open the
   * inventory, for example, showing this screen immediately would dismiss the
   * inventory before the user could even see it.
   */
  private void showMessageScreen(World world) {
    int remainingTimeToShowMessageScreen =
        this.getRemainingSecondsToShowMessageScreen(world);

    if (remainingTimeToShowMessageScreen <= 0) {
      this.askClientsToShowMessageScreen();
      this.status = Status.WAITING_FOR_MESSAGE_DISMISSAL;
    }
  }

  /**
   * Asks the clients to show message screen
   */
  private void askClientsToShowMessageScreen() {
    TomcatMessageData messageData = new TomcatMessageData();
    messageData.setMissionPhaseMessage(this.messageOnCompletion);
    TomcatMessaging.TomcatMessage message =
        new TomcatMessage(TomcatMessageType.SHOW_MESSAGE_SCREEN, messageData);
    TomcatClientServerHandler.sendMessageToAllClients(message, false);
  }

  private int getRemainingSecondsToShowMessageScreen(World world) {
    long currentWorldTime = world.getTotalWorldTime();

    if (this.worldTimeOnPhaseCompletion == 0) {
      this.worldTimeOnPhaseCompletion = currentWorldTime;
    }

    return Converter.getRemainingTimeInSeconds(
        world,
        this.worldTimeOnPhaseCompletion,
        this.secondsUntilShowMessageScreen);
  }

  /**
   * Send a message to the client side close the current message screen after
   * some duration so it can dismiss after the time limit defined
   */
  public void handleStatusWaitingForMessageDismissal(World world) {
    int remainingSeconds = this.getRemainingTimeToDismissMessageScreen(world);

    if (remainingSeconds <= 0) {
      TomcatMessaging.TomcatMessage message =
          new TomcatMessage(TomcatMessageType.DISMISS_OPEN_SCREEN);
      TomcatClientServerHandler.sendMessageToAllClients(message, false);
      this.handleDismissalOfOpenScreen();
    }
  }

  /**
   * Retrieves the remaining number of seconds to dismiss the message screen
   * @param world - Minecraft world
   * @return
   */
  private int getRemainingTimeToDismissMessageScreen(World world) {
    long currentWorldTime = world.getTotalWorldTime();

    if (this.worldTimeOnMessageDisplay == 0) {
      this.worldTimeOnMessageDisplay = currentWorldTime;
    }

    return Converter.getRemainingTimeInSeconds(
        world,
        this.worldTimeOnMessageDisplay,
        this.secondsToDismissMessageScreen);
  }

  /**
   * Reopens phase goals
   */
  public void reset() {
    for (MissionGoal goal : this.completedGoals) {
      goal.reset();
    }
    this.openGoals = this.completedGoals;
    this.completedGoals.clear();
  }

  /**
   * Notifies listeners about the phase completion
   */
  private void notifyAllAboutPhaseCompletion() {
    for (PhaseListener listener : this.listeners) {
      listener.phaseCompleted();
    }
  }

  /**
   * Notifies listeners about a goal achievement
   */
  private void notifyAllAboutGoalAchievement(World world, MissionGoal goal) {
    for (PhaseListener listener : this.listeners) {
      listener.goalAchieved(world, goal);
    }
  }

  /**
   * Adds a new goal to the phase
   * @param goal - Goal to be achieved
   */
  public void addGoal(MissionGoal goal) { this.openGoals.add(goal); }

  /**
   * Adds listener to be notified upon phase completion
   * @param listener - Listener
   */
  public void addListener(PhaseListener listener) {
    this.listeners.add(listener);
  }

  /**
   * Defines actions to be taken after the currently open screen was dismissed
   * by the client
   */
  public void handleDismissalOfOpenScreen() {
    switch (this.status) {
    case WAITING_FOR_INSTRUCTIONS_DISMISSAL:
      this.timeOnStatusSet = 0;
      this.status = Status.RUNNING;
      break;
    case WAITING_FOR_MESSAGE_DISMISSAL:
      this.notifyAllAboutPhaseCompletion();
      break;

    default:
      break;
    }
  }

  /**
   * Allows the player to reread the instructions for in the middle of an active
   * phase;
   */
  public void showInstructions() {
    if (this.status == Status.RUNNING) {
      this.status = Status.DISPLAYING_INSTRUCTIONS;
    }
  }
}
