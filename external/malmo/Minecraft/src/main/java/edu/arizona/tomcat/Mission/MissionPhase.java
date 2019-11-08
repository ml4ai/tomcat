package edu.arizona.tomcat.Mission;

import java.util.ArrayList;

import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.InstructionsScreen;
import edu.arizona.tomcat.Mission.gui.MessageScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import edu.arizona.tomcat.Utils.Converter;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.world.World;

public class MissionPhase implements ScreenListener {
	
	public static enum CompletionStrategy { ANY_GOAL, ALL_GOALS };
	private static enum Status {WAITING_TO_START, DISPLAYING_INSTRUCTIONS, RUNNING, COMPLETED};
	private static int SECONDS_TO_DISMISS_MESSAGE_SCREEN = 2;
	private static int SECONDS_UNTIL_SHOW_MESSAGE_SCREEN = 2;

	private long secondsBeforeStart;
	private long timeOnStatusSet;
	private long worldTimeOnPhaseCompletion;
	private boolean showCompletionMessage;
	private String messageOnCompletion;	
	private CompletionStrategy completionStrategy;	
	private Status status;
	private InstructionsScreen instructionsScreen;
	private MessageScreen messageScreen;	
	private ArrayList<String> instructions;
	private ArrayList<MissionGoal> openGoals;
	private ArrayList<MissionGoal> completedGoals;
	private ArrayList<PhaseListener> listeners;	
	
	/**
	 * Default constructor. Sets the completion strategy to ALL_GOALS and defines start as immediate
	 */
	public MissionPhase() {
		this.init();
	}
	
	/**
	 * Constructor
	 */
	public MissionPhase(CompletionStrategy completionStrategy, int secondsBeforeStart, boolean showCompletionMessage) {
		this.init();
		this.completionStrategy = completionStrategy;
		this.secondsBeforeStart = secondsBeforeStart;
		this.showCompletionMessage = showCompletionMessage;
	}
	
	/**
	 * Constructor
	 * @param instructions - Instructions to complete de phase
	 * @param showCompletionMessage - Flag that indicates the presence of a message screen on the completion of the phase  
	 * @param messageOnCompletion - Message to be shown on the completion of the phase
	 */
	public MissionPhase(ArrayList<String> instructions, boolean showCompletionMessage, String messageOnCompletion) {
		this.init();
		this.instructions = instructions;
		this.showCompletionMessage = showCompletionMessage;
		this.messageOnCompletion = messageOnCompletion;
	}
	
	/**
	 * Initialization of the private attributes
	 */
	private void init() {
		this.showCompletionMessage = false;
		this.completionStrategy = CompletionStrategy.ALL_GOALS;
		this.secondsBeforeStart = 0;
		this.status = Status.WAITING_TO_START;
		this.instructions = new ArrayList<String>();
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
			this.handleStatusComplete();
			break;
		
		default:
			break;
		}
	}
	
	/**
	 * Does stuff prior to the phase start
	 * @param world
	 */
	protected void handleStatusWaitingToStart(World world) {
		long remainingTime = this.getRemainingTimeOnStatus(this.secondsBeforeStart);
		
		if (remainingTime <= 0) {
			this.timeOnStatusSet = 0;
			this.status = Status.DISPLAYING_INSTRUCTIONS;			
		}
	}
	
	/**
	 * Get remaining time in the current status
	 * @return
	 */
	private long getRemainingTimeOnStatus(long timeLimit) {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();
		
		if (this.timeOnStatusSet == 0) {
			this.timeOnStatusSet = currentWorldTime;
		}	
		
		return Converter.getRemainingTimeInSeconds(this.timeOnStatusSet, timeLimit);
	}
	
	
	/**
	 * Shows the instructions of the phase
	 * @param world
	 */
	protected void handleStatusDisplayingInstructions(World world) {
		if (this.instructions.isEmpty()) {
			this.status = Status.RUNNING;
		} else {
			if (this.instructionsScreen == null) {
				this.instructionsScreen = new InstructionsScreen(this.instructions);
				this.instructionsScreen.addListener(this);
				Minecraft.getMinecraft().displayGuiScreen(this.instructionsScreen);
			}			
		}
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
				this.notifyAllAboutGoalAchievement(goal);
			} 
		}
		this.openGoals.removeAll(toBeRemoved);
		if ((this.completionStrategy == CompletionStrategy.ANY_GOAL && !toBeRemoved.isEmpty()) || this.openGoals.isEmpty()) {
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
	public void handleStatusComplete() {
		if (this.showCompletionMessage) {
			createAndOpenMessageScreen();		
			updateMessageScreen();
		} else {
			this.notifyAllAboutPhaseCompletion();
		}
	}
	
	/**
	 * Updates a message screen so it can dismiss after the time defined on its creation
	 */
	private void updateMessageScreen() {
		if (this.messageScreen != null) {
			this.messageScreen.update();
		}
	}
	
	/**
	 * Creates a message screen after some seconds and presents it to the user. Here we define a waiting time 
	 * before showing the screen because there can only be one active screen at a time. So if the goal is to open
	 * the inventory, for example, showing this screen immediately would dismiss the inventory before the user could 
	 * even see it.  
	 */
	private void createAndOpenMessageScreen() {
		int remainingTimeToShowMessageScreen = this.getRemainingSecondsToShowMessageScreen();
		
		if (remainingTimeToShowMessageScreen <= 0 && this.messageScreen == null) {
			this.messageScreen = new MessageScreen(this.messageOnCompletion, SECONDS_TO_DISMISS_MESSAGE_SCREEN);
			this.messageScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(this.messageScreen);
		}
	}
	
	private int getRemainingSecondsToShowMessageScreen() {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();
		
		if (this.worldTimeOnPhaseCompletion == 0) {
			this.worldTimeOnPhaseCompletion = currentWorldTime;
		}	
		
		return Converter.getRemainingTimeInSeconds(this.worldTimeOnPhaseCompletion, SECONDS_UNTIL_SHOW_MESSAGE_SCREEN);		
	}
	
	/**
	 * Reopens phase goals
	 */
	public void reset() {
		for(MissionGoal goal : this.completedGoals) {
			goal.reset();
		}
		this.openGoals = this.completedGoals;
		this.completedGoals.clear();		
	}

	@Override
	public void screenDismissed(GuiScreen screen, ButtonType buttonType) {
		if (screen.equals(this.instructionsScreen) && buttonType == ScreenListener.ButtonType.OK) {
			this.timeOnStatusSet = 0;
			this.status = Status.RUNNING;
		} else if (screen.equals(this.messageScreen)) {
			this.notifyAllAboutPhaseCompletion();
		}		
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
	private void notifyAllAboutGoalAchievement(MissionGoal goal) {		
		for (PhaseListener listener : this.listeners) {
			listener.goalAchieved(goal);			
		}
	}
	
	
	/**
	 * Set instructions for the phase
	 * @param instructions
	 */
	public void addInstructionsLine(String pieceOfInstructions) {
		this.instructions.add(pieceOfInstructions);
	}
	
	/**
	 * Adds a new goal to the phase
	 * @param goal - Goal to be achieved
	 */
	public void addGoal(MissionGoal goal) {
		this.openGoals.add(goal);
	}
	
	/**
	 * Adds listener to be notified upon phase completion
	 * @param listener - Listener
	 */
	public void addListener(PhaseListener listener) {
		this.listeners.add(listener);
	}
	
	/**
	 * Sets the number of seconds before the mission starts
	 * @param secondsBeforeStart - Waiting time in seconds
	 */
	public void setSecondsBeforeStart(long secondsBeforeStart) {
		this.secondsBeforeStart = secondsBeforeStart;
	}
	
	/**
	 * Indicates if a message should be rendered on the screen at the completion of the phase
	 * @param showCompletionMessage - Flag that indicates the presence of the message 
	 */
	public void setShowCompletionMessage(boolean showCompletionMessage) {
		this.showCompletionMessage = showCompletionMessage;
	}
	
	/**
	 * Sets the strategy for the completion of the phase
	 * @param completionStrategy - Strategy (all goals must be completed or just one of them)
	 */
	public void setCompletionStrategy(CompletionStrategy completionStrategy) {
		this.completionStrategy = completionStrategy;
	}
	
	/**
	 * Sets the message to be shown on the completion of the phase
	 * @param messageOnCompletion - Message to be shown on the completion of the phase
	 */
	public void setMessageOnCompletion(String messageOnCompletion) {
		this.messageOnCompletion = messageOnCompletion;
	}	
	
	
}
