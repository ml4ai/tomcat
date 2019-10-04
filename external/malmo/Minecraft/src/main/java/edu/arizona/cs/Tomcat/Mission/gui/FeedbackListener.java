package edu.arizona.cs.Tomcat.Mission.gui;

import edu.arizona.cs.Tomcat.Emotion.EmotionHandler;

public interface FeedbackListener {
	
	/**
	 * Informs when an emotion was reported 
	 * @param emotion
	 */
	void emotionProvided(EmotionHandler.Emotion emotion);
}
