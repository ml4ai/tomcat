package edu.arizona.tomcat.Mission.gui;

import edu.arizona.tomcat.Emotion.EmotionHandler;

public interface FeedbackListener {

    /**
     * Informs when an emotion was reported
     * @param emotion
     */
    void emotionProvided(EmotionHandler.Emotion emotion);
}
