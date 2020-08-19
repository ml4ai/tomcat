Set-up instructions for Mumble
==============================

MacOS
-----

- Run the following from the root of the ToMCAT repository.

    ./tools/mumble_helper install

- Launch Mumble with the following command.

    ./tools/mumble_helper launch

The first time you launch Mumble, you'll be prompted to configure Mumble's
audio settings, which can do at that time itself, or later on by going to
`Configure > Audio Wizard`.

- Click 'next' on the first page of the setup:
![](2.png)

- Device selection: 'default' option:
![](3.png)

- Volume tuning: choose lowest value(default 10ms, lowest)
![](4.png)

- Voice Activity detection: Here, keep the Control Panel also open:
![](5.png)
While following the instructions, remember to lower the input volume in the
control panel so that when you speak with a loud, excited tone, the bar stays
in the blue/green zone.
![](6.png)

- Voice Activity Detection: to setup up Push to Talk, click on th text box next
  to 'Push to Talk' and click on a suitable shortcut on your keyboard.
  ![](7.png) For continuous recording, 'Raw Amplitude' gave better speech
  detection results. Adjust the slider until your speech is in the yellow or
  green zone.

- Quality and notifications: Custom setting is appropriate if one anticipates
  bandwidth issues and has setup a custom value in `murmur.ini`. Else, choose
  'high'. Disable Text-to-speech, so that the recording is not interrupted by
  the TTS: [Note: TTS can also be disabled from the 'configuration' option in
  the Mumble toolbar.] ![](8.png)


- Positional Audio: click 'Next'
![](9.png)

- Click 'Finish' on the last page:
![](10.png)
