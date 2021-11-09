property _config : ""
on initialize()
	-- Get the current directory 
	set _current_path to ""
	tell application "Finder"
		set _current_path to POSIX path of (container of (path to me) as string)
	end tell

	-- Read and parse config file
	set _config to {}
	set _config_file to _current_path & "config.txt"
	set _lines to paragraphs of (read POSIX file _config_file)
	repeat with _line in _lines
		set AppleScript's text item delimiters to {"="}
		set _line to every text item of _line
		set AppleScript's text item delimiters to {""}
		set end of _config to _line
	end repeat
end initialize

on get_config_value(key)
        repeat with _entry in _config
		if (item 1 of _entry) = key then
			return (item 2 of _entry)
		end if
        end repeat
end get_config_value
