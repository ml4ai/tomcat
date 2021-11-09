property _total_spaces : 5

on go_to_space(_space_num)
	-- Return to first space
	repeat _total_spaces times
		tell application "System Events"
			key code 123 using control down
		end tell
	end repeat
	-- Go to requested space
        repeat _space_num-1 times
                tell application "System Events"
                        key code 124 using control down
                end tell
        end repeat
end next_space
