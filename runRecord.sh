#/usr/bin/bash -u

X = 0

start=$SECONDS;
echo "testRecord231703012020" | ./record.sh & bg_pid=$!
#echo "testRecord231703012020" | ./record.sh & bg_pid=$!

while true; do
	echo "X = $X"
	echo "Sleeping for 1 seconds..."
	sleep 1
	end=$SECONDS;
	dt_sec=$(( end - start ))
	echo "$dt_sec seconds has elapsed"
	ps ax | grep ffmpeg
	if [ "$X" -ge 5 ];
	then
		kill -2 $bg_pid
		echo "$bg_pid is killed"
		echo "Exit"
		break
	fi
	X=$(($X+1))
done