#!/bin/bash

# Output result: first value - volume level in %; space; the second value is the status of the sound (on or off)
amixerStatus=$(amixer get Master | egrep 'Playback.*?\[o' | egrep -o '\[.+\]' | tr -d [] | uniq)
# Output result, two values (on or off)
soundStatus=$(echo $amixerStatus | tr -d [:digit:] | tr -d [:blank:] | tr -d '%')
# Output result, volume level in %
volumeStatus=$(echo $amixerStatus | tr -d [:alpha:] | tr -d [:blank:] )

# Result variable
# non-empty value if the data is not received in a variable
result='N/A'

# If the sound is on
if [[ "$soundStatus" == "on" ]]
then 
	result=$volumeStatus
else
	result="muted"
fi

# Result volume value in % or muted
echo $result
