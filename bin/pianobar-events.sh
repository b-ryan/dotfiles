#!/bin/bash

OUTFILE=~/.config/pianobar/output
PAUSED="(Paused)"
THUMBS_UP="<3"

# create variables
while read L; do
    k="`echo "$L" | cut -d '=' -f 1`"
    v="`echo "$L" | cut -d '=' -f 2`"
    export "$k=$v"
done < <(grep -e '^\(title\|artist\|album\|stationName\|songStationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin)

xmobar () { echo -n $@ > $OUTFILE; }

case "$1" in
    songstart | songlove | playpause | playresume)
        [ "$1" = "playpause" ] && pre="$PAUSED "
        [ "$rating" -eq 1 ] && post=" $THUMBS_UP"
        xmobar $pre\"$title\" by \"$artist\"$post
        ;;

    songfinish)
        > $OUTFILE
        ;;
esac
