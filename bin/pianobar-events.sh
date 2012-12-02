#!/bin/bash

OUTFILE=~/.config/pianobar/output

# create variables
while read L; do
    k="`echo "$L" | cut -d '=' -f 1`"
    v="`echo "$L" | cut -d '=' -f 2`"
    export "$k=$v"
done < <(grep -e '^\(title\|artist\|album\|stationName\|songStationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin) # don't overwrite $1...

case "$1" in
    songstart | songlove)
        [ "$rating" -eq 1 ] && extra=" <3"
        echo -n \"$title\" by \"$artist\" $extra > $OUTFILE
        ;;

    songfinish)
        > $OUTFILE
        ;;

#    songshelf)
#        kdialog --title pianobar --passivepopup "SHELVING '$title' by '$artist' on '$album' on station '$stationName'" 10
#        ;;

#    songban)
#        kdialog --title pianobar --passivepopup "BANNING '$title' by '$artist' on '$album' on station '$stationName'" 10
#        ;;

#    songbookmark)
#        kdialog --title pianobar --passivepopup "BOOKMARKING '$title' by '$artist' on '$album'" 10
#        ;;

#    artistbookmark)
#        kdialog --title pianobar --passivepopup "BOOKMARKING '$artist'" 10
#        ;;

esac

