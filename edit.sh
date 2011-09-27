#!/bin/bash

: ${EDITOR:=vim}

journal_dir="$HOME/usr/var/journal"
#gtdnextday_hs="$HOME/src/gtdnextday/gtdnextday.hs"  # for Cygin/runghc
#gtdnextday_exe="$HOME/src/gtdnextday/gtdnextday"  # for Linux/ghc precompiled
gtdnextday_exe="$HOME/src/gtdnextday/gtdnextday"

conf_use_gui () { false; }

cygwin_editor () {
    export HOME="$HOMEPATH"
    /cygdrive/c/Programs/vim72/gvim.exe "$(cygpath -w "$1")"
}

gtdnextday ()
{
    ## for Cygwin/runghc
    # gtdnextday_hs="`cygpath -m $gtdnextday_hs`"
    # runghc "$gtdnextday_hs" "$(cygpath -w "$1")" "$(cygpath -w "$2")"

    "$HOME/Software/gtdnextday/gtdnextday" "$@"
}

#############################################################
search_back_days_max=100

todays_journal="$journal_dir/`date -I -d '2 hours ago'`".txt

case `uname -o` in
    Cygwin) GUI_EDITOR=cygwin_editor;;
         *) : ${GUI_EDITOR:=gvim};;
esac

MSG () { echo "$@"; }

DIE () {
    echo "ERROR: $@" >&2
    exit 1
}

check_if_date_is_gnudate ()
{
    ! date | grep -q 'GNU'
}

gui_running ()
{
    case `uname -o` in
        Cygwin)
            true;;
        *)
            [ -n "$DISPLAY" ];;
    esac
}

use_gui () { gui_running && conf_use_gui; }

start_editor ()
{
    if use_gui
    then
        "${GUI_EDITOR}" "$1"
    else
        "${EDITOR}" "$1"
    fi
}

search_old_journal ()
{
    local file=""
    i=1
    until [ -e "$file" ]
    do
        file="$journal_dir/`date -I -d "$i day ago"`.txt"
        (( i++ ))
        if [ $i -ge $search_back_days_max ]
        then
            return 1
        fi
    done
    echo "$file"
}


if ! check_if_date_is_gnudate
then
    DIE "Need GNU version of 'date' command"
fi

if [ ! -e "$todays_journal" ]
then
    if oldfile=`search_old_journal`
    then
        MSG "Using $oldfile as template"
        case `uname -o` in
            Cygwin)
                oldfile="$(cygpath -m "$oldfile")"
                todays_journal="$(cygpath -m "$todays_journal")"
            ;;
        esac
        gtdnextday "$oldfile" "$todays_journal" ||
            DIE "gtdnextday failed for $oldfile"
    else
        MSG "No previous journal for these $search_back_days_max days"
        MSG "Start with empty file"
        printf "%s\n\n\nvim: ft=todo\n" "`date -I`" > "$todays_journal"
    fi
fi
MSG "Start editing $todays_journal"
ln -sf "$todays_journal" "$journal_dir/today.txt"

if use_gui 
then
    start_editor "$todays_journal" &
else
    start_editor "$todays_journal"
fi

