#!/bin/bash

: ${EDITOR:=vim}
case `uname -o` in
    Cygwin) GUI_EDITOR=cygwin_editor;;
         *) : ${GUI_EDITOR:=gvim};;
esac

cygwin_editor () {
    export HOME="$HOMEPATH"
    /cygdrive/c/Programs/vim72/gvim.exe "$(cygpath -w "$1")"
}

journal_dir=$HOME/Dropbox/GTD/journal
todays_journal="$journal_dir/`date -I -d '2 hours ago'`".txt

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

start_editor ()
{
    if gui_running
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
    done
    echo "$file"
}

gtdnextday ()
{
    case `uname -o` in
        Cygwin)
            gtdnextday_hs="$journal_dir/../../Software/gtdnextday/gtdnextday.hs"
            gtdnextday_hs="`cygpath -m $gtdnextday_hs`"
            runghc "$gtdnextday_hs" "$(cygpath -w "$1")" "$(cygpath -w "$2")"
            ;;
        *)
            gtdnextday_exe="$journal_dir/../gtdnextday"
            if [ ! -x "$gtdnextday_exe" ]; then
                chmod +x "$gtdnextday_exe"
            fi
            "$gtdnextday_exe" "$@"
            ;;
    esac
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
        DIE "No current or old journal found"
    fi
fi
MSG "Start editing $todays_journal"
ln -sf "$todays_journal" "$journal_dir/today.txt"
start_editor "$todays_journal" &

