#!/bin/sh -e
# Portable install version that supports -D -m and -t
# We have our own extension flag -s for running sed on the given files while
# installing.
usage() {
    printf '%s\n' "usage: $0 [-D] [-m mode] [-s sedcmd] source dest" \
                  "   or: $0 [-D] [-m mode] [-s sedcmd] [-t dir] [source...]" >&2
    exit 0
}

die() { printf '%s\n' "$@" >&2; exit 1;}

sed=''
mkdirp=''
target=''
mode=''
REST=''

while getopts 'Dm:s:t:h' opt; do
    case $opt in
        D) mkdirp=1 ;;
        s) sed=$OPTARG ;;
        t) target=$OPTARG ;;
        m) mode=$OPTARG ;;
        h) usage
           exit 0
           ;;
        '?') exit 1
    esac
done

shift "$((OPTIND - 1))"

if [ "$target" ]; then
    [ "$mkdirp" ] || [ -d "$target" ] || die "$target doesn't exist"
    mkdir -p "$target"
    for arg; do
        [ -d "$target/${arg##*/}" ] && die "$target/${arg##*/} is a directory"
        if [ "$sed" ]; then
            sed "$sed" < "$arg" > "$target/${arg##*/}"
        else
            cp "$arg" "$target"
        fi

        # Most implementations set the mode to 0755 by default when -t is set.
        chmod "${mode:=0755}" "$target/${arg##*/}"
    done
else
    case "$2" in */*) [ "$mkdirp" ] || [ -d "${2%/*}" ] || die "${2%/*} doesn't exist"
                      mkdir -p "${2%/*}"
    esac
    [ -d "$2" ] && die "$2 is a directory"
    if [ "$sed" ]; then sed "$sed" < "$1" > "$2"; else cp "$1" "$2"; fi
    chmod "${mode:=0755}" "$2"
fi
