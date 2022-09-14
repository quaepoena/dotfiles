# -*- mode: Shell-script; -*-


function anki-format() {
    echo -e "$(cat "/dev/stdin" | tr '\n' ';')"
}


# simple backup function, not to be trusted
function backup() {
    if [[ "$#" -ne 2 ]]; then
	echo "Usage: backup <new_directory/> <old_directory/>" >&2
	return 1
    fi

    if ! [[ "$1" =~ /$ && "$2" =~ /$ ]]; then
	echo "The directory names must end with slashes." >&2
	echo "Usage: backup <new_directory/> <old_directory/>" >&2
	return 1
    fi

    local new="$1"
    local old="$2"
    local endpoint1="dummy"
    local endpoint2=""

    if [[ "${new}" =~ (.*/)*(.+/)$ ]]; then
	endpoint1="${BASH_REMATCH[2]}";
    fi

    if [[ "${old}" =~ (.*/)*(.+/)$ ]]; then
	endpoint2="${BASH_REMATCH[2]}"
    fi

    if [[ "${endpoint1}" != "${endpoint2}" ]]; then
	echo "The two endpoints are not the 'same' directory." >&2
	echo "Usage: backup <new_directory/> <old_directory/>" >&2
	return 1
    fi

    rsync -vaz --partial -e ssh --delete "${new}" "${old}"
    return "$?"
}


function error() {
    echo "$@" >&2
    return 1
}


function pw() {

    if [[ "$#" -ne 1 ]]; then
	echo "Bruk: pw fil" >&2
	return 1
    fi

    gpg -qd ~/pw/"${1}".gpg;
}


function timer() {
    if [[ "$#" -ne 1 ]]; then
	echo "Geben Sie eins Zahl als Eingabewert." >&2
	echo "Benutzung: timer <Zahl>." >&2
	return 1
    fi

    if ! [[ "$1" -gt 0 ]]; then
	echo "Geben Sie eine Zahl die gröser als null ist." >&2
	echo "Benutzung: timer <Zahl>." >&2
	return 1
    fi

    sleep $(( 60 * "$1" )) && xeyes

}

function veke() {
    date "+%V"
}


if [[ -f ~/.bash_aliases_mach_specific ]]; then
    . ~/.bash_aliases_mach_specific
fi

if [[ -f ~/declinatio/declinatio.sh ]]; then
    . ~/declinatio/declinatio.sh
fi


# for pinentry-tty
export GPG_TTY="$(tty)"

alias af="anki-format"
alias d="declinatio"
alias em="emacsclient -c &"
alias emd="env -u XMODIFIERS emacs --daemon"
alias emt="emacsclient -t"
alias l="$@ &>/dev/null"
alias x="xmodmap ~/.Xmodmaprc"
