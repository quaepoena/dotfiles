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

    if [[ "$1" =~ ^.+\.gpg$ ]]; then
	gpg -qd "${1}";
    else
	gpg -qd ~/pw/"${1}".gpg;
    fi
}


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

# TODO: Why is this here?
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
alias x="xmodmap ~/.Xmodmaprc"

# https://tex.stackexchange.com/questions/1092/how-to-install-vanilla-texlive-on-debian-or-ubuntu
export PATH="/usr/local/texlive/2022/bin/x86_64-linux:${PATH}"

# TODO: Emacs was unable to find info files when this line was active. Fix?
#export INFOPATH="${INFOPATH}:/usr/local/texlive/2021/texmf-dist/doc/info"
#export MANPATH="${MANPATH}:/usr/local/texlive/2021/texmf-dist/doc/man"
export TEXMFHOME="${HOME}/texmf"
