# -*- mode: Shell-script; -*-


# simple backup function, not to be trusted
function backup() {
    if [[ $# -lt 2 ]]; then
	echo "Usage: backup <new_directory/> <old_directory/> [--fat]" >&2
	return 1
    fi

    if ! [[ "$1" =~ /$ && "$2" =~ /$ ]]; then
	echo "The directory names must end with slashes." >&2
	echo "Usage: backup <new_directory/> <old_directory/>" >&2
	return 1
    fi

    local new="$1"
    local old="$2"
    local fat="$3"
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

    if [[ "$3" == "--fat" ]]; then
	rsync -va --modify-window=1 --partial --delete "${new}" "${old}"
    else
	rsync -vaz --partial -e ssh --delete "${new}" "${old}"
    fi

    return "$?"
}

# TODO: Is this behavior correct in other programs?
function error() {
    echo "error: $@" >&2
    kill -INT $$
}

function ins() {
    if [[ $# -ne 2 ]]; then
	echo "Bruk: ins <str> <fil>" >&2
	return 1
    fi
    sed -i "1i$1" "${2}"
}

# TODO: Expand this to behave more like head(1) and tail(1), including
# reading from STDIN and looping through "$@".
# TODO: This is too hacky. Write it in another language.
function line() {
    if [[ $# -ne 2 ]]; then
	echo "Bruk: line <linje> <fil> …" >&2
	return 1
    fi

    local num="$1"
    local fil="$2"
    local lines="$(wc -l "${fil}" | cut -d ' ' -f 1)"

    if [[ "${num}" -gt "${lines}" || "${num}" -lt 1 ]]; then
	return 1
    fi

    head -n "${num}" "${fil}" | tail -n 1
}

function member() {
    if [[ $# -lt 2 ]]; then
	error "Bruk: ${FUNCNAME[0]} <item> <list>"
    fi

    local item="$1"
    shift
    local -a list="$@"

    for i in ${list[@]}; do
	if [[ "${item}" == "${i}" ]]; then
	    return 0
	fi
    done

    return 1
}

function pw() {
    if [[ $# -ne 1 ]]; then
	echo "Bruk: pw fil" >&2
	return 1
    fi

    if [[ "$1" =~ ^.+\.gpg$ ]]; then
	gpg -qd "${1}";
    else
	gpg -qd ~/pw/"${1}".gpg;
    fi
}


function gs-pdf-concat() {
    if [[ $# -lt 2 ]]; then
	echo "Usage: gs-concat <fil01.pdf> <fil02.pdf> […]" >&2
	return 1
    fi

    local time="$(date "+%Y%m%d%H%M")"
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -o /tmp/gs-pdf-concat-output-"${time}".pdf "$@"
}


function gs-pages() {
    if [[ $# -ne 2 ]]; then
	echo "Usage: gs-pages <input> <page range>" >&2
	return 1
    fi

    local time="$(date "+%Y%m%d%H%M")"
    ghostscript -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sPageList="$2" -o /tmp/gs-output-"${time}".pdf "$1"
}

# Prepend the first file to the second.
# TODO: Write this in C.
function prepend() {
    if [[ $# -ne 2 ]]; then
	error "Bruk: ${FUNCNAME[0]} <file> <file>"
    fi

    local tmp="$(mktemp)"

    cp "$2" "${tmp}"
    cp "$1" "$2"
    cat "${tmp}" >> "$2"

    rm "${tmp}"
}

function timer() {
    if [[ $# -ne 1 ]]; then
	echo "Geben Sie eins Zahl als Eingabewert." >&2
	echo "Benutzung: ${FUNCNAME[0]} <Zahl>." >&2
	return 1
    fi

    if ! [[ "$1" -gt 0 ]]; then
	echo "Geben Sie eine Zahl die gröser als null ist." >&2
	echo "Benutzung: ${FUNCNAME[0]} <Zahl>." >&2
	return 1
    fi

    date +%T
    sleep $(( 60 * "$1" )) && xeyes

}

function veke() {
    date "+%V"
}


if [[ -f ~/.bash_aliases_mach_specific ]]; then
    . ~/.bash_aliases_mach_specific
fi

# for pinentry-tty
export GPG_TTY="$(tty)"

alias c="setxkbmap -option 'compose:rctrl'"
alias em="emacsclient -c &"
alias emd="env -u XMODIFIERS emacs --daemon"
alias emt="emacsclient -t"
alias fu='sudo $(history -p !!)'
alias x="xmodmap ~/.Xmodmaprc"

# https://tex.stackexchange.com/questions/1092/how-to-install-vanilla-texlive-on-debian-or-ubuntu
export PATH="/usr/local/texlive/2022/bin/x86_64-linux:${PATH}"

# TODO: Emacs was unable to find info files when this line was active. Fix?
#export INFOPATH="${INFOPATH}:/usr/local/texlive/2021/texmf-dist/doc/info"
#export MANPATH="${MANPATH}:/usr/local/texlive/2021/texmf-dist/doc/man"
export TEXMFHOME="${HOME}/texmf"

export -f error
export -f line
export -f member
