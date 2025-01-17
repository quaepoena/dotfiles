# -*- mode: Shell-script; -*-


# Append a(n existing) directory to PATH without duplication.
# Adapted slightly from https://superuser.com/a/39995.
function append-to-path() {
    if [[ -d "$1" ]] && [[ ":${PATH}:" != *":$1:"* ]]; then
        PATH="${PATH:+"${PATH}:"}$1"
    fi
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

    local time="$(date "+%Y%m%d%H%M%S")"
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -o /tmp/gs-pdf-concat-output-"${time}".pdf "$@"
    echo "Fasciculus scriptus est: /tmp/gs-pdf-concat-output-${time}.pdf" >&2
}


function gs-pages() {
    if [[ $# -ne 2 ]]; then
	    echo "Usage: gs-pages <input> <page range>" >&2
	    return 1
    fi

    local time="$(date "+%Y%m%d%H%M%S")"
    ghostscript -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sPageList="$2" -o /tmp/gs-output-"${time}".pdf "$1"
}


function pdf-paginae-vacae-per-totum-indere() {
    if [[ "$#" -ne 3 ]]; then
        echo "Ūsus: ${FUNCNAME[0]} <numerus pāginārum scrīptī> <fōns.pdf> <ēventus.pdf>" >&2
        return 1
   fi

    local numerus="$1"
    local fons="$2"
    local eventus="$3"
    local vacua="/tmp/pāgina-vacua.pdf"

    pdf-paginam-vacuam-facere

    pars_iussus="$(for n in $(seq "${numerus}"); do
        echo B1 "A${n}"
        done | xargs)"

    pdftk A="${fons}" B="${vacua}" cat ${pars_iussus} B1 output "${eventus}"
}

# https://unix.stackexchange.com/a/15995
function pdf-paginam-vacuam-indere() {
    if [[ "$#" -ne 3 ]]; then
        echo "Ūsus: ${FUNCNAME[0]} <numerus pāginae> <fōns.pdf> <ēventus.pdf>" >&2
        return 1
   fi

    local numerus="$1"
    local fons="$2"
    local eventus="$3"
    local vacua="/tmp/pāgina-vacua.pdf"

    pdf-paginam-vacuam-facere

    pdftk A="${fons}" B="${vacua}" cat "A1-$(( ${numerus} - 1 ))" B1 "A${numerus}-end" output "${eventus}"
}

function pdf-paginam-vacuam-facere() {
    echo "" | ps2pdf -sPAPERSIZE=a4 - /tmp/pāgina-vacua.pdf
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


function ssh-add-init() {
    ssh-add -l &>/dev/null
    if [[ "$?" -eq 1 ]]; then
        ssh-add
    fi
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

# Reuse existing ssh-agent. Adapted (slightly) from
# https://superuser.com/a/1469727 and https://unix.stackexchange.com/a/132117.
export SSH_AUTH_SOCK="${HOME}/.ssh/ssh-agent.sock"

# Test whether SSH_AUTH_SOCK is valid.
ssh-add -l &>/dev/null

# If not valid, then start ssh-agent using SSH_AUTH_SOCK.
[[ "$?" -ge 2 ]] && ssh-agent -a "${SSH_AUTH_SOCK}" >/dev/null

ssh-add-init

# for pinentry-tty
export GPG_TTY="$(tty)"

alias em="emacsclient -c &"
alias emd="env -u XMODIFIERS emacs --daemon"
alias emt="emacsclient -t"
alias fu='sudo $(history -p !!)'
alias x="xmodmap ~/.Xmodmaprc"

append-to-path "/usr/local/texlive/2024/bin/x86_64-linux"
export TEXMFHOME="${HOME}/texmf"

export -f error
export -f line
export -f member
