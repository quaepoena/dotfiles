# -*- mode: Shell-script; -*-


function anki-format() {
    echo -e "$(cat "/dev/stdin" | tr '\n' ';')"
}


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

    rsync -az -e ssh --delete "${new}" "${old}"
    return "$?"
}


function declinatio() {
    if [[ "$#" -lt 1 ]]; then
	echo "Usage: declinatio file [args]" >&2
	return 1
    fi

    local declinatio_file="$1"
    source "${declinatio_file}"

    if [[ "$?" -ne 0 ]]; then
	echo "Error while sourcing ${declinatio_file}. Exiting." >&2
	return 1
    fi

    shift

    if [[ "$#" -ne "${argument_count}" ]]; then
	echo "${usage}" >&2
	return 1
    fi

    run "$@"
}


function declinatio::array_contains() {
    local -r element="$1"
    local -ra array="$2"

    for i in ${array[@]}; do
	if [[ "${element}" == "${i}" ]]; then
	    return 0
	fi
    done

    return 1
}


declinatio::check_mt() {
    if [[ "$#" -ne 1 ]]; then
	echo "Must pass one argument to declinatio::check_mt." >&2
	return 1
    fi

    # Relevant for verbs like "atmen."
    if [[ "$1" =~ (.*m)(s{,1}t.*) ]]; then
	echo "${BASH_REMATCH[1]}e${BASH_REMATCH[2]}"
    else
	echo "$1"
    fi
}


function declinatio::print_passive() {
    local string="$1"
    local -r mask="$2"
    local -ar allowed="$3"

    if [[ "$#" -lt 3 ]]; then
    	echo "At least three arguments needed for print_passive()." >&2
    	return 1
    fi

    # Account for passives that don't allow animate subjects.
    if declinatio::array_contains "${mask}" "tjn tnn"; then
	string="$(echo "${string}" | sed 's_er/sie/es_es_')"
    fi

    if declinatio::array_contains "${mask}" "${allowed}"; then
	echo "${string}"
    else
	echo "-"
    fi

    return 0
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
	echo "Geben Sie eine Zahl dei gröser als null ist." >&2
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


xmodmap ~/.Xmodmaprc

# for pinentry-tty
export GPG_TTY="$(tty)"

alias af="anki-format"
alias d="declinatio"
alias em="emacsclient -c &"
alias emd="env -u XMODIFIERS emacs --daemon"
alias emt="emacsclient -t"
alias l="$@ &>/dev/null"
alias x="xmodmap ~/.Xmodmaprc"
