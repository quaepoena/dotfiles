# -*- mode: Shell-script; -*-


function launch() {
    local -r temp="$(mktemp)"
    echo "'$@' skal til ${temp}."
    echo "Køyrer '$@' på $(date)" > "${temp}"
    "$@" &>>"${temp}" &
}

function pw() {

    if [[ "$#" -ne 1 ]]; then
	echo "Bruk: pw fil" >&2
	return 1
    fi

    gpg -qd ~/pw/"${1}".gpg;
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

function declinatio::begins_with() {
    local -r word="$1"
    local -r beginning="$2"
    local -r stem="${word#${beginning}}"

    if [[ "$(( "${#stem}" + "${#beginning}" == "${#word}" ))" -ne 1 ]]; then
	return 1
    fi

    return 0
}

function declinatio::ends_with() {
    local -r word="$1"
    local -r ending="$2"
    local -r stem="${word%${ending}}"

    if [[ "$(( "${#stem}" + "${#ending}" == "${#word}" ))" -ne 1 ]]; then
	return 1
    fi

    return 0
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
#    elif [[ "$1" =~ (.*m)(t.*) ]]; then
#	echo "${BASH_REMATCH"
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

function anki-format() {
    echo -e "$(cat "/dev/stdin" | tr '\n' ';')"
}

function veke() {
    date "+%V"
}


if [ -f ~/.bash_aliases_mach_specific ]; then
    . ~/.bash_aliases_mach_specific
fi


xmodmap ~/.Xmodmaprc


export GPG_TTY="$(tty)"

alias x='xmodmap ~/.Xmodmaprc'
alias af="anki-format"
alias d="declinatio"
alias l="launch"
