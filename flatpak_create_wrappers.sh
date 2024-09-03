#!/bin/bash

# Create wrapper scripts for all the programs running under flatpak.
# Hat tip to ssokolow. Adapted from https://unix.stackexchange.com/a/672466.
function flatpack_create_wrappers::main() {
    local bin_dir="${HOME}/bin/"

    # Remove any stale launch scripts.
    rm -f "${bin_dir}"/*

    for X in $(flatpak list --columns=ref); do
        app_command="$(flatpak info -m "$X" | grep command= | cut -d= -f2)"
        cmd_path="${bin_dir}"/"$app_command"

        if [[ -n "$app_command" ]]; then
            # Unset LD_PRELOAD to silence gtk-nocsd errors and support file.
            # forwarding so you can sandbox browsers and still open local files.
            printf '#!/bin/bash\n# -*- mode: shell-script; -*-\n\nunset LD_PRELOAD\nexec flatpak run --file-forwarding "%s" @@u "$@" @@' "$X" > "${cmd_path}"
            chmod +x "${cmd_path}"
        fi
    done
}

flatpack_create_wrappers::main
