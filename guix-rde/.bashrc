# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

inst () {
    set -x
    raco pkg install --deps search-auto laundry
    { retval="$?";
      set +x; } 2>/dev/null
    return $retval
}

test () {
    set -x
    raco test --package laundry
    { retval="$?";
      set +x; } 2>/dev/null
    return $retval
}

inst_test () {
    inst && test
}

guix_prompt () {
    cat << EOF
=========================================
  ____ _   _ _   _    ____       _
 / ___| \ | | | | |  / ___|_   _(_)_  __
| |  _|  \| | | | | | |  _| | | | \ \/ /
| |_| | |\  | |_| | | |_| | |_| | |>  <
 \____|_| \_|\___/   \____|\__,_|_/_/\_\\

Available commands: inst, test, inst_test
=========================================
EOF
}

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && guix_prompt

    # Don't do anything else.
    return
fi

# Source the system-wide file.
# source /etc/bashrc

guix_prompt

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
alias clear="printf '\e[2J\e[H'"
