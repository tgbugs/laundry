# Setups system and user profiles and related variables
# /etc/profile will be sourced by bash automatically
# Setups home environment profile
if [ -f ~/.profile ]; then source ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
     export HISTFILE=$XDG_CACHE_HOME/.bash_history
