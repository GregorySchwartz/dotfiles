####################
# Variables
####################

#set fish_greeting ( fortune | cowsay )

set -x PATH /home/gw/.local/bin/ $PATH
set -x PATH /home/gw/.cabal/bin/ $PATH
set -x PATH /usr/bin/core_perl/ $PATH

# Prevent flickering in libreoffice in xmonad
set -x SAL_USE_VCLPLUGIN gen

####################
# Aliases
####################

# Use tvim within nvim to open up a file in the host nvim instance from a nested
# terminal
alias tvim "python2 /home/gw/git_repos/dotfiles/bin/tvim.py"

# For sessions
alias sesh "dtach -A /tmp/gw -r winch nvim"

####################
# Functions
####################

function fish_greeting
    fortune | cowsay
end
