####################
# Variables
####################

set -x PATH /home/gw/.local/bin/ $PATH
set -x PATH /home/gw/.cabal/bin/ $PATH
set -x PATH /usr/bin/core_perl/ $PATH

# Prevent flickering in libreoffice in xmonad
set -x SAL_USE_VCLPLUGIN gtk

# qt5 themes. Use qt5ct to configure the theme.
set -x QT_STYLE_OVERRIDE kde
set -x QT_QPA_PLATFORMTHEME kde

# Default editor

set -x ALTERNATE_EDITOR ""
set -x EDITOR "emacsclient -t"                  # $EDITOR opens in terminal
set -x VISUAL "emacsclient -c -a emacs"         # $VISUAL opens in GUI mode

####################
# Aliases
####################

# Use tvim within nvim to open up a file in the host nvim instance from a nested
# terminal
alias tvim "python2 /home/gw/git_repos/dotfiles/bin/tvim.py"

# For sessions
alias sesh "dtach -A /tmp/gw_sesh -r winch nvim"

# For emacs sessions, fixing any weird hidpi issues.
alias e "env GDK_SCALE=1 emacsclient"

####################
# Functions
####################

function fish_greeting
    fortune | cowsay
end

####################
# Prompts
####################

# Play nice with emacs.
switch "$TERM"
    case "dumb"
        function fish_title; end
    case "eterm-color"
        function fish_title; end
end
