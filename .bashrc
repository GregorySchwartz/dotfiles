#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# hidpi for gtk applications
export GDK_SCALE=2

# qt5 themes. Use qt5ct to configure the theme.
export QT_STYLE_OVERRIDE="kde"
export QT_QPA_PLATFORMTHEME="qt5ct"
