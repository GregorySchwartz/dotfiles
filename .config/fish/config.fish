set PATH /home/gw/haskell/hdevtools/.cabal-sandbox/bin/ $PATH
set PATH /home/gw/haskell/yesod-bin/.cabal-sandbox/bin/ $PATH

# Prevent flickering in libreoffice in xmonad
set -x SAL_USE_VCLPLUGIN gen

# base16 colors in terminal
eval sh /home/gw/.config/base16-shell/base16-eighties.dark.sh

# Aliases
alias sesh "dtach -A /tmp/gw -r winch nvim"
