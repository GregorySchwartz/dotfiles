set PATH /home/gw/haskell/hdevtools/.cabal-sandbox/bin/ $PATH
set PATH /home/gw/haskell/yesod-bin/.cabal-sandbox/bin/ $PATH
set PATH /home/gw/haskell/ghc-mod/.cabal-sandbox/bin/ $PATH
set PATH /home/gw/.cabal/bin/ $PATH

# Use the correct gtk theme
set -x SAL_USE_VCLPLUGIN "gtk3 lowriter"

####################
# Aliases
####################

# Use tvim within nvim to open up a file in the host nvim instance from a nested
# terminal
alias tvim "python2 /home/gw/git_repos/dotfiles/bin/tvim.py"

# For sessions
alias sesh "dtach -A /tmp/gw -r winch nvim"
