set PATH /home/gw/haskell/hdevtools/.cabal-sandbox/bin/ $PATH
set PATH /home/gw/haskell/yesod-bin/.cabal-sandbox/bin/ $PATH

# Truecolor in nvim
set -x NVIM_TUI_ENABLE_TRUE_COLOR 1

# base16 colors in terminal
eval sh /home/gw/.config/base16-shell/base16-wasteland.dark.sh

# Prevent flickering in libreoffice in xmonad
set -x SAL_USE_VCLPLUGIN gen
