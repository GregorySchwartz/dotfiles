"
"                       __   _(_)_ __ ___  _ __ ___
"                       \ \ / / | '_ ` _ \| '__/ __|
"                        \ V /| | | | | | | | | (__
"                       (_)_/ |_|_| |_| |_|_|  \___|
"
" Author: G.W., with help from (joe di castro <joe@joedicastro.com>)
" Source (joe di castro): http://github.com/joedicastro/dotfiles/tree/master/vim

" Remember, you like #65a5cc color cursor

" Prerequisites (enable profiling in cabal)
" python    (sudo apt-get install python python3)
" ghc       (sudo apt-get install haskell-platform)
" ctags     (sudo apt-get install ctags)
" ghc-mod   (cabal install ghc-mod)
" hdevtools (cabal install hdevtools)
" hlint     (cabal install hlint)
" hoogle    (cabal install hoogle)
" lushtags  (cabal install lushtags)

" NOTE: NEED hlint FOR SYNTASTIC TO WORK WITH HASKELL!!!
" NOTE: NEED 'cabal install hdevtools' for vim-hdevtools to work!!

"=================================================================

set nocompatible             " No to the total compatibility with the ancient vi

" For fish
set shell=/bin/sh

set rtp+=$HOME/.vim/bundle/neco-ghc/

" Setup language {{{ ==========================================================

language en_US.UTF-8           " Solve some plugins incompatibilities

" }}}

" NEOBUNDLE {{{ ===============================================================

" NeoBundle auto-installation and setup {{{

" Auto installing NeoBundle
let iCanHazNeoBundle=1
let neobundle_readme=expand($HOME.'/.vim/bundle/neobundle.vim/README.md')
if !filereadable(neobundle_readme)
    echo "Installing NeoBundle.."
    echo ""
    silent !mkdir -p $HOME/.vim/bundle
    silent !git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
    let iCanHazNeoBundle=0
endif


" Call NeoBundle
if has('vim_starting')
    set rtp+=$HOME/.vim/bundle/neobundle.vim/
endif
call neobundle#begin(expand($HOME.'/.vim/bundle/'))

" is better if NeoBundle rules NeoBundle (needed!)
NeoBundle 'Shougo/neobundle.vim'
" }}}

" BUNDLES (plugins administrated by NeoBundle) {{{

" Shougo's way {{{

" Vimproc to asynchronously run commands (NeoBundle)
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

" Autocompletion
NeoBundle 'Shougo/neocomplete.vim'
"NeoBundle 'Valloric/YouCompleteMe'

" }}}

" Tmux {{{

" Vim Tmux Navigation
NeoBundle 'christoomey/vim-tmux-navigator'

" }}}

" Fuzzy file find {{{

" ctrlp
NeoBundle 'kien/ctrlp.vim'
" ctrlp-funky
NeoBundle 'tacahiroy/ctrlp-funky'
" ctrlp-extensions
NeoBundle 'sgur/ctrlp-extensions.vim'

" }}}
"
" Colorschemes {{{

" Dark themes
" Improved terminal version of molokai, almost identical to the GUI one
NeoBundle 'joedicastro/vim-molokai256'

NeoBundle 'tomasr/molokai'
NeoBundleLazy 'sjl/badwolf'
NeoBundleLazy 'nielsmadan/harlequin'

" Light themes
NeoBundleLazy 'vim-scripts/summerfruit256.vim'
NeoBundleLazy 'joedicastro/vim-github256'

" Make terminal themes from GUI themes
"NeoBundleLazy 'godlygeek/csapprox', { 'autoload' :
"        \ { 'commands' : ['CSApprox', 'CSApproxSnapshot']}}

" }}}

" DCVS {{{
"
" Admin Git
NeoBundle 'tpope/vim-fugitive'

" }}}

" Haskell {{{

" Haskell helpers
NeoBundleLazy 'Twinside/vim-hoogle', {'autoload': {'filetypes': ['haskell']}}
" Using vim2hs instead for now due to better syntax highlighting
NeoBundleLazy 'dag/vim2hs', {'autoload': {'filetypes': ['haskell']}}
"NeoBundleLazy 'travitch/hasksyn', {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'lukerandall/haskellmode-vim', {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'bitc/vim-hdevtools', {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'eagletmt/ghcmod-vim', {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'ujihisa/neco-ghc', {'autoload': {'filetypes': ['haskell']}}
NeoBundle 'pbrisbin/vim-syntax-shakespeare'

" }}}

" Python {{{

" A Python plugin
NeoBundleLazy 'klen/python-mode', {'autoload': {'filetypes': ['python']}}

" }}}

" Code Snippets {{{

" Powerful and advanced Snippets tool
NeoBundle 'SirVer/ultisnips'

" }}}

" Syntax {{{

NeoBundle 'scrooloose/syntastic'

" }}}

" Text editing {{{

" Fast navigation
NeoBundle 'justinmk/vim-sneak'
" Autocompletion of (, [, {, ', ", ...
NeoBundle 'delimitMate.vim'
" to surround vim objects with a pair of identical chars
NeoBundle 'tpope/vim-surround'
" extend repetitions by the 'dot' key
NeoBundle 'tpope/vim-repeat'
" toggle comments
NeoBundle 'tpope/vim-commentary'
" browse the vim undo tree
NeoBundleLazy 'sjl/gundo.vim', { 'autoload' : {'commands' : 'GundoToggle'}}
" reveals all the character info, Unicode included
NeoBundle 'tpope/vim-characterize'
" marks admin
NeoBundle 'kshenoy/vim-signature'
" Rainbow parentheses
NeoBundle 'kien/rainbow_parentheses.vim'
" Easy tab browsing
NeoBundle 'kien/tabman.vim'
" ctags in vim
NeoBundle 'majutsushi/tagbar', { 'autoload' : {'commands' : 'TagbarToggle'} }
" Tabular editing in vim
NeoBundle 'junegunn/vim-easy-align'

" }}}

" GUI {{{

" A better looking status line
NeoBundle 'bling/vim-airline'

" easily window resizing
NeoBundle 'jimsei/winresizer'

" }}}

" END BUNDLES }}}

call neobundle#end()

" Auto install the plugins {{{

" First-time plugins installation
if iCanHazNeoBundle == 0
    echo "Installing Bundles, please ignore key map error messages"
    echo ""
    :NeoBundleInstall
endif

" Check if all of the plugins are already installed, in other case ask if we
" want to install them (useful to add plugins in the .vimrc)
NeoBundleCheck

" }}}

filetype plugin indent on      " Indent and plugins by filetype

" END NEOBUNDLE }}}

" VIM Setup {{{ ===============================================================

" <Leader> mapping {{{

let mapleader=' '

" }}}

" Basic options {{{

scriptencoding utf-8
set encoding=utf-8              " setup the encoding to UTF-8
set fileencoding=utf-8          " setup the file encoding to UTF-8
set ls=2                        " status line always visible
set go-=T                       " hide the toolbar
set go-=m                       " hide the menu
" The next two lines are quite tricky, but in Gvim, if you don't do this, if you
" only hide all the scrollbars, the vertical scrollbar is showed anyway
set go+=rRlLbh                  " show all the scrollbars
set go-=rRlLbh                  " hide all the scrollbars
set visualbell                  " turn on the visual bell
"set cursorline                  " highlight the line under the cursor
set fillchars+=vert:│           " better looking for windows separator
set ttyfast                     " better screen redraw
set title                       " set the terminal title to the current file
set showcmd                     " shows partial commands
set hidden                      " hide the inactive buffers
set ruler                       " sets a permanent rule
set lazyredraw                  " only redraws if it is needed
set autoread                    " update a open file edited outside of Vim
set ttimeoutlen=0               " toggle between modes almost instantly
set backspace=indent,eol,start  " defines the backspace key behavior
set virtualedit=all             " to edit where there is no actual character

" }}}

"Moving around {{{

inoremap jk <esc>
nnoremap j gj
nnoremap k gk

" }}}

" Change the way text is displayed for bottom of vim {{{

set display+=lastline

" }}}

" Scrolloff, 999=middle {{{

set scrolloff=5

" }}}

" Searching {{{

set incsearch                   " incremental searching
set showmatch                   " show pairs match
set matchtime=3                 " tenths of a second to show the matching paren
set hlsearch                    " highlight search results
set smartcase                   " smart case ignore
set ignorecase                  " ignore case letters

" }}}

" History and permanent undo levels {{{

set history=10000
set undofile
set undoreload=10000

" }}}

" Make a dir if no exists {{{

function! MakeDirIfNoExists(path)
    if !isdirectory(expand(a:path))
        call mkdir(expand(a:path), "p")
    endif
endfunction

" }}}

" Backups {{{

set backup
set noswapfile
set backupdir=$HOME/.vim/tmp/backup/
set undodir=$HOME/.vim/tmp/undo/
set directory=$HOME/.vim/tmp/swap/
set viminfo+=n$HOME/.vim/tmp/viminfo

" make this dirs if no exists previously
silent! call MakeDirIfNoExists(&undodir)
silent! call MakeDirIfNoExists(&backupdir)
silent! call MakeDirIfNoExists(&directory)

" }}}

" Wildmenu {{{

set wildmenu                        " Command line autocompletion
set wildmode=list:longest,full      " Shows all the options

set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.bak,*.?~,*.??~,*.???~,*.~      " Backup files
set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.jar                            " java archives
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.stats                          " Pylint stats

" }}}

" Tabs, space and wrapping {{{

set expandtab                  " spaces instead of tabs
set tabstop=4                  " a tab = four spaces
set shiftwidth=4               " number of spaces for auto-indent
set softtabstop=4              " a soft-tab of four spaces
set autoindent                 " set on the auto-indent
set nojoinspaces               " when joining lines (gq), only have one space

" set formatoptions=qrn1ct
set textwidth=80
set colorcolumn=81

function! ToggleWrap()
    let s:nowrap_cc_bg = [22, '#005f00']
    redir => s:curr_cc_hi
    silent hi ColorColumn
    redir END
    let s:curr_cc_ctermbg = matchstr(s:curr_cc_hi, 'ctermbg=\zs.\{-}\s\ze\1')
    let s:curr_cc_guibg = matchstr(s:curr_cc_hi, 'guibg=\zs.\{-}\_$\ze\1')
    if s:curr_cc_ctermbg != s:nowrap_cc_bg[0]
        let g:curr_cc_ctermbg = s:curr_cc_ctermbg
    endif
    if s:curr_cc_guibg != s:nowrap_cc_bg[1]
        let g:curr_cc_guibg = s:curr_cc_guibg
    endif
    if &textwidth == 80
        set textwidth=0
        exec 'hi ColorColumn ctermbg='.s:nowrap_cc_bg[0].
                    \' guibg='.s:nowrap_cc_bg[1]
    elseif &textwidth == 0
        set textwidth=80
        exec 'hi ColorColumn ctermbg='.g:curr_cc_ctermbg.
                    \' guibg='.g:curr_cc_guibg
    endif
endfunction

nmap <silent><Leader>ew :call ToggleWrap()<CR>

" }}}

" Colorscheme {{{

set background=dark            " set a dark background
set t_Co=256                   " 256 colors for the terminal
if has('gui_running')
    colorscheme molokai
else
    colorscheme molokai
endif

" }}}

" Syntax Highlighting {{{

syntax enable                  " enable the syntax highlight
"let g:tex_conceal = ""        " use only if editing tex is slow

" }}}

" Font {{{

set guifont=PragmataPro\ for\ Powerline\ 11

" }}}

" Resize the divisions if the Vim window size changes {{{

au VimResized * exe "normal! \<c-w>="

" }}}

" Fast window moves {{{

" Go to next buffer
nnoremap <Leader>n :bnext<CR>

" Worthless now that I use the tmux package, but requires tmux

" nnoremap <C-h> <C-w>h
" nnoremap <C-j> <C-w>j
" nnoremap <C-k> <C-w>k
" nnoremap <C-l> <C-w>l

" }}}

" Fast window & buffer close and kill {{{

nnoremap <Leader>k <C-w>c
nnoremap <silent><Leader>K :bd<CR>

" }}}

" Toggle line numbers {{{

set number

nnoremap <silent><Leader>l :call ToggleRelativeAbsoluteNumber()<CR>
function! ToggleRelativeAbsoluteNumber()
  if !&number && !&relativenumber
      set number
      set relativenumber
  elseif &number && !&relativenumber
      set nonumber
      set relativenumber
  elseif !&number && &relativenumber
      set number
      set norelativenumber
  elseif &number && &relativenumber
      set nonumber
      set norelativenumber
  endif
endfunction

" }}}

" Show hidden chars {{{

:set list
nmap <Leader>eh :set list!<CR>
set listchars=tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶

" }}}

" Folding {{{

set foldmethod=marker

" Toggle folding

"nnoremap \ za
"vnoremap \ za

" }}}

" Cut/Paste {{{

" to/from the clipboard
map <Leader>y "*y
map <Leader>p "*p

" toggle paste mode
map <Leader>P :set invpaste<CR>

" }}}

" Autoload configuration when this file changes ($MYVIMRC) {{{

autocmd! BufWritePost vimrc source %

" }}}

" Spelling {{{

" turn on the spell checking and set the English language
nmap <Leader>se :setlocal spell spelllang=en<CR>
" turn off the spell checking
nmap <Leader>so :setlocal nospell <CR>
" jump to the next bad spell word
nmap <Leader>sn ]s
" suggest words
nmap <Leader>sp z=
" jump to the next bad spell word and suggests a correct one
nmap <Leader>sc ]sz=
" add word to the dictionary
nmap <Leader>sa zg
" }}}

" Save as root {{{

cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

" }}}

" Delete trailing whitespaces {{{

nmap <silent><Leader>et :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" }}}

" Toggle the Quickfix window {{{

function! s:QuickfixToggle()
    for i in range(1, winnr('$'))
        let bnum = winbufnr(i)
        if getbufvar(bnum, '&buftype') == 'quickfix'
            cclose
            lclose
            return
        endif
    endfor
    copen
endfunction
command! ToggleQuickfix call <SID>QuickfixToggle()

nnoremap <silent> <Leader>q :ToggleQuickfix<CR>

" }}}

" Text statistics {{{

" get the total of lines, words, chars and bytes (and for the current position)
map <Leader>es g<C-G>

" }}}

" Toggle the search results highlighting {{{

map <silent><Leader>eq :set invhlsearch<CR>

" }}}

" Quick exiting without save {{{

nnoremap <Leader>`` :qa!<CR>

" }}}

" Make the Y behavior similar to D & C {{{

nnoremap Y y$

" }}}

" END VIM SETUP }}}

" PLUGINS Setup {{{ ===========================================================

" ctrlp {{{ -------------------------------------------------------------

let g:ctrlp_extensions = ['funky', 'yankring']
map <silent><Leader>fo :CtrlP<CR>
map <silent><Leader>fb :CtrlPBuffer<CR>
map <silent><Leader>ft :CtrlPBufTag<CR>
map <silent><Leader>ff :CtrlPFunky<CR>
map <silent><Leader>fy :CtrlPYankring<CR>
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'

" }}}

" TabMan {{{ -------------------------------------------------------------

let g:tabman_toggle = '<silent><Leader>b'

" }}}

" Rainbow Parentheses {{{ ------------------------------------------------------

map <silent><Leader>r :RainbowParenthesesToggle<CR>

" Start on
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
" Incompatability with hasksyn
"au Syntax * RainbowParenthesesLoadBraces

" }}}

" EasyAlign {{{ -------------------------------------------------------------

" Start interactive EasyAlign in visual mode
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign with a Vim movement
nmap <Leader>a <Plug>(EasyAlign)

" Live interactive mode with preview
 vmap <Leader><Enter> <Plug>(LiveEasyAlign)
nmap <Leader>A       <Plug>(LiveEasyAlign)

" }}}

" Commentary {{{ -------------------------------------------------------------

nmap <Leader>c <Plug>CommentaryLine
xmap <Leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType python setlocal commentstring=#%s
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType puppet setlocal commentstring=#\ %s
augroup END

" }}}

" delimitmate {{{

let delimitMate_expand_space = 1

" }}}

" Fugitive {{{

nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>go :Gread<CR>
nnoremap <Leader>gR :Gremove<CR>
nnoremap <Leader>gm :Gmove<Space>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gB :Gbrowse<CR>
nnoremap <Leader>gp :Git! push<CR>
nnoremap <Leader>gP :Git! pull<CR>
nnoremap <Leader>gi :Git!<Space>
nnoremap <Leader>ge :Gedit<CR>
nnoremap <Leader>gE :Gedit<Space>

" for the diffmode
noremap <Leader>du :diffupdate<CR>

if !exists(":Gdiffoff")
    command Gdiffoff diffoff | q | Gedit
endif
noremap <Leader>dq :Gdiffoff<CR>
" }}}

" Gundo {{{ ------------------------------------------------------------------

nnoremap <Leader>u :GundoToggle<CR>

let g:gundo_preview_bottom = 1

" }}}

" Ultisnips {{{

let g:UltiSnipsExpandTrigger="<C-j>"

" }}}

" {{{ Haskell
" IMPORTANT HASKELL IDE STUFF

" For cabal programs
let $PATH = $PATH . ':' . expand("~/.cabal/bin")
" For hdevtools
"let g:syntastic_haskell_checkers=['hlint', 'ghc-mod']
"For haskell_mode
au BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/firefox"
let g:haddock_docdir = "/usr/share/doc/ghc-doc/html/"
" For vim2hs nice symbols for types
let g:haskell_conceal_wide = 0
let g:haskell_conceal = 0
let g:haskell_multiline_strings = 1

" Hotkeys for ghcmod-vim and hdevtools
autocmd FileType haskell nnoremap <buffer> <Leader>hl :GhcModLint<CR>
autocmd FileType haskell nnoremap <buffer> <Leader>hc :GhcModCheck<CR>
autocmd FileType haskell nnoremap <buffer> <Leader>ht :HdevtoolsType<CR>
autocmd FileType haskell nnoremap <buffer> <Leader>hq :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <Leader>hi :HdevtoolsInfo<CR>

" }}}

" Tagbar {{{ ------------------------------------------------------

nmap <silent><Leader>t :TagbarToggle<CR>

" For lushtags
if executable('lushtags')
    let g:tagbar_type_haskell = {
        \ 'ctagsbin' : 'lushtags',
        \ 'ctagsargs' : '--ignore-parse-error --',
        \ 'kinds' : [
            \ 'm:module:0',
            \ 'e:exports:1',
            \ 'i:imports:1',
            \ 't:declarations:0',
            \ 'd:declarations:1',
            \ 'n:declarations:1',
            \ 'f:functions:0',
            \ 'c:constructors:0'
        \ ],
        \ 'sro' : '.',
        \ 'kind2scope' : {
            \ 'd' : 'data',
            \ 'n' : 'newtype',
            \ 'c' : 'constructor',
            \ 't' : 'type'
        \ },
        \ 'scope2kind' : {
            \ 'data' : 'd',
            \ 'newtype' : 'n',
            \ 'constructor' : 'c',
            \ 'type' : 't'
        \ }
    \ }
endif

" }}}

" PythonMode {{{ -------------------------------------------------------------

let g:pymode_breakpoint_key = '<Leader>B'

let g:pymode_lint_checker = 'pylint,pep8,mccabe,pep257'
let g:pymode_lint_ignore = ''
let g:pymode_virtualenv = 0

let g:pymode_rope = 1
let g:pymode_rope_goto_def_newwin = 'new'
let g:pymode_rope_guess_project = 0
let g:pymode_rope_vim_completion = 1
let g:pymode_rope_always_show_complete_menu = 1

" }}}

" Neocomplete {{{

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_select = 0
let g:neocomplete#enable_refresh_always = 1
let g:neocomplete#max_list = 30
let g:neocomplete#min_keyword_length = 1
let g:neocomplete#sources#syntax#min_keyword_length = 1
let g:neocomplete#force_overwrite_completefunc = 1
let g:neocomplete#data_directory = $HOME.'/.vim/tmp/neocomplete'
" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

inoremap <expr> <tab> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr> <S-tab> pumvisible() ? "\<C-p>" : "\<TAB>"
" }}}

" Neco-ghc {{{

let g:necoghc_enable_detailed_browse = 1

" }}}

" Syntastic {{{

let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol  = '⚡'
let g:syntastic_style_warning_symbol  = '⚡'

" Checkers
let g:syntastic_tex_checkers=['chktex']

" }}}

" winresizer {{{

let g:winresizer_start_key = '<silent><leader>w'
" cancelar pulsando ESC
" let g:winresizer_finish_with_escape = 1
let g:winresizer_keycode_finish = 27

" }}}

" Airline {{{

set laststatus=2   " Always show the statusline
set noshowmode

let g:airline_theme='bubblegum'
let g:airline_powerline_fonts = 1

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_min_count = 1

" }}}

" Sneak {{{ -------------------------------------------------------------

let g:sneak#streak = 1

" }}}

" END PLUGINS SETUP }}}
