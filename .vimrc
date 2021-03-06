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

" Plugin manager auto-installation and setup {{{

" Auto installing plugin manager
let installed_plug=1
if !filereadable(expand('~/.vim/autoload/plug.vim'))
    echo "Installing plugin manager..."
    echo ""
    silent !mkdir -p $HOME/.vim/bundle
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    let installed_plug=0
endif


" Call plugin manager
call plug#begin('~/.vim/plugged')

" BUNDLES (plugins administrated by vim-plug) {{{

" Additional steps {{{

" Vimproc to asynchronously run commands (NeoBundle)
Plug 'Shougo/vimproc', { 'do': 'make' }

" Autocompletion
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }

" }}}

" Tmux {{{

" Vim Tmux Navigation
Plug 'christoomey/vim-tmux-navigator'

" }}}

" Fuzzy file find {{{

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

" }}}
"
" Colorschemes {{{

" Dark themes
Plug 'chriskempson/base16-vim'
Plug 'tomasr/molokai'
Plug 'morhetz/gruvbox'
Plug 'sjl/badwolf'
Plug 'nielsmadan/harlequin'

" }}}

" DCVS {{{
"
" Admin Git
Plug 'tpope/vim-fugitive'

" }}}

" Shortcuts {{{

" More setting shortcuts
Plug 'tpope/vim-unimpaired'

" }}}

" Haskell {{{

" Haskell helpers
Plug 'Twinside/vim-hoogle', {'for': ['haskell']}
" Using vim2hs instead for now due to better syntax highlighting
Plug 'dag/vim2hs', {'for': ['haskell']}
"Plug 'travitch/hasksyn', {'for': ['haskell']}
Plug 'lukerandall/haskellmode-vim', {'for': ['haskell']}
Plug 'bitc/vim-hdevtools', {'for': ['haskell']}
Plug 'eagletmt/ghcmod-vim', {'for': ['haskell']}
Plug 'ujihisa/neco-ghc', {'for': ['haskell']}
Plug 'pbrisbin/vim-syntax-shakespeare'

" }}}

" Python {{{

" A Python plugin
Plug 'klen/python-mode', {'for': ['python']}

" }}}

" Code Snippets {{{

" Powerful and advanced Snippets tool
Plug 'SirVer/ultisnips'

" }}}

" Syntax {{{

"Plug 'scrooloose/syntastic'
Plug 'benekastah/neomake'

" }}}

" Text editing {{{

" Fast navigation
Plug 'Lokaltog/vim-easymotion'
" Autocompletion of (, [, {, ', ", ...
Plug 'delimitMate.vim'
" to surround vim objects with a pair of identical chars
Plug 'tpope/vim-surround'
" extend repetitions by the 'dot' key
Plug 'tpope/vim-repeat'
" toggle comments
Plug 'tpope/vim-commentary'
" browse the vim undo tree
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
" reveals all the character info, Unicode included
Plug 'tpope/vim-characterize'
" marks admin
Plug 'kshenoy/vim-signature'
" Rainbow parentheses
Plug 'kien/rainbow_parentheses.vim'
" ctags in vim
Plug 'majutsushi/tagbar'
" Tabular editing in vim
Plug 'junegunn/vim-easy-align'
" Convert cases
Plug 'tpope/vim-abolish'

" }}}

" GUI {{{

" A better looking status line
Plug 'bling/vim-airline'

" easily window resizing
Plug 'jimsei/winresizer'

" }}}

" END BUNDLES }}}

call plug#end()

" Auto install the plugins {{{

" First-time plugins installation
if installed_plug == 0
    echo "Installing Bundles..."
    echo ""
    :PlugInstall
endif

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
set cursorline                  " highlight the line under the cursor
set cursorcolumn                " highlight the column under the cursor
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
set number                      " Show line numbers

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
let base16colorspace=256       " Access colors present in 256 colorspace 
set t_Co=256                   " 256 colors for the terminal
colorscheme base16-default

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

" Show hidden chars {{{

:set list
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

" Quick exiting without save {{{

nnoremap <Leader>`` :qa!<CR>

" }}}

" Make the Y behavior similar to D & C {{{

nnoremap Y y$

" }}}

" END VIM SETUP }}}

" PLUGINS Setup {{{ ===========================================================

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

" Enable omni completion.
"autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
"autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
"autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"inoremap <expr> <tab> pumvisible() ? "\<C-n>" : "\<TAB>"
"inoremap <expr> <S-tab> pumvisible() ? "\<C-p>" : "\<TAB>"
" }}}

" Neco-ghc {{{

let g:necoghc_enable_detailed_browse = 1
let g:ycm_semantic_triggers = {'haskell' : ['.']}

" }}}

" Neomake {{{

autocmd! BufWritePost * Neomake
"let g:syntastic_error_symbol='✗'
"let g:syntastic_warning_symbol='⚠'
"let g:syntastic_style_error_symbol  = '⚡'
"let g:syntastic_style_warning_symbol  = '⚡'
"
"" Checkers
"let g:syntastic_tex_checkers=['chktex']

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

let g:airline_theme='base16'
let g:airline_powerline_fonts = 1

" }}}

" EasyMotion {{{ -------------------------------------------------------------

let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key
" binding.
" 's{char}{label}'
nmap s <Plug>(easymotion-s)
" or
" 's{char}{char}{label}'
" Need one more keystroke, but on average, it may be more comfortable.
"nmap s <Plug>(easymotion-s2)

" Turn on case sensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" }}}
"
" fzf {{{ -------------------------------------------------------------

nnoremap <C-P> :FZF<CR>

" }}}

" END PLUGINS SETUP }}}
