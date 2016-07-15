" Enable file type, plugin, and indent
filetype plugin indent on
" Load plugins
call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'EasyMotion'
Plug 'airblade/vim-gitgutter'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'YankRing.vim'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'osyo-manga/vim-anzu'
Plug 'luochen1990/rainbow'
Plug 'cohama/lexima.vim'
Plug 'timkendrick/vim-duplicate'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
Plug 'lambdalisue/vim-manpager', {'on': 'MANPAGER'}
Plug 'lambdalisue/vim-pager', {'on': 'PAGER'}
Plug 'powerman/vim-plugin-AnsiEsc', {'on': 'PAGER'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'derekwyatt/vim-scala', {'for': ['scala', 'sbt.scala']}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'mtth/scratch.vim', {'on': ['Scratch', 'ScratchInsert']}
Plug 'fatih/vim-go', {'tag': 'v1.6', 'for': 'go'}
Plug 'smerrill/vcl-vim-plugin', {'for': 'vcl'}
Plug 'cespare/vim-toml', {'for': 'toml'}
Plug 'othree/html5.vim', {'for': ['html', 'xml']}
Plug 'guns/vim-clojure-static', {'for': 'clojure'}
Plug 'scrooloose/syntastic', {'for': 'fsharp'}
Plug 'ervandew/supertab', {'for': 'fsharp'}
Plug 'fsharp/vim-fsharp', {'for': 'fsharp', 'do': 'make fsautocomplete'}
call plug#end()
" Color theme
colorscheme tir_black
" Allow navigation away from modified buffers
set hidden
" Turn off beeping
set visualbell
" Highlight current line
set cursorline
" Show 3 lines of context around the cursor
set scrolloff=3
" Set the terminal's title
set title
" Tabs and spaces
set shiftwidth=2
set tabstop=2
set expandtab
set smarttab
" Allow backspace in insert mode
set backspace=indent,eol,start
" Use OS clipboard
set clipboard=unnamed
" Use undo file to maintain undo state per buffer
set undofile
" Enable line numbers
set number
" Searching
set incsearch
set ignorecase smartcase
set nohls
" Better? completion on command line
set wildmenu
" Enable mouse in all modes
set mouse=a
" Omnicomplete
set completeopt=longest,menuone
" <Leader>
let mapleader=','
" let maplocalleader=','
" Enable edn filetype detection
autocmd BufNewFile,BufRead *.edn setlocal filetype=clojure
" Enable sc filetype detection
autocmd BufNewFile,BufRead *.sc setlocal filetype=scala
" HTML autocompletion
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
" Terminal
tnoremap <Esc> <C-\><C-n>
" vim-airline
set laststatus=2
let g:airline_powerline_fonts=0
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_tab_nr=0
let g:airline#extensions#tabline#show_close_button=0
let g:airline#extensions#tabline#buffer_idx_mode=1
let g:airline#extensions#tabline#show_tab_type=0
let g:airline#extensions#tabline#fnamemod=':t'
nmap <Leader>1 <Plug>AirlineSelectTab1
nmap <Leader>2 <Plug>AirlineSelectTab2
nmap <Leader>3 <Plug>AirlineSelectTab3
nmap <Leader>4 <Plug>AirlineSelectTab4
nmap <Leader>5 <Plug>AirlineSelectTab5
nmap <Leader>6 <Plug>AirlineSelectTab6
nmap <Leader>7 <Plug>AirlineSelectTab7
nmap <Leader>8 <Plug>AirlineSelectTab8
nmap <Leader>9 <Plug>AirlineSelectTab9
" Backspace closes buffer
nmap <BS> :bd<CR>
" Buffer previous
nmap <M-b> :bp!<CR>
" Buffer next
nmap <M-f> :bn!<CR>
" Last buffer
nmap <C-l> :e#<CR>
" Move lines up/down
nmap <C-j> :m +1<CR>
nmap <C-k> :m -2<CR>
vmap <C-j> :m '>+1<CR> gv
vmap <C-k> :m '<-2<CR> gv
" Duplicate line/selection
nmap <Leader>d <Plug>Duplicate
vmap <Leader>d <Plug>Duplicate gv
" Emacs-like beginning and end of line
imap <C-e> <C-o>$
imap <C-a> <C-o>^
" Trailing whitespace
let g:strip_whitespace_on_save=1
" YankRing
nmap <Leader>y :YRShow<CR>
function! YRRunAfterMaps()
  nnoremap Y :<C-U>YRYankCount 'y$'<CR>
endfunction
" Easy Motion
let g:EasyMotion_leader_key='m'
" Undo-tree
nmap <Leader>u :UndotreeToggle<CR>
let g:undotree_SetFocusWhenToggle=1
let g:undotree_DiffAutoOpen=1
" Scratch
let g:scratch_insert_autohide=0
nmap gs :ScratchInsert<CR>
" Rainbow
let g:rainbow_active=1
let g:rainbow_conf={
\	'ctermfgs': ['darkgray', 'gray'],
\	'operators': '_,_',
\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\	'separately': {
\		'*': {},
\		'vim': {
\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
\		},
\		'css': 0,
\	}
\}
" FZF
nmap <Leader>f :Files<CR>
nmap <Leader>c :Commands<CR>
nmap <Leader>a :Ag<Space>
nmap <Leader>b :Buffers<CR>
nmap <Leader>hf :History<CR>
nmap <Leader>hc :History:<CR>
nmap <Leader>hs :History/<CR>
" Anzu
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)
let g:anzu_status_format='%p (%i/%l)'
" Clojure
let g:clojure_syntax_keywords={'clojureDefine': ['defn$','defna','defnv'], 'clojureSpecial': ['fn$','fna','fnv']}
" Fugitive
nmap <Leader>gh :Gbrowse<CR>
" Open lines without changing to Insert mode
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>
