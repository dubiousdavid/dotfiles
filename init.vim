" Enable file type, plugin, and indent
filetype plugin indent on
" Load plugins
call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'EasyMotion'
Plug 'airblade/vim-gitgutter'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-fugitive'
" Plug 'gregsexton/gitv'
Plug 'tpope/vim-repeat'
Plug 'YankRing.vim'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'luochen1990/rainbow'
Plug 'cohama/lexima.vim'
Plug 'timkendrick/vim-duplicate'
Plug 'haya14busa/incsearch.vim'
Plug 'osyo-manga/vim-over'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
Plug 'lambdalisue/vim-pager', {'on': 'PAGER'}
Plug 'powerman/vim-plugin-AnsiEsc', {'on': 'PAGER'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'ensime/ensime-vim', {'for': 'scala'}
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
Plug 'ElmCast/elm-vim', {'for': 'elm'}
call plug#end()
" Color theme
colorscheme tir_black
" Allow navigation away from modified buffers
set hidden
" Turn off beeping
set visualbell
" Highlight current line
set cursorline
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
let g:airline_theme='murmur'
" Backspace closes buffer
nmap <BS> :bd<CR>
" Buffer previous
nmap <M-b> :bp!<CR>
" Buffer next
nmap <M-f> :bn!<CR>
" Last buffer
nmap <C-l> :e#<CR>
" Quickfix
nmap <M-n> :cnext<CR>
nmap <M-p> :cprev<CR>
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
imap <M-BS> <C-o>db
" Strip trailing whitespace
let g:strip_whitespace_on_save=1
" Don't highlight trailing whitespace
let g:better_whitespace_enabled=0
" YankRing
nmap <Leader>y :YRShow<CR>
function! YRRunAfterMaps()
  nnoremap Y :<C-U>YRYankCount 'y$'<CR>
endfunction
" Easy Motion
let g:EasyMotion_leader_key=',m'
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
" Clojure
let g:clojure_syntax_keywords={'clojureDefine': ['defn$','defna','defnv'], 'clojureSpecial': ['fn$','fna','fnv']}
" Fugitive
nmap <Leader>gh :Gbrowse<CR>
vmap <Leader>gh :Gbrowse<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :terminal git diff<CR>
nmap <Leader>gl :Glog<Space>
nmap <Leader>gg :Ggrep<Space>
nmap <Leader>ge :Gedit<Space>
nmap <Leader>gp :Gpush<CR>
" Open lines without changing to Insert mode
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>
" netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 20
" Incremental searching
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
" Incremental replacing
nmap <Leader>s :OverCommandLine<CR>%s/
vmap <Leader>s :OverCommandLine<CR>s/
" Display full path of current file
nmap <Leader>n :echo expand('%:p')<CR>
" Ensime
nmap <Leader>ej :EnDeclaration<CR>
nmap <Leader>et :EnType<CR>
