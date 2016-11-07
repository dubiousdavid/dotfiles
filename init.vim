" Enable file type, plugin, and indent
filetype plugin indent on
" Load plugins
call plug#begin()
" Statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Opening files and buffers
Plug 'ctrlpvim/ctrlp.vim'
" Word and character motions
Plug 'easymotion/vim-easymotion'
" Show git changes in gutter
Plug 'airblade/vim-gitgutter'
" Remove trailing whitespace
Plug 'ntpeters/vim-better-whitespace'
" Git/Github integration
Plug 'tpope/vim-fugitive'
" Extend . command
Plug 'tpope/vim-repeat'
" Display yank ring
Plug 'vim-scripts/YankRing.vim'
" Surround text with delimeters
Plug 'tpope/vim-surround'
" Motions for adding/removing comments
Plug 'tomtom/tcomment_vim'
" Rainbow delimiters
Plug 'luochen1990/rainbow'
" Auto-close parens
Plug 'cohama/lexima.vim'
" Duplicate lines and visual selection
Plug 'timkendrick/vim-duplicate'
" Display search count
Plug 'henrik/vim-indexed-search'
" Motions for function arguments
Plug 'vim-scripts/argtextobj.vim'
" Auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Auto completion for Go
Plug 'nsf/gocode', {'for': 'go', 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh'}
" Go integration for deoplete
Plug 'zchee/deoplete-go', {'for': 'go', 'do': 'make'}
" Go syntax highlighting, jump-to-definition, gofmt
Plug 'fatih/vim-go', {'tag': 'v1.6', 'for': 'go'}
" Extend % matching
Plug 'matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
" Use vim as a pager
Plug 'lambdalisue/vim-pager', {'on': 'PAGER'}
Plug 'powerman/vim-plugin-AnsiEsc', {'on': 'PAGER'}
" Rust
Plug 'rust-lang/rust.vim', {'for': 'rust'}
" Scala
Plug 'ensime/ensime-vim', {'for': 'scala'}
Plug 'derekwyatt/vim-scala', {'for': ['scala', 'sbt.scala']}
" Scratch buffer
Plug 'mtth/scratch.vim', {'on': ['Scratch', 'ScratchInsert']}
" VCL
Plug 'smerrill/vcl-vim-plugin', {'for': 'vcl'}
" TOML
Plug 'cespare/vim-toml', {'for': 'toml'}
" HTML5
Plug 'othree/html5.vim', {'for': ['html', 'xml']}
" Clojure
Plug 'guns/vim-clojure-static', {'for': 'clojure'}
" F#
Plug 'scrooloose/syntastic', {'for': 'fsharp'}
Plug 'ervandew/supertab', {'for': 'fsharp'}
Plug 'fsharp/vim-fsharp', {'for': 'fsharp', 'do': 'make fsautocomplete'}
" Elm
Plug 'ElmCast/elm-vim', {'for': 'elm'}
" Javascript (ES6)
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
call plug#end()
" Color theme
colorscheme tir_black
" Don't show mode below statusline
set noshowmode
" Allow navigation away from modified buffers
set hidden
" Disable tabline
set showtabline=0
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
set omnifunc=syntaxcomplete#Complete
" List
set listchars+=space:.
" Change default split behavior
set splitbelow
set splitright
" <Leader>
let mapleader=','
" let maplocalleader=','
" Enable edn filetype detection
au BufNewFile,BufRead *.edn setlocal filetype=clojure
" Enable sc filetype detection
au BufNewFile,BufRead *.sc setlocal filetype=scala
" HTML autocompletion
au FileType html set omnifunc=htmlcomplete#CompleteTags
" Terminal
tnoremap <Esc> <C-\><C-n>
tnoremap <A-2> <C-\><C-n><C-w>j
tnoremap <A-1> <C-\><C-n><C-w>k
nnoremap <A-2> <C-w>j
nnoremap <A-1> <C-w>k
autocmd BufWinEnter,WinEnter term://* startinsert
" vim-airline
set laststatus=2
let g:airline_theme='murmur'
" Horizontal split
nmap _ :sp<CR>
" Backspace closes buffer
nmap <BS> :bd<CR>
" Buffer previous
map <M-b> :bp!<CR>
" Buffer next
map <M-f> :bn!<CR>
" Last buffer
map <C-l> :e#<CR>
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
" Emacs keybindings (insert mode)
imap <C-e> <C-o>$
imap <C-a> <C-o>^
imap <C-f> <Right>
imap <C-b> <Left>
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
map <Leader>s <Plug>(easymotion-s)
map <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
map <Leader>W <Plug>(easymotion-bd-W)
" Scratch
let g:scratch_insert_autohide=0
nmap gs :Scratch<CR>
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
" CtrlP
nmap <Leader>f :CtrlP<CR>
nmap <Leader>b :CtrlPBuffer<CR>
nmap <Leader>m :CtrlPMRUFiles<CR>
let g:ctrlp_user_command="ag -f -U --hidden -g '' %s"
let g:ctrlp_open_new_file='r'
let g:ctrlp_open_multiple_files='i'
" Clojure
let g:clojure_syntax_keywords={'clojureDefine': ['defn$','defna','defnv'], 'clojureSpecial': ['fn$','fna','fnv']}
" Ag
function! s:Ag(term)
  execute 'te ag --pager=less ' . a:term
endfunction
command! -nargs=1 Ag call s:Ag(<f-args>)
nmap <Leader>a :Ag<Space>
" Fugitive
nmap <Leader>gh :Gbrowse<CR>
vmap <Leader>gh :Gbrowse<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :terminal git diff<CR>
nmap <Leader>gl :terminal git tree<CR>
nmap <Leader>gg :Ggrep<Space>
nmap <Leader>ge :Gedit<Space>
nmap <Leader>gp :! git push<CR>
" Open lines without changing to Insert mode
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>
" netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 20
" Display full path of current file
nmap <Leader>n :echo expand('%:p')<CR>
" Ensime
au FileType scala nmap <Leader>j :EnDeclaration<CR>
au FileType scala nmap <Leader>t :EnType<CR>
au FileType scala nmap <Leader>d :EnDocBrowse<CR>
au FileType scala nmap <Leader>c :EnTypeCheck<CR>
au FileType scala nmap <Leader>r :EnRename<CR>
" Deoplete
let g:deoplete#enable_at_startup=1
let g:deoplete#sources = {}
let g:deoplete#sources.scala = ['buffer', 'tags', 'omni']
let g:deoplete#omni#input_patterns = {}
let g:deoplete#omni#input_patterns.scala = ['[^. *\t0-9]\.\w*',': [A-Z]\w', '[\[\t\( ][A-Za-z]\w*']
" Go
au FileType go nmap <Leader>j <Plug>(go-def)
au FileType go nmap <leader>t <Plug>(go-info)
au FileType go nmap <Leader>d <Plug>(go-doc)
au FileType go nmap <leader>r <Plug>(go-rename)
