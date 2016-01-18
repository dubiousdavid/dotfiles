" Turn off vi compatibility
set nocompatible
" Enable file type, plugin, and indent
filetype plugin indent on

" Load plugins
call plug#begin()
Plug 'bling/vim-airline'
Plug 'EasyMotion'
Plug 'tir_black'
Plug 'airblade/vim-gitgutter'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-repeat'
Plug 'YankRing.vim'
Plug 'matchit.zip'
Plug 'tpope/vim-surround'
Plug 'drmingdrmer/xptemplate'
Plug 'lambdalisue/vim-manpager', {'on': 'MANPAGER'}
Plug 'lambdalisue/vim-pager', {'on': 'PAGER'}
Plug 'powerman/vim-plugin-AnsiEsc', {'on': 'PAGER'}
Plug 'ensime/ensime-vim', {'for': 'scala'}
Plug 'derekwyatt/vim-scala', {'for': 'scala'}
Plug 'ctrlpvim/ctrlp.vim', {'on': ['CtrlP', 'CtrlPClearCache', 'CtrlPBuffer']}
Plug 'mileszs/ack.vim', {'on': 'Ack'}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'mtth/scratch.vim', {'on': ['Scratch', 'ScratchInsert']}
Plug 'smerrill/vcl-vim-plugin', {'for': 'vcl'}
Plug 'othree/html5.vim', {'for': 'html,xml'}
Plug 'guns/vim-sexp', {'for': 'lisp,scheme,clojure'}
Plug 'guns/vim-clojure-static', {'for': 'clojure'}
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
Plug 'scrooloose/syntastic', {'for': 'fsharp'}
Plug 'ervandew/supertab', {'for': 'fsharp'}
Plug 'fsharp/vim-fsharp', {'for': 'fsharp', 'do': 'make fsautocomplete'}
Plug 'lambdatoast/elm.vim', {'for': 'elm'}
call plug#end()

" Color theme
colorscheme tir_black
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
set ic
set nohls
" Enable mouse in all modes
set mouse=a
" Omnicomplete
set completeopt=longest,menuone
" <Leader>
let mapleader=','
let maplocalleader=','
" Enable edn filetype detection
autocmd BufNewFile,BufRead *.edn setlocal filetype=clojure
" HTML autocompletion
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
" vim-airline
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_tab_nr=0
let g:airline#extensions#tabline#show_close_button=0
let g:airline#extensions#tabline#buffer_idx_mode=1
let g:airline#extensions#tabline#show_tab_type=0
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
nmap <C-j> :bp!<CR>
" Buffer next
nmap <C-k> :bn!<CR>
" Last buffer
nmap <C-l> :e#<CR>
" Trailing whitespace
let g:strip_whitespace_on_save=1
" Ack
let g:ack_default_options=' -s -H --no-color --smart-case --nogroup --column --literal'
nmap <Leader>a :Ack<Space>
" Ctrlp
let g:ctrlp_root_markers=['project.clj', 'build.sbt']
nmap <Leader>f :CtrlP<CR>
nmap <Leader>cf :CtrlPClearCache<CR>
" nmap <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_open_new_file='r'
let g:ctrlp_open_multiple_files='i'
let g:ctrlp_custom_ignore='\v[\/](target)$'
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
" Ensime
let EnErrorStyle='Underlined'
autocmd FileType scala nmap <Leader>t :EnType<CR>
autocmd FileType scala nmap <Leader>d :EnDocUri<CR>
autocmd FileType scala nmap <Leader>b :EnDocBrowse<CR>
autocmd FileType scala nmap <Leader>g :EnDeclaration<CR>
autocmd FileType scala ca format EnFormatSource
