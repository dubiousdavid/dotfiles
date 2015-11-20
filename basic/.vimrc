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
" Use OS clipboard
set clipboard=unnamed
" Turn syntax highlighting on
syntax on
" Enable line numbers
set number
" Searching
set incsearch
set ic
" Enable mouse in all modes
set mouse=a
" Color theme
colorscheme tir_black
" <Leader>
let mapleader=','
let maplocalleader=','
" Backspace closes buffer
nmap <BS> :bd<CR>
" Buffer previous
nmap <C-j> :bp!<CR>
" Buffer next
nmap <C-k> :bn!<CR>
" Last buffer
nmap <C-l> :e#<CR>
" Visual mode delimiter insertion
vmap s( "zdi(<C-R>z)<Esc>
vmap s[ "zdi[<C-R>z]<Esc>
vmap s{ "zdi{<C-R>z}<Esc>
