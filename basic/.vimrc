" Turn off vi compatibility
set nocompatible
filetype off

call plug#begin()
" Plugins
Plug 'bling/vim-airline'
Plug 'EasyMotion'
Plug 'ctrlpvim/ctrlp.vim', {'on': ['CtrlP','CtrlPClearCache','CtrlPBuffer']}
Plug 'mtth/scratch.vim', {'on': ['Scratch', 'ScratchInsert']}
call plug#end()

filetype plugin indent on
" Turn syntax highlighting on
syntax on
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
" Enable line numbers
set number
" Searching
set incsearch
set ic
" Enable mouse in all modes
set mouse=a
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
" vim-airline
set laststatus=2
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
" Ctrlp
let g:ctrlp_root_markers=['project.clj']
nmap <Leader>f :CtrlP<CR>
nmap <Leader>cf :CtrlPClearCache<CR>
nmap <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_open_new_file='r'
let g:ctrlp_open_multiple_files='i'
" Easy Motion
let g:EasyMotion_leader_key='m'
" Scratch
let g:scratch_insert_autohide=0
nmap gs :ScratchInsert<CR>
