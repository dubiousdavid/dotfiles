" Turn off vi compatibility
set nocompatible
filetype off

call plug#begin()
" Plugins
Plug 'bling/vim-airline'
Plug 'EasyMotion'
Plug 'tir_black'
Plug 'airblade/vim-gitgutter'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-repeat'
Plug 'YankRing.vim'
Plug 'matchit.zip'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim', {'on': ['CtrlP','CtrlPClearCache','CtrlPBuffer']}
Plug 'mileszs/ack.vim', {'on': 'Ack'}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'mtth/scratch.vim', {'on': ['Scratch', 'ScratchInsert']}
Plug 'xolox/vim-misc', {'on': ['Note', 'NoteFromSelectedText', 'DeleteNote', 'SearchNotes']}
Plug 'xolox/vim-notes', {'on': ['Note', 'NoteFromSelectedText', 'DeleteNote', 'SearchNotes']}
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
filetype plugin indent on

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
" Turn syntax highlighting on
syntax on
" Color theme
colorscheme tir_black
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
" Enable edn filetype detection
autocmd BufNewFile,BufRead *.edn setlocal filetype=clojure
" HTML autocompletion
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
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
" Sexp
let g:sexp_filetypes=''
let g:sexp_insert_after_wrap=1

function! s:init_lisp()
  " vim-sexp
  xmap <silent><buffer> af              <Plug>(sexp_outer_list)
  omap <silent><buffer> af              <Plug>(sexp_outer_list)
  xmap <silent><buffer> if              <Plug>(sexp_inner_list)
  omap <silent><buffer> if              <Plug>(sexp_inner_list)
  xmap <silent><buffer> aF              <Plug>(sexp_outer_top_list)
  omap <silent><buffer> aF              <Plug>(sexp_outer_top_list)
  xmap <silent><buffer> iF              <Plug>(sexp_inner_top_list)
  omap <silent><buffer> iF              <Plug>(sexp_inner_top_list)
  xmap <silent><buffer> as              <Plug>(sexp_outer_string)
  omap <silent><buffer> as              <Plug>(sexp_outer_string)
  xmap <silent><buffer> is              <Plug>(sexp_inner_string)
  omap <silent><buffer> is              <Plug>(sexp_inner_string)
  xmap <silent><buffer> ae              <Plug>(sexp_outer_element)
  omap <silent><buffer> ae              <Plug>(sexp_outer_element)
  xmap <silent><buffer> ie              <Plug>(sexp_inner_element)
  omap <silent><buffer> ie              <Plug>(sexp_inner_element)
  nmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  xmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  omap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  nmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  xmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  omap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  nmap <silent><buffer> <Left>          <Plug>(sexp_move_to_prev_element_head)
  xmap <silent><buffer> <Left>          <Plug>(sexp_move_to_prev_element_head)
  omap <silent><buffer> <Left>          <Plug>(sexp_move_to_prev_element_head)
  nmap <silent><buffer> <Right>         <Plug>(sexp_move_to_next_element_head)
  xmap <silent><buffer> <Right>         <Plug>(sexp_move_to_next_element_head)
  omap <silent><buffer> <Right>         <Plug>(sexp_move_to_next_element_head)
  nmap <silent><buffer> <Up>            <Plug>(sexp_move_to_prev_element_tail)
  xmap <silent><buffer> <Up>            <Plug>(sexp_move_to_prev_element_tail)
  omap <silent><buffer> <Up>            <Plug>(sexp_move_to_prev_element_tail)
  nmap <silent><buffer> <Down>          <Plug>(sexp_move_to_next_element_tail)
  xmap <silent><buffer> <Down>          <Plug>(sexp_move_to_next_element_tail)
  omap <silent><buffer> <Down>          <Plug>(sexp_move_to_next_element_tail)
  nmap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  xmap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  omap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  nmap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)
  xmap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)
  omap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)
  nmap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  xmap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  omap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  nmap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  xmap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  omap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  nmap <silent><buffer> ==              <Plug>(sexp_indent)
  nmap <silent><buffer> =-              <Plug>(sexp_indent_top)
  nmap <silent><buffer> <LocalLeader>i  <Plug>(sexp_round_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>i  <Plug>(sexp_round_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>I  <Plug>(sexp_round_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>I  <Plug>(sexp_round_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>[  <Plug>(sexp_square_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>[  <Plug>(sexp_square_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>]  <Plug>(sexp_square_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>]  <Plug>(sexp_square_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>{  <Plug>(sexp_curly_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>{  <Plug>(sexp_curly_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>}  <Plug>(sexp_curly_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>}  <Plug>(sexp_curly_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>w  <Plug>(sexp_round_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>w  <Plug>(sexp_round_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>W  <Plug>(sexp_round_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>W  <Plug>(sexp_round_tail_wrap_element)
  nmap <silent><buffer> <LocalLeader>e[ <Plug>(sexp_square_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>e[ <Plug>(sexp_square_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>e] <Plug>(sexp_square_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>e] <Plug>(sexp_square_tail_wrap_element)
  nmap <silent><buffer> <LocalLeader>e{ <Plug>(sexp_curly_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>e{ <Plug>(sexp_curly_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>e} <Plug>(sexp_curly_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>e} <Plug>(sexp_curly_tail_wrap_element)
  nmap <silent><buffer> <LocalLeader>h  <Plug>(sexp_insert_at_list_head)
  nmap <silent><buffer> <LocalLeader>l  <Plug>(sexp_insert_at_list_tail)
  nmap <silent><buffer> <LocalLeader>@  <Plug>(sexp_splice_list)==
  nmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)==
  xmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)==
  nmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)==
  xmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)==
  nmap <silent><buffer> <M-k>           <Plug>(sexp_swap_list_backward)
  xmap <silent><buffer> <M-k>           <Plug>(sexp_swap_list_backward)
  nmap <silent><buffer> <M-j>           <Plug>(sexp_swap_list_forward)
  xmap <silent><buffer> <M-j>           <Plug>(sexp_swap_list_forward)
  nmap <silent><buffer> <LocalLeader>s  <Plug>(sexp_swap_element_backward)
  xmap <silent><buffer> <LocalLeader>s  <Plug>(sexp_swap_element_backward)
  nmap <silent><buffer> <LocalLeader>S  <Plug>(sexp_swap_element_forward)
  xmap <silent><buffer> <LocalLeader>S  <Plug>(sexp_swap_element_forward)
  nmap <silent><buffer> <LocalLeader><  <Plug>(sexp_emit_head_element)==
  xmap <silent><buffer> <LocalLeader><  <Plug>(sexp_emit_head_element)==
  nmap <silent><buffer> <LocalLeader>>  <Plug>(sexp_emit_tail_element)==
  xmap <silent><buffer> <LocalLeader>>  <Plug>(sexp_emit_tail_element)==
  nmap <silent><buffer> <LocalLeader>(  <Plug>(sexp_capture_prev_element)==
  xmap <silent><buffer> <LocalLeader>(  <Plug>(sexp_capture_prev_element)==
  nmap <silent><buffer> <LocalLeader>)  <Plug>(sexp_capture_next_element)==
  xmap <silent><buffer> <LocalLeader>)  <Plug>(sexp_capture_next_element)==
  imap <silent><buffer> <BS>            <Plug>(sexp_insert_backspace)
  imap <silent><buffer> "               <Plug>(sexp_insert_double_quote)
  imap <silent><buffer> (               <Plug>(sexp_insert_opening_round)
  imap <silent><buffer> )               <Plug>(sexp_insert_closing_round)
  imap <silent><buffer> [               <Plug>(sexp_insert_opening_square)
  imap <silent><buffer> ]               <Plug>(sexp_insert_closing_square)
  imap <silent><buffer> {               <Plug>(sexp_insert_opening_curly)
  imap <silent><buffer> }               <Plug>(sexp_insert_closing_curly)
endfunction

function s:init_clojure()
  " Fireplace
  nmap <Leader>r :Require<CR>
  " Clojure indentation
  setlocal lispwords=as->,binding,bound-fn,case,catch,cond->,cond->>,condp,def,definline,definterface,defmacro,defmethod,defmulti,defn,defn-,defonce,defprotocol,defrecord,defstruct,deftest,deftest-,deftype,doseq,dotimes,doto,extend,extend-protocol,extend-type,fn,for,if,if-let,if-not,if-some,let,letfn,locking,loop,ns,proxy,reify,set-test,testing,when,when-first,when-let,when-not,when-some,while,with-bindings,with-in-str,with-local-vars,with-open,with-precision,with-redefs,with-redefs-fn,with-test,some->,some->>,go-loop,facts,fact
  " Clojure custom keywords
  let g:clojure_syntax_keywords={'clojureDefine': ['defn$','defna','defnv'], 'clojureSpecial': ['fn$','fna','fnv']}
endfunction

" Initialize lisp languages
autocmd FileType clojure,scheme,lisp call s:init_lisp()
autocmd FileType clojure call s:init_clojure()
" Trailing whitespace
let g:strip_whitespace_on_save=1
" Ack
let g:ack_default_options=' -s -H --no-color --smart-case --nogroup --column --literal'
nmap <Leader>a :Ack<Space>
" Ctrlp
let g:ctrlp_root_markers=['project.clj']
nmap <Leader>f :CtrlP<CR>
nmap <Leader>cf :CtrlPClearCache<CR>
nmap <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_open_new_file='r'
let g:ctrlp_open_multiple_files='i'
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
" Notes
let g:notes_directories=['~/Dropbox/Notes']
" Scratch
let g:scratch_insert_autohide=0
nmap gs :ScratchInsert<CR>
