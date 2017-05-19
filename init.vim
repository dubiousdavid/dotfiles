" Enable file type, plugin, and indent
filetype plugin indent on
" Load plugins
call plug#begin()
" Statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Opening files and buffers
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Word and character motions
Plug 'easymotion/vim-easymotion'
" Show git changes in gutter
Plug 'airblade/vim-gitgutter'
" Remove trailing whitespace
Plug 'ntpeters/vim-better-whitespace'
" Git integration
Plug 'tpope/vim-fugitive'
" Github integration
Plug 'tpope/vim-rhubarb'
" Extend . command
Plug 'tpope/vim-repeat'
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
" Plug 'henrik/vim-indexed-search'
" Motions for function arguments
Plug 'b4winckler/vim-angry'
" Sort motion
Plug 'christoomey/vim-sort-motion'
" Open file at last edit position
Plug 'dietsche/vim-lastplace'
" Recent files (overrides start screen)
Plug 'mhinz/vim-startify'
" File commands
Plug 'tpope/vim-eunuch'
" Ack, find and replace
Plug 'wincent/ferret'
" Scratch
Plug 'mtth/scratch.vim'
" Formatter
Plug 'sbdchd/neoformat'
" Auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Angular/Typescript
" Plug 'Quramy/vim-js-pretty-template', {'for': 'typescript'}
" Flow
" Plug 'flowtype/vim-flow'
" Auto completion for Go
Plug 'nsf/gocode', {'for': 'go', 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh'}
" Go integration for deoplete
Plug 'zchee/deoplete-go', {'for': 'go', 'do': 'make'}
" Go syntax highlighting, jump-to-definition, gofmt
Plug 'fatih/vim-go', {'tag': 'v1.6', 'for': 'go'}
" Extend % matching
Plug 'matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
" Rust
Plug 'rust-lang/rust.vim', {'for': 'rust'}
" Scala
Plug 'ensime/ensime-vim', {'for': 'scala', 'commit': '1b32e85f5c1883af3946cf2bf7a39f60801f47b1'}
Plug 'derekwyatt/vim-scala', {'for': ['scala', 'sbt.scala']}
" VCL
Plug 'smerrill/vcl-vim-plugin', {'for': 'vcl'}
" TOML
Plug 'cespare/vim-toml', {'for': 'toml'}
" HTML5
Plug 'othree/html5.vim', {'for': ['html', 'xml']}
" Keybindings for closing tags
Plug 'tpope/vim-ragtag', {'for': ['javascript', 'html', 'xml']}
" Format Javascript
Plug 'maksimr/vim-jsbeautify', {'for': ['html', 'xml', 'css', 'json']}
" Javascript (ES6)
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
" JSX
Plug 'mxw/vim-jsx', {'for': 'javascript'}
" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
" Clojure
Plug 'guns/vim-clojure-static', {'for': 'clojure'}
" F#
Plug 'scrooloose/syntastic', {'for': 'fsharp'}
Plug 'ervandew/supertab', {'for': 'fsharp'}
Plug 'fsharp/vim-fsharp', {'for': 'fsharp', 'do': 'make fsautocomplete'}
" Elm
Plug 'ElmCast/elm-vim', {'for': 'elm'}
" Typescript
Plug 'leafgarland/typescript-vim', {'for': 'typescript'}
Plug 'Quramy/tsuquyomi', {'for': 'typescript', 'do': 'make -f make_mac.mak'}
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
" Plug 'mhartington/nvim-typescript', {'for': 'typescript'}
call plug#end()
" Color theme
colorscheme tir_black
" Show replacement inline (kind of broken)
" set inccommand=nosplit
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
set softtabstop=2
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
" Don't offer to open certain files/directories
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico
set wildignore+=*.pdf,*.psd
set wildignore+=node_modules/*,bower_components/*
" Enable mouse in all modes
set mouse=a
" Omnicomplete
set completeopt=longest,menuone,preview
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
" Enable .babelrc filetype detection
au BufNewFile,BufRead .babelrc setlocal filetype=json
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
let g:airline_left_sep=''
let g:airline_right_sep=''
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.readonly='RO'
let g:airline_symbols.linenr = ''
let g:airline_symbols.maxlinenr = ''
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
" Stay in visual mode when indenting
vnoremap < <gv
vnoremap > >gv
" Override Y
nnoremap Y y$
" Entire file
onoremap af :<C-u>normal! ggVG<CR>
" Strip trailing whitespace
let g:strip_whitespace_on_save=1
" Don't highlight trailing whitespace
let g:better_whitespace_enabled=0
" Easy Motion
map <Leader>c <Plug>(easymotion-s)
map <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
map <Leader>W <Plug>(easymotion-bd-W)
" Rainbow
let g:rainbow_active=1
let g:rainbow_conf={'ctermfgs': ['darkgray', 'gray'], 'separately': {'html': {}}}
au FileType sh call rainbow#clear()
" FZF
nmap <Leader>f :Files<CR>
nmap <Leader>b :Buffers<CR>
" Clojure
let g:clojure_syntax_keywords={'clojureDefine': ['defn$','defna','defnv'], 'clojureSpecial': ['fn$','fna','fnv']}
" Ag
function! s:Ag(term)
  execute 'te ag --pager=less ' . a:term
endfunction
command! -nargs=1 Ag call s:Ag(<f-args>)
nmap <Leader>a :Ag<Space>
" Git
nmap <Leader>gh :Gbrowse<CR>
vmap <Leader>gh :Gbrowse<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :terminal git diff<CR>
nmap <Leader>gl :terminal git tree<CR>
nmap <Leader>gg :Ggrep<Space>
nmap <Leader>ge :Gedit<Space>
nmap <Leader>gf :!git fetch<CR>
nmap <Leader>gF :!git pull<CR>
nmap <Leader>gp :!git push<CR>
nmap <Leader>gup :!git push -u origin $(git rev-parse --abbrev-ref HEAD)<CR>
nmap <Leader>gn :GitGutterNextHunk<CR>
" Open lines without changing to Insert mode
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>
" netrw
let g:netrw_liststyle = 3
" Display full path of current file
nmap <Leader>n :echo expand('%:p')<CR>
" Ensime
au FileType scala nmap <buffer> <silent> <Leader>j :EnDeclaration<CR>
au FileType scala nmap <buffer> <silent> <Leader>t :EnType<CR>
au FileType scala nmap <buffer> <silent> <Leader>T :EnTypeCheck<CR>
au FileType scala nmap <buffer> <silent> <Leader>i :EnInspectType<CR>
au FileType scala nmap <buffer> <silent> <Leader>l :EnDocBrowse<CR>
au FileType scala nmap <buffer> <silent> <Leader>x :EnRename<CR>
" au BufWritePost *.scala silent :EnTypeCheck
" Deoplete
let g:deoplete#enable_at_startup=1
let g:deoplete#sources = {}
" let g:deoplete#sources.scala = ['buffer', 'tags', 'omni']
let g:deoplete#sources.scala = ['tags', 'omni']
let g:deoplete#omni#input_patterns = {}
" let g:deoplete#omni#input_patterns.scala = ['[^. *\t0-9]\.\w*',': [A-Z]\w', '[\[\t\( ][A-Za-z]\w*']
" Go
au FileType go nmap <buffer> <silent> <Leader>j <Plug>(go-def)
au FileType go nmap <buffer> <silent> <Leader>t <Plug>(go-info)
au FileType go nmap <buffer> <silent> <Leader>l <Plug>(go-doc)
au FileType go nmap <buffer> <silent> <Leader>x <Plug>(go-rename)
" Typescript
au FileType typescript nmap <buffer> <silent> <Leader>j :TsuDefinition<CR>
au FileType typescript nmap <buffer> <silent> <Leader>i :TsuImport<CR>
au FileType typescript nmap <buffer> <silent> <Leader>t : <C-u>echo tsuquyomi#hint()<CR>
au FileType typescript nmap <buffer> <silent> <Leader>x <Plug>(TsuquyomiRenameSymbol)
let g:tsuquyomi_completion_detail = 1
" XML
au FileType xml nmap <buffer> <silent> <Leader>xc :!xmllint --noout %<CR>
" Save window position when changing buffers
au BufLeave * let b:winview = winsaveview()
au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
" Format
autocmd FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType xml noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>
" Sort (case insensitive)
let g:sort_motion_flags = "i"
" JSX
let g:jsx_ext_required = 0
" Startify
nmap <Leader>r :Startify<CR>
let g:startify_list_order = ['dir', 'files', 'bookmarks', 'sessions', 'commands']
let g:startify_change_to_dir = 0
let g:startify_custom_header = []
" Make directory
command MakeDirectory :silent !mkdir -p %:h
" Snippets
let g:UltiSnipsJumpBackwardTrigger="<C-z>"
let g:UltiSnipsJumpForwardTrigger="<C-q>"
" Wrap lines in Quickfix
augroup quickfix
  autocmd!
  autocmd FileType qf setlocal wrap
augroup END
" Scratch
let g:scratch_no_mappings = 1
nmap <leader>si <plug>(scratch-insert-reuse)
xmap <leader>si <plug>(scratch-selection-reuse)
nmap <leader>sp :ScratchPreview<CR>
nmap <leader>se :Scratch<CR>
" Angular
" autocmd FileType typescript JsPreTmpl html
" autocmd FileType typescript syn clear foldBraces
" Javascript
let g:javascript_plugin_jsdoc=1
autocmd FileType javascript.jsx,javascript setlocal formatprg=prettier\ --stdin\ --print-width\ 100\ --single-quote
autocmd BufWritePre *.js Neoformat
let g:neoformat_javascript_prettier = {
  \ 'exe': 'prettier', 'args': ['--stdin', '--single-quote', '--print-width 100'], 'stdin': 1 }
