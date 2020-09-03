" Load plugins
call plug#begin()
" Statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Opening files and buffers
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
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
" " Motions for adding/removing comments
Plug 'tomtom/tcomment_vim'
" Auto-close parens
Plug 'jiangmiao/auto-pairs'
" " Duplicate lines and visual selection
Plug 'timkendrick/vim-duplicate'
" " Motions for function arguments
Plug 'b4winckler/vim-angry'
" Open file at last edit position
Plug 'dietsche/vim-lastplace'
" Recent files (overrides start screen)
Plug 'mhinz/vim-startify'
" File commands
Plug 'tpope/vim-eunuch'
" Flow
Plug 'flowtype/vim-flow', {'for': 'javascript'}
" Extend % matching
Plug 'vim-scripts/matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
" JSX
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
" Clojure
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
" Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Prettier
Plug 'prettier/vim-prettier', {
  \ 'do': 'npm install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql'] }
" Linting
Plug 'dense-analysis/ale', {'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql']}
call plug#end()
" Color theme
colorscheme tir_black
" Don't show mode
set noshowmode
" set termguicolors
" Allow navigation away from modified buffers
set hidden
" Disable tabline
set showtabline=0
" Turn off beeping
set visualbell
" Highlight current line
set cursorline
" " Set the terminal's title
" set title
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
set wildignore+=node_modules/*
" Enable mouse in all modes
set mouse=a
" " Omnicomplete
" " set completeopt=longest,menuone,preview
" " IMPORTANT: :help Ncm2PopupOpen for more information
" set completeopt=noinsert,menuone,noselect
" set omnifunc=syntaxcomplete#Complete
" List
set listchars+=space:.
" Change default split behavior
set splitbelow
set splitright
" <Leader>
let mapleader=','
let maplocalleader=','
" Enable edn filetype detection
au BufNewFile,BufRead *.edn setlocal filetype=clojure
" Enable .babelrc filetype detection
au BufNewFile,BufRead .babelrc setlocal filetype=json
" " HTML autocompletion
" au FileType html set omnifunc=htmlcomplete#CompleteTags
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
" " Horizontal split
" nmap _ :sp<CR>
" Backspace closes buffer
nmap <BS> :bd<CR>
" Last buffer
map <C-l> :e#<CR>
" Move lines up/down
nmap <C-j> :m +1<CR>
nmap <C-k> :m -2<CR>
vmap <C-j> :m '>+1<CR> gv
vmap <C-k> :m '<-2<CR> gv
" Duplicate line/selection
nmap <Leader>d <Plug>Duplicate
vmap <Leader>d <Plug>Duplicate gv
" Emacs keybindings (insert mode)
imap <C-f> <Right>
imap <C-b> <Left>
" " Stay in visual mode when indenting
" vnoremap < <gv
" vnoremap > >gv
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
" FZF
nmap <Leader>f :Files<CR>
nmap <Leader>b :Buffers<CR>
nmap <Leader>h :History/<CR>
" Search
function! s:Search(term)
  execute 'terminal rg -M 125 -p -S --no-ignore-vcs --hidden ' . a:term
endfunction
command! -nargs=1 Search call s:Search(<f-args>)
nmap <Leader>s :Search<Space>
" Set grep program to rg
set grepprg=rg\ --vimgrep
set grepformat^=%f:%l:%c:%m
" Git
nmap <Leader>gh :Gbrowse<CR>
vmap <Leader>gh :Gbrowse<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :terminal git diff<CR>
" nmap <Leader>gl :terminal git tree<CR>
nmap <Leader>gl :BCommits!<CR>
nmap <Leader>gg :Ggrep<Space>
nmap <Leader>ge :Gedit<Space>
nmap <Leader>gf :!git fetch<CR>
nmap <Leader>gP :!git pull<CR>
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
" Javascript
au FileType javascript nmap <buffer> <silent> <Leader>j :FlowJumpToDef<CR>
au FileType javascript nmap <buffer> <silent> <Leader>t :FlowType<CR>
" Startify (start screen)
nmap <Leader>r :Startify<CR>
let g:startify_list_order = ['dir']
let g:startify_change_to_dir = 0
let g:startify_custom_header = []
" Wrap lines in Quickfix
" augroup quickfix
"   autocmd!
"   autocmd FileType qf setlocal wrap
" augroup END
" Prettier
let g:prettier#autoformat = 0
" autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql Prettier
" max line length that prettier will wrap on
let g:prettier#config#print_width = 80
" number of spaces per indentation level
let g:prettier#config#tab_width = 2
" print semicolons
let g:prettier#config#semi = 'false'
" single quotes over double quotes
let g:prettier#config#single_quote = 'true'
" print spaces between brackets
let g:prettier#config#bracket_spacing = 'true'
" put > on the last line instead of new line
let g:prettier#config#jsx_bracket_same_line = 'false'
" none|es5|all
let g:prettier#config#trailing_comma = 'all'
" flow|babylon|typescript|postcss|json|graphql
let g:prettier#config#parser = 'babylon'
" cli-override|file-override|prefer-file
let g:prettier#config#config_precedence = 'prefer-file'
" Flow
let g:flow#autoclose=1
" MJML
autocmd BufEnter *.mjml setlocal filetype=xml
au FileType clojure nmap <buffer> <silent> <Leader>j <Plug>FireplaceDjump
" Yank history
nnoremap <silent> <Leader>y :<C-u>CocList -A --normal yank<cr>
" Conjure
" let g:coc_global_extensions = ['coc-conjure']
" COC
" " nmap <silent> <C-p> <Plug>(coc-diagnostic-prev)
" " nmap <silent> <C-n> <Plug>(coc-diagnostic-next)
" Ale
nmap <silent> <C-p> <Plug>(ale_previous_wrap)
nmap <silent> <C-n> <Plug>(ale_next_wrap)
