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
" Duplicate lines and visual selection
Plug 'timkendrick/vim-duplicate'
" Motions for function arguments
Plug 'b4winckler/vim-angry'
" Open file at last edit position
Plug 'farmergreg/vim-lastplace'
" Recent files (overrides start screen)
Plug 'mhinz/vim-startify'
" File commands
Plug 'tpope/vim-eunuch'
" Extend % matching
Plug 'vim-scripts/matchit.zip', {'for': ['html', 'xml', 'sh', 'vim']}
" JSX
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
" Clojure
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
" Clojure formatting
Plug 'venantius/vim-cljfmt', {'for': 'clojure'}
" Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Prettier
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
" Codeowners
Plug 'rhysd/vim-syntax-codeowners'
" Handlebars/Mustache templates
Plug 'mustache/vim-mustache-handlebars', {'for': 'hbs'}
call plug#end()
" Color theme
colorscheme tir_black
" Yank
nmap Y y$
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
function s:Search(term)
  execute 'terminal rg --fixed-strings --max-columns 125 --pretty --smart-case --no-ignore-vcs ' . a:term
endfunction
command -nargs=1 Search call s:Search(<f-args>)
nmap <Leader>s :Search<Space>
" Search (with quickfix)
function s:SearchWithQuickfix(term)
  cgetexpr system('rg --vimgrep --fixed-strings --smart-case --no-ignore-vcs ' . a:term)
  copen
endfunction
command -nargs=1 SearchWithQuickfix call s:SearchWithQuickfix(<f-args>)
nmap <Leader>/ :SearchWithQuickfix<Space>
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
nmap <Leader>gl :BCommits!<CR>
nmap <Leader>gf :Git fetch<CR>
nmap <Leader>gp :Git push<CR>
nmap <Leader>gP :Git pull<CR>
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
au FileType javascript nmap <silent> <Leader>j <Plug>(coc-definition)
au FileType javascript nmap <silent> gd <Plug>(coc-definition)
au FileType javascript nmap <silent> gr <Plug>(coc-references)
" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction
" Symbol renaming.
nmap <Leader>rn <Plug>(coc-rename)
" Startify (start screen)
nmap <Leader>r :Startify<CR>
let g:startify_list_order = ['dir']
let g:startify_change_to_dir = 0
let g:startify_custom_header = []
" MJML
autocmd BufEnter *.mjml setlocal filetype=xml
au FileType clojure nmap <buffer> <silent> <Leader>j <Plug>FireplaceDjump
" Yank history
nnoremap <silent> <Leader>y :<C-u>CocList -A --normal yank<cr>
" COC
nmap <silent> <C-p> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-n> <Plug>(coc-diagnostic-next)
let g:coc_global_extensions = ['coc-json', 'coc-sql', 'coc-css', 'coc-html', 'coc-eslint', 'coc-conjure', 'coc-sh', 'coc-yank', 'coc-vimlsp', 'coc-xml', 'coc-yaml', 'coc-tsserver' ]
" Prettier
let g:prettier#config#semi = 'false'
let g:prettier#config#single_quote = 'true'
nmap <leader>p :Prettier<CR>
