execute pathogen#infect()
syntax on
filetype plugin indent on
colorscheme ir_black 
" map leader to comma.  turned off for now because my fingers are too used to
" \-rs/rl/rf for running specs.
" let mapleader = ","

set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set ruler
set number
set backspace=indent,eol,start
set hlsearch
set incsearch
nmap <silent> <leader>n :silent :nohlsearch<CR>
set visualbell

" leader-rs to run a spec file
nmap <silent> <leader>rs :call RunRspecCurrentFileConque()<CR>
" leader-rf to run focused on line number
nmap <silent> <leader>rf :call RunRspecCurrentLineConque()<CR>
" leader-rl for run last command
nmap <silent> <leader>rl :call RunLastConqueCommand()<CR>

colorscheme railscasts-tweaked

set wildignore=public/**,.git/,log
let g:CommandTMaxFiles=20000

set expandtab
set shiftwidth=4
set tabstop=4
