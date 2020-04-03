"NO vi SIMULATION
set nocompatible

"ENABLE PLUGIN MANAGE
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'morhetz/gruvbox'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-fugitive'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-surround'
Plugin 'jiangmiao/auto-pairs'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
call vundle#end()
filetype plugin indent on

"COLOR SCHEME CONFIG
set term=screen-256color
set background=dark
colorscheme gruvbox

"MOUSE
set mouse=a

set number

"SEARCH OPTIONS
set hlsearch
set incsearch
set ignorecase

"CURSORLINE
set cursorline

"TAB SPACE
set tabstop=2
set softtabstop=2
set expandtab

"Allow switch buffers w/o save
set hidden

"SIZE OF INDENT
set shiftwidth=2
set backspace=indent,eol,start

let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/.ycm_extra_conf.py'

"Enable powerline
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup
set laststatus=2

set splitbelow
set splitright

:syntax enable
"Remove all trailing whitespace by pressing F5
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

"Config NERDTree
"NerdTree Toggle
map <C-n> :NERDTreeToggle<CR>
"Close vim when NerdTree is only left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


