" Buck's vimrc
" ------------
"
" Installed plugins:
"
"    plugin name      short description
"    --------------------------------------------------------------------------
"   * NERDTree        Provides a way to peruse directories
"   * rename.vim      Allows renaming of files by doing :rename <file>
"   * a.vim           Easy switching between header and source files
"   * MRU             Remembers recently visited files to open quickly
"   * protodef        Creates skeleton C++ source files based on header files
"   * snipMate        Snippets!
"   * surround        Surround text with tags, quotes, etc.
"   * tcomment        Quickly comment out lines or selections
"   * OmniCppComplete C++ Omni-Complete
"   * CtrlP           Fuzzy Finder
"   * delimitMate     Auto complete brackets, etc.

" basic settings --------------------------------------------------------------
set nocompatible " not vi-compatible
set rnu " shows relative line numbers
if has("gui_running")
    set background=dark
    colorscheme solarized
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    if has("gui_gtk2") " Running on Linux
        set guifont=Inconsolata\ 12
    elseif has("gui_win32") " Running on Windows
        set guifont=Inconsolata:h12:cANSI
        au GUIEnter * simalt ~x " starts gvim in maximized mode
    endif
endif
set bs=2 " needed on Windows for backspace to work properly

" tab settings ->
set autoindent " Uses indent from current line as indent for new line
set expandtab " Expands tab into spaces
set smarttab " Allows deleting of full tab at beginning of lines when it's turned into spaces
set shiftwidth=4
set softtabstop=4
" <-

" Visual whitespace
set list
set listchars=tab:>\ ,trail:. 

set visualbell " Stops the 'ding' heard all the time

" searching
set incsearch " incremental search (i.e. search while typing)
set hlsearch  " highlight searched text
set ignorecase " ignore case on searches

set mouse=a " enables mouse use in all modes
syntax enable " enables syntax highlighting
filetype on " enables filetype detection
filetype plugin on

" change <leader> to a comma
let mapleader = ","

" jump to last position on previous close
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" other stuff -----------------------------------------------------------------
" From http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns/235970#235970
set colorcolumn=80
highlight OverLength ctermbg=red ctermfg=white guibg=#DE7676
let g:OverLengthOn = 0
function OverLengthToggle()
    if g:OverLengthOn == 1
        match none
        let g:OverLengthOn = 0
    else
        match OverLength /.\%>81v/
        let g:OverLengthOn = 1
    endif
endfunction
nnoremap <C-h> :call OverLengthToggle()<CR>

" set cursorline
" highlight CursorLine guibg=#FFE0F7
" highlight CursorColumn guibg=#FFE0F7
nnoremap <Leader>l :set cursorline!<CR>
nnoremap <Leader>c :set cursorcolumn!<CR>

" general key mappings --------------------------------------------------------
" Change 'Y' to copy to end of line to be similar to D and C
nnoremap Y y$

" Fix mistake I often make -> typing :a instead of :wa
map :a<CR> :wa<CR>

" Keys for more efficient saving
nnoremap <F11> :w<CR>
nnoremap <F12> :wa<CR>

" line movement mappings from http://vim.wikia.com/wiki/Moving_lines_up_or_down
" Use Alt-j or Alt-k to move lines up or down, respectively
nnoremap <A-j> :m+<CR>==
inoremap <A-j> <Esc>:m+<CR>==gi
vnoremap <A-j> :m'>+<CR>gv=gv
nnoremap <A-k> :m-2<CR>==
inoremap <A-k> <Esc>:m-2<CR>==gi
vnoremap <A-k> :m-2<CR>gv=gv

" Map Ctrl+Del in insert mode to delete back a word
inoremap <C-BS> <C-w>

" Proper Ctrl+C -> Esc map
map <C-c> <Esc>

" fold mapping
nnoremap + zo
nnoremap - zc

" Tab movements
nnoremap <F5> :tabm<CR>
nmap <C-S-PageUp> :tabm tabpagenr()-1<CR>
nmap <C-S-PageDown> :tabm tabpagenr()+1<CR>

" Mapping to auto-format the entire document and return
" to original position
nnoremap <F8> mzgggqG`z

" automatically open and close the popup menu / preview window
" from: http://vim.wikia.com/wiki/C%2B%2B_code_completion
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menu,menuone,longest,preview

" -----------------------------------------------------------------------------
" Plugin settings and mappings
" -----------------------------------------------------------------------------

" Tips for getting header/source switch came from
" http://vim.wikia.com/wiki/Easily_switch_between_source_and_header_file
" mappings for a.vim
nnoremap <F4> :A<CR>
inoremap <F4> <Esc>:A<CR>

" NERDTree settings and mappings
let NERDTreeIgnore=['\.swp$', '\.orig$', '\.pyc$', '\.class$']
let NERDTreeChDirMode=2 " set the CWD whenever NERDTree root changes
let NERDTreeShowHidden=1 " show hidden files
" mappings to open NERDTree
nnoremap <F3> :NERDTreeToggle<CR>
inoremap <F3> <Esc>:NERDTreeToggle<TR>a
" open current file in NerdTree
map <leader>r :NERDTreeFind<cr>

" OmniCppComplete settings ---------------------------------------------------
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::o

" delimitMate ----------------------------------------------------------------
let delimitMate_expand_cr=1 " Expand carriage return
let delimitMate_expand_space=1 " Expand spaces
silent! imap <unique> <buffer> <C-Tab> <Plug>delimitMateS-Tab
silent! imap <unique> <buffer> <C-S-Tab> <Plug>delimitMateJumpMany
