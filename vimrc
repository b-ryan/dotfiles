" Buck's vimrc
" ------------
"
" Installed plugins:
"
"    plugin name      short description
"    --------------------------------------------------------------------------
"
"   Simple plugins (one script in the plugin/ dir)
"   * a.vim           Easy switching between header and source files
"   * grep
"   * MRU             Remembers recently visited files to open quickly
"   * Rename          Allows renaming of files by doing :Rename <file>
"   * pathogen        Plugin path manager (for bundling plugins)
"
"   Simple ftplugins (one script in the ftplugin/ dir)
"   * nsh
"   * php_folding
"   * xml             Contains function for pretty XML printing
"   * php             TODO remove
"
"   Bundled plugins
"   * bufexplorer     Buffer explorer
"   * clojure         Clojure highlighting, indenting, etc.
"   * coffee-script   Coffee highlight, indenting, etc.
"   * CtrlP           Fuzzy Finder
"   * fugitive        Git wrapper
"   * NERDTree        Provides a way to peruse directories
"   * OmniCppComplete C++ Omni-Complete
"   * powerline       Better status bar
"   * protodef        Creates skeleton C++ source files based on header files
"   * racket          Highlight, indenting, etc. for Racket language
"   * supertab
"   * surround        Surround text with tags, quotes, etc.
"   * taglist         CTags explorer for viewing all tags in open files
"   * tcomment        Quickly comment out lines or selections
"   * UltiSnips
"   * dwm.vim         Window management

call pathogen#infect()

" change <leader> to a comma
let mapleader = ","

" basic settings --------------------------------------------------------------
set nocompatible " not vi-compatible
set rnu " shows relative line numbers
set bs=2 " needed on Windows for backspace to work properly

" tab settings -------------------------------------------------------------->
set autoindent " Uses indent from current line as indent for new line
set expandtab " Inserts spaces instead of actual tabs
set shiftwidth=4 " The number of columns to use when auto-indenting lines
set tabstop=4 " Determines the number of columns to use when showing actual
              " tab characters
" set softtabstop=4 " Determines the number of columns that will be inserted or
                  " deleted when you hit the tab key. Will use a mixture of
                  " spaces and tabs when expandtab is not set.
set smarttab " Uses the value of shiftwidth when inserting or deleting tabs
             " at the beginnings of lines (in more practical terms, it allows
             " you to treat tabs that were expanded into spaces as tabs, but
             " only when they are at the beginning of a line).
             " When using softtabstop and expand tab, smarttab doesn't need
             " to be used
" From my understand, the fundamental difference between tabstop and
" softtabstop is that tabstop determines the behavior of actual tab characters
" (<Tab>). If neither expandtab nor softtabstop are set, then using the tab
" key will just insert the <Tab> character and use the value of tabstop to
" determine how wide the tab will be.
" On the other hand, softtabstop takes over when the tab key on your keyboard
" is pressed. If your softtabstop is set to 4 and tabstop is set to 8, then
" hitting tab will insert 4 columns for you (the value of softtabstop).
" However, since this isn't enough to be a tab character, it will use spaces
" to achieve those 4 columns.

" mapping to toggle spaces/tabs. This should work fine, but I haven't figured
" out under what circumstances behaviors will be exactly the same.
nmap <Leader>t :set expandtab!<CR>
" <---------------------------------------------------------------------------

set laststatus=2 " Ensures that if only 1 window is visible, the status bar will show
set statusline=%f\ %m%r " filename, modified flag, and readonly flag
set statusline+=%= " left/right separator
set statusline+=[%{strlen(&ft)?&ft:'none'}] " filetype
set statusline+=[%{&ff}] " file format (ie. line endings)
set statusline+=[%{strlen(&fenc)?&fenc:'none'}] " encoding
set statusline+=\ \|\ " a separator
set statusline+=line\ %l\ of\ %L " line number
" for more about customizing the status bar, see
" http://got-ravings.blogspot.com/2008/08/vim-pr0n-making-statuslines-that-own.html

set hidden " only hide buffers when switching (don't close them which erases undo)

" Visual whitespace
set list
set listchars=tab:>\ ,trail:.
autocmd BufEnter * set list " make sure visual whitespace is always shown

" searching
set incsearch " incremental search (i.e. search while typing)
set hlsearch  " highlight searched text
set ignorecase " ignore case on searches
set smartcase " override ignorecase when search term has uppercase in it

set mouse=a " enables mouse use in all modes

syntax enable " enables syntax highlighting
filetype on " enables filetype detection
filetype plugin on

" jump to last position on previous close
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

au FileType python set omnifunc=pythoncomplete#Complete

" automatically open and close the popup menu / preview window
" from: http://vim.wikia.com/wiki/C%2B%2B_code_completion
" au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
" set completeopt=menu,menuone,longest,preview
set completeopt=preview
set complete-=t,i

" From http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns/235970#235970
set colorcolumn=80

" split files below and to the right!
set splitbelow
set splitright

" don't save those annoying swp files
set noswapfile

" general key mappings --------------------------------------------------------

" Open vimrc / source it
nnoremap <Leader>ev :vsplit $MYVIMRC<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

" Change 'Y' to copy to end of line to be similar to D and C
nnoremap Y y$

" Keys for more efficient saving
nmap <F11> :w<CR>
nmap <F12> :wa<CR>
imap <F12> <ESC>:wa<CR>

" line movement mappings from http://vim.wikia.com/wiki/Moving_lines_up_or_down
" Use Alt-j or Alt-k to move lines up or down, respectively
nnoremap <A-j> :m+<CR>==
inoremap <A-j> <Esc>:m+<CR>==gi
vnoremap <A-j> :m'>+<CR>gv=gv
nnoremap <A-k> :m-2<CR>==
inoremap <A-k> <Esc>:m-2<CR>==gi
vnoremap <A-k> :m-2<CR>gv=gv

" Map Ctrl+Backspace in insert mode to delete back a word
inoremap <C-BS> <C-w>

" Proper Ctrl+C -> Esc map
map <C-c> <Esc>

" Remove trailing whitespace
nmap <Leader>w :%s/\s\+$//<CR>

" A bit tricky to explain: will put you insert mode on the next line (like
" doing just 'o', but will also put an extra blank line below this new line.
nnoremap <Leader>o o<esc>O

" Search for the word under the cursor, but stay on the current instance of it
nnoremap <Leader>n *N

" Set clojure indenting to two spaces
au BufEnter *.clj setlocal shiftwidth=2
au BufEnter *.c setlocal shiftwidth=2
au BufEnter Makefile setlocal noexpandtab

" -----------------------------------------------------------------------------
" Plugin settings and mappings
" -----------------------------------------------------------------------------

" phpfolding ------------------------------------------------------------------
map <F5> <Esc>:EnableFastPHPFolds<Cr>
map <F6> <Esc>:DisablePHPFolds<Cr>

" Tips for getting header/source switch came from
" http://vim.wikia.com/wiki/Easily_switch_between_source_and_header_file
" mappings for a.vim
nnoremap <F4> :A<CR>

" NERDTree settings and mappings
let NERDTreeIgnore=['\.swp$', '\.orig$', '\.pyc$', '\.class$', '__pycache__',
                \   '\.swo$']
let NERDTreeShowHidden=1 " show hidden files
" mapping to open NERDTree
nnoremap <F3> :NERDTreeToggle<CR>
" find the current file in NerdTree
map <leader>r :NERDTreeFind<CR>

" supertab -------------------------------------------------------------------
" kick off supertab with space
let g:SuperTabMappingForward = '<C-Space>'
let g:SuperTabMappingBackward = '<S-C-Space>'

" OmniCppComplete settings ---------------------------------------------------
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::o

" ctrlp ----------------------------------------------------------------------
let g:ctrlp_cmd = 'CtrlPLastMode'
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|build$',
    \ 'file': '\.swp$\|\.pyc$',
    \ }
let g:ctrlp_by_filename = 1 " default to filename search instead of full path
let g:ctrlp_regexp = 1 " default to regexp search

" taglist --------------------------------------------------------------------
let Tlist_Use_Right_Window = 1 " place taglist window on the right
let Tlist_Display_Prototype = 1 " show prototypes instead of tags
" mapping to open taglist
nmap <F7> :TlistToggle<CR>

" UltiSnips ------------------------------------------------------------------
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" dwm.vim --------------------------------------------------------------------
