" Buck's vimrc
" ------------

call pathogen#infect()

let mapleader = ","

""" See :help 'option' for details on any option below

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
nmap <Leader>t :setlocal expandtab!<CR>
" <---------------------------------------------------------------------------

set rnu
set bs=2
set laststatus=2
set hidden
set list
set listchars=tab:>\ ,trail:.
set incsearch
set hlsearch
set ignorecase
set smartcase
set mouse=a
set splitbelow
set splitright
set noswapfile
set completeopt=menu,preview
set complete-=t,i
set colorcolumn=80
set showmatch
set wildmenu

set statusline=%f\ %m%r " filename, modified flag, and readonly flag
set statusline+=%= " left/right separator
set statusline+=[%{strlen(&ft)?&ft:'none'}] " filetype
set statusline+=[%{&ff}] " file format (ie. line endings)
set statusline+=[%{strlen(&fenc)?&fenc:'none'}] " encoding
set statusline+=\ \|\ " a separator
set statusline+=line\ %l\ of\ %L " line number
" for more about customizing the status bar, see
" http://got-ravings.blogspot.com/2008/08/vim-pr0n-making-statuslines-that-own.html

autocmd BufEnter * set list " make sure visual whitespace is always shown

filetype plugin indent on
syntax enable

" jump to last position on previous close
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" general key mappings --------------------------------------------------------

" Open vimrc / source it
nnoremap <Leader>ev :vsplit $MYVIMRC<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

" Change 'Y' to copy to end of line to be similar to D and C
nnoremap Y y$

" Keys for more efficient saving
nmap <F11> :w<CR>
nmap <F12> :wa<CR>

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

inoremap <A-e> ë

" Proper Ctrl+C -> Esc map
map <C-c> <Esc>

" Remove trailing whitespace
nmap <Leader>w :%s/\s\+$//<CR>

" A bit tricky to explain: will put you insert mode on the next line (like
" doing just 'o', but will also put an extra blank line below this new line.
nnoremap <Leader>o o<esc>O

" Search for the word under the cursor, but stay on the current instance of it
nnoremap <Leader>n *N

autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType clojure setlocal iskeyword+=>
autocmd FileType clojure setlocal iskeyword-=/
autocmd FileType clojure setlocal iskeyword-=,
autocmd FileType clojure setlocal iskeyword-=.
autocmd FileType clojure setlocal shiftwidth=2
autocmd BufRead *.clj,*.c,*.html,*.js setlocal shiftwidth=2
autocmd BufRead Makefile,*.php setlocal noexpandtab
autocmd BufRead *.pp,Vagrantfile set ft=ruby
autocmd BufNewFile,BufRead *.edn set filetype=clojure
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.ino set filetype=c

" -----------------------------------------------------------------------------
" Plugin settings and mappings
" -----------------------------------------------------------------------------

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
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
let g:SuperTabMappingForward = '<C-Space>'
let g:SuperTabMappingBackward = '<S-C-Space>'

" OmniCppComplete settings ---------------------------------------------------
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::o

" ctrlp ----------------------------------------------------------------------
let g:ctrlp_cmd = 'CtrlPLastMode'
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|\.venv$\|env$\|build$\|\.compiled$\|\.awesomo',
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

" VimClojure -----------------------------------------------------------------
" notes on setup:
"http://naleid.com/blog/2011/12/19/getting-a-clojure-repl-in-vim-with-vimclojure-nailgun-and-leiningen/
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
let g:vimclojure#NailgunClient = "/home/vagrant/bin/ng"
let g:vimclojure#WantNailgun = 1
let g:vimclojure#SplitPos = "right"
" let g:vimclojure#SplitSize = 10
