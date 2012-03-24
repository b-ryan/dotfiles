" basic settings --------------------------------------------------------------
set nocompatible " not vi-compatible
" set number " shows line numbers
set rnu " shows relative line numbers
set guifont=Consolas:h11:cANSI
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
if has("gui_running")
    colorscheme kellys
    if has("gui_gtk2")
        set guifont=Inconsolata\ 12
    elseif has("gui_win32")
        au GUIEnter * simalt ~x " starts gvim in maximized mode
        set guifont=Inconsolata:h12:cANSI
    endif
endif
set bs=2 " needed on Windows for backspace to work properly

" tab settings ->
set autoindent
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4
" <-

set visualbell

" searching
set incsearch " incremental search (i.e. search while typing)
set hlsearch  " highlight searched text
set ignorecase " ignore case on searches

set mouse=a " enables mouse use in all modes
syntax enable " enables syntax highlighting
filetype on " enables filetype detection
filetype plugin on
au BufNewFile,BufRead *.nsh set filetype=nsis " sets filetype for NSH scripts

let mapleader = ","

" jump to last position on previous close
if has("autocmd")
    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
endif

" Brackets & paretheses
" bracket completion
inoremap {<CR> {<CR>}<Esc>O<Tab>

" other stuff -----------------------------------------------------------------
" From http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns/235970#235970
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
map :a<CR> :wa<CR>

" line movement mappings from
" http://vim.wikia.com/wiki/Moving_lines_up_or_down
nnoremap <A-j> :m+<CR>==
inoremap <A-j> <Esc>:m+<CR>==gi
vnoremap <A-j> :m'>+<CR>gv=gv
nnoremap <A-k> :m-2<CR>==
inoremap <A-k> <Esc>:m-2<CR>==gi
vnoremap <A-k> :m-2<CR>gv=gv

" Map Ctrl+Del in insert mode to delete back a word
inoremap <C-BS> <C-w>

" Proper Ctrl+C -> Esc map
imap <C-c> <Esc>
nmap <C-c> <Esc>

" fold mapping
nnoremap + zo
nnoremap - zc

" plugin mappings ------------------------------------------------------------
nnoremap <F5> :tabm<CR>
nmap <C-S-PageUp> :tabm tabpagenr()-1<CR>
nmap <C-S-PageDown> :tabm tabpagenr()+1<CR>
" Tips for getting header/source switch came from
" http://vim.wikia.com/wiki/Easily_switch_between_source_and_header_file
" mappings for a.vim
" nnoremap <F4> :AT<CR>
nnoremap  <F4> :AT<CR>
inoremap <F4> <Esc>:AT<CR>
" map <F4> :FSRight<CR>
" map <F4> :e %:p:s,.h$,.X123X,:s,.cpp$,.h,:s,.X123X$,.cpp,<CR>

" NERDTree settings and mappings
let NERDTreeIgnore=['\.swp$', '\.orig$', '\.pyc$', '\.class$']
nnoremap  <F3> :NERDTreeToggle<CR><C-w><C-w>:q<CR> " open NERDTree and close split
inoremap <F3> <Esc>:NERDTreeToggle<TR>a

" auto complete ---------------------------------------------------------------
" Cpp auto complete tag files
set tags+=~/vimfiles/tags/cpp
set tags+=~/vimfiles/tags/qt4
set tags+=~/vimfiles/tags/gridmule

" let OmniCpp_SelectFirstItem = 1 " selects the first item in the complete box
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::o

" Fuzzy Finder ---------------------------------------------------------------
nnoremap <F6> :FufDir<CR>
nnoremap <F7> :FufFile<CR>

let g:SuperTabLongestEnhanced=1 " Fills in the longest common text found
let g:SuperTabLongestHighlight=1 " automatically highlights the first entry

" automatically open and close the popup menu / preview window
" FROM: http://vim.wikia.com/wiki/C%2B%2B_code_completion
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

" Mapping to auto-format the entire document and return
" to original position
nnoremap <F8> mzgggqG`z
