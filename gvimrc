colorscheme pyte
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
if has("gui_gtk2") " Running on Linux
    set guifont=Inconsolata\ 12
elseif has("gui_win32") " Running on Windows
    set guifont=Inconsolata:h12:cANSI
    au GUIEnter * simalt ~x " starts gvim in maximized mode
endif
" visualmark highlight
highlight SignColor ctermfg=white ctermbg=blue guifg=#333 guibg=#999
