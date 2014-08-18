"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                                                         "
"                                 COMPOT                                  "
"                                                                         "
"                 https://github.com/aerosol/vim-compot                   "
"                                                                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "compot"

" Basic groups {{{
hi Tabline          guifg=#191919 guibg=#929292 gui=NONE
hi TablineFill      guifg=#191919 guibg=#929292 gui=NONE
hi TablineSel       guifg=#929292 guibg=#191919 gui=bold
hi Cursor           guifg=NONE    guibg=#ffffff gui=NONE
hi Visual           guifg=#ffffff guibg=#319BD2 gui=NONE
hi CursorLine       guifg=NONE    guibg=#222222 gui=NONE
hi CursorColumn     guifg=NONE    guibg=#1f1f1f gui=NONE
hi ColorColumn      guifg=NONE    guibg=#1f1f1f gui=NONE
hi LineNr           guifg=#333333 guibg=#000000 gui=NONE
hi VertSplit        guifg=#303030 guibg=#303030 gui=NONE
hi MatchParen       guifg=#FF4600 guibg=#444444 gui=NONE
hi StatusLine       guifg=#929292 guibg=#303030 gui=bold
hi StatusLineNC     guifg=#929292 guibg=#303030 gui=NONE
hi Pmenu            guifg=#aaaaaa guibg=#133045 gui=NONE
hi PmenuSel         guifg=#ffffff guibg=#369BD5 gui=NONE
hi IncSearch        guifg=#ffffff guibg=#4B8A45 gui=NONE
hi Search           guifg=#ffffff guibg=#4B8A45 gui=NONE
hi Directory        guifg=NONE    guibg=NONE    gui=NONE
hi Folded           guifg=#B06490 guibg=#281721 gui=NONE
hi Normal           guifg=#929292 guibg=#191919 gui=NONE
hi Boolean          guifg=#39946a guibg=NONE    gui=NONE
hi Character        guifg=NONE    guibg=NONE    gui=NONE
hi Comment          guifg=#3c403b guibg=NONE    gui=NONE
hi Conditional      guifg=#86B9BE guibg=NONE    gui=NONE
hi Constant         guifg=#948369 guibg=NONE    gui=NONE
hi Define           guifg=#B1BE4F guibg=NONE    gui=NONE
hi ErrorMsg         guifg=#BE3A00 guibg=NONE    gui=NONE
hi WarningMsg       guifg=#BE3A00 guibg=NONE    gui=NONE
hi Float            guifg=#2E5C30 guibg=NONE    gui=NONE
hi Function         guifg=NONE    guibg=NONE    gui=NONE
hi Identifier       guifg=#777777 guibg=NONE    gui=NONE
hi Keyword          guifg=#819FBE guibg=NONE    gui=NONE
hi Label            guifg=#3695B9 guibg=NONE    gui=NONE
hi NonText          guifg=#333333 guibg=NONE    gui=NONE
hi Number           guifg=#2E5C30 guibg=NONE    gui=NONE
hi Operator         guifg=#4b4b4b guibg=NONE    gui=NONE
hi PreProc          guifg=#718394 guibg=NONE    gui=NONE
hi Special          guifg=#929292 guibg=NONE    gui=NONE
hi SpecialKey       guifg=#333333 guibg=NONE    gui=NONE
hi Statement        guifg=#86B9BE guibg=NONE    gui=NONE
hi StorageClass     guifg=#8E9878 guibg=NONE    gui=NONE
hi String           guifg=#A09754 guibg=NONE    gui=NONE
hi Tag              guifg=#606060 guibg=NONE    gui=NONE
hi Title            guifg=#929292 guibg=NONE    gui=bold
hi Todo             guifg=#ffffff guibg=#E8751F gui=bold
hi Type             guifg=NONE    guibg=NONE    gui=NONE
hi Underlined       guifg=NONE    guibg=NONE    gui=underline
hi DiffChange       guifg=#ffffff guibg=#346094 gui=NONE
hi DiffDelete       guifg=#ffffff guibg=#ff0000 gui=NONE
hi DiffAdd          guifg=#ffffff guibg=#3F9439 gui=NONE
hi DiffText         guifg=#ffffff guibg=NONE    gui=NONE
" }}}
" Erlang {{{
" Example highlighting groups available at:
" https://github.com/oscarh/vimerl/blob/master/syntax/erlang.vim
hi erlangType           guifg=#2F7B59 guibg=#222222 gui=NONE
hi erlangRecord         guifg=#3B6929 guibg=NONE    gui=NONE
hi erlangRecordDef      guifg=#3B6929 guibg=NONE    gui=NONE
hi erlangMacro          guifg=#4D5C62 guibg=NONE    gui=NONE
hi erlangTuple          guifg=#C14054 guibg=NONE    gui=NONE
hi erlangList           guifg=#DD6B6C guibg=NONE    gui=NONE
hi erlangBIF            guifg=#5E5B5A guibg=NONE    gui=NONE
hi erlangStringModifier guifg=#D9681E guibg=NONE    gui=NONE
hi erlangBoolean        guifg=NONE    guibg=#222222 gui=NONE
hi erlangOperator       guifg=#25765D guibg=NONE    gui=NONE
hi erlangGuard          guifg=#323C35 guibg=NONE    gui=NONE
hi erlangBitType        guifg=#A5AB9F guibg=NONE    gui=NONE
hi erlangBinary         guifg=#454842 guibg=NONE    gui=NONE
" }}}
" Extra highlights {{{
function! s:ExtraHighlights()
    match WhitespaceEOL /\s\+$/
    match VCSConflict '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
    match LongLine /\%>81v.\+/
endfunction

hi WhitespaceEOL guifg=white guibg=red  gui=NONE
hi LongLine      guifg=NONE  guibg=NONE gui=undercurl guisp=#333333
hi VCSConflict   guifg=white guibg=red  gui=NONE

au BufRead,BufNewFile * call s:ExtraHighlights()

hi SignColumn   guifg=#eeeeee guibg=NONE gui=NONE
hi ShowMarksHLl guifg=#00ff00 guibg=NONE gui=NONE
hi ShowMarksHLu guifg=#ff0000 guibg=NONE gui=NONE
hi ShowMarksHLm guifg=#00ffff guibg=NONE gui=NONE

hi link NERDTreeDir          Statement
hi link NERDTreeBookmarkName Label
hi link NERDTreeBookmark     Identifier
" }}}

