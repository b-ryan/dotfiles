
set background=dark "or light
highlight clear
if exists("syntax_on")
	syntax reset
endif
" let g:colors_name = "vivify"
set t_Co=256

highlight Boolean             guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight CTagsClass          guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight CTagsGlobalConstant guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight CTagsGlobalVariable guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight CTagsImport         guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight CTagsMember         guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight Character           guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Comment             guifg=#F2B1CB ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Conditional         guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight Constant            guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Cursor              guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight CursorColumn        guifg=#eeeeee ctermfg=255 guibg=#303132 ctermbg=67  gui=none cterm=none
highlight CursorLine          guifg=#eeeeee ctermfg=255 guibg=#303132 ctermbg=67  gui=none cterm=none
highlight Debug               guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Define              guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight DefinedName         guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight Delimiter           guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight DiffAdd             guifg=#2a2b2f ctermfg=60  guibg=#9ab2c8 ctermbg=110 gui=none cterm=none
highlight DiffChange          guifg=#2a2b2f ctermfg=60  guibg=#d1c79e ctermbg=187 gui=none cterm=none
highlight DiffDelete          guifg=#67686b ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight DiffText            guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Directory           guifg=#e6ac32 ctermfg=179 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight EnumerationName     guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight EnumerationValue    guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight Error               guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight ErrorMsg            guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight Exception           guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight Float               guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight FoldColumn          guifg=#2a2b2f ctermfg=60  guibg=#67686b ctermbg=60  gui=none cterm=none
highlight Folded              guifg=#2a2b2f ctermfg=60  guibg=#67686b ctermbg=60  gui=none cterm=none
highlight Function            guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Identifier          guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Ignore              guifg=#67686b ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
" highlight IncSearch           guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight Include             guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Keyword             guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight Label               guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight LineNr              guifg=#67686b ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight LocalVariable       guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight Macro               guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight MatchParen          guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=bold,underline cterm=bold,underline
highlight ModeMsg             guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight MoreMsg             guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight NonText             guifg=#67686b ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Normal              guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Number              guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Operator            guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight PMenu               guifg=#2a2b2f ctermfg=60  guibg=#9ab2c8 ctermbg=110 gui=none cterm=none
highlight PMenuSbar           guifg=#2a2b2f ctermfg=60  guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight PMenuSel            guifg=#2a2b2f ctermfg=60  guibg=#62acce ctermbg=74  gui=bold cterm=bold
highlight PMenuThumb          guifg=#2a2b2f ctermfg=60  guibg=#62acce ctermbg=74  gui=none cterm=none
highlight PreCondit           guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight PreProc             guifg=#d1d435 ctermfg=185 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Question            guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight Repeat              guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
" highlight Search              guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight SignColumn          guifg=#2a2b2f ctermfg=60  guibg=#67686b ctermbg=60  gui=none cterm=none
highlight Special             guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight SpecialChar         guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight SpecialComment      guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight SpecialKey          guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight SpellBad            guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight SpellCap            guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight SpellLocal          guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight SpellRare           guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight Statement           guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight StatusLine          guifg=#2a2b2f ctermfg=60  guibg=#62acce ctermbg=74  gui=bold cterm=bold
highlight StatusLineNC        guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight StorageClass        guifg=#e6ac32 ctermfg=179 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight String              guifg=#d1c79e ctermfg=187 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Structure           guifg=#e6ac32 ctermfg=179 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight TabLine             guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight TabLineFill         guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight TabLineSel          guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight Tag                 guifg=#9ab2c8 ctermfg=110 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Title               guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight Todo                guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight Type                guifg=#e6ac32 ctermfg=179 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Typedef             guifg=#e6ac32 ctermfg=179 guibg=#2a2b2f ctermbg=60  gui=none cterm=none
highlight Underlined          guifg=#e1e0e5 ctermfg=146 guibg=#2a2b2f ctermbg=60  gui=underline cterm=underline
highlight Union               guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight VertSplit           guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight Visual              guifg=#2a2b2f ctermfg=60  guibg=#e1e0e5 ctermbg=146 gui=none cterm=none
highlight VisualNOS           guifg=#eeeeee ctermfg=255                           gui=none cterm=none
highlight WarningMsg          guifg=#e1e0e5 ctermfg=146 guibg=#9d0e15 ctermbg=204 gui=bold cterm=bold
highlight WildMenu            guifg=#62acce ctermfg=74  guibg=#2a2b2f ctermbg=60  gui=bold cterm=bold
highlight pythonBuiltin       guifg=#e1e0e5 ctermfg=146                           gui=none cterm=none
