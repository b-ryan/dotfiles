"%% SiSU Vim color file
" Buck colorscheme, adapted from the slate colorscheme
" Maintainer: Buck Ryan <buck.ryan@gmail.com>
" Slate Maintainer: Ralph Amissah <ralph@amissah.com>

:set background=dark
:highlight clear

if version > 580
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif

" set t_Co=256

let colors_name = "buck"

" note that gui=none means there is no bold, underline, etc.
" see :h bold

" :hi  Normal
" :hi  Todo
" :hi  Include                           ctermfg=red         cterm=none

" Ones I care about the most

" SpecialKey is for visual whitespace
:hi  SpecialKey                        ctermfg=black       cterm=bold
:hi  Cursor        ctermbg=white       ctermfg=white
:hi  IncSearch     ctermbg=gray        ctermfg=black       cterm=none
:hi  Search        ctermbg=gray        ctermfg=black       cterm=underline,bold
:hi  Constant                          ctermfg=brown
:hi  Comment                           ctermfg=black       cterm=bold
:hi  Identifier                        ctermfg=lightcyan   cterm=none
:hi  Operator                          ctermfg=lightcyan   cterm=none
:hi  String                            ctermbg=None        ctermfg=darkcyan    cterm=bold

" language-specific

:hi  phpRegion                      ctermbg=None        ctermfg=white       cterm=none
:hi  phpIdentifier                  ctermfg=darkcyan
:hi  rubyString                     ctermbg=None        ctermfg=red         cterm=none
:hi  rubyInterpolation              ctermbg=None        ctermfg=red         cterm=bold
:hi  rubyInterpolationDelimiter     ctermbg=None        ctermfg=red         cterm=bold
:hi  clojureSpecial                 ctermbg=None        ctermfg=yellow      cterm=bold

" don't know what Special is - it was used for quotations and such in Ruby
:hi  Special       ctermbg=None        ctermfg=none       cterm=none

" Others
:hi  VertSplit                                             cterm=reverse
:hi  Folded        ctermbg=darkgrey    ctermfg=grey
:hi  FoldColumn    ctermbg=gray        ctermfg=darkblue
:hi  ModeMsg                           ctermfg=brown       cterm=none
:hi  MoreMsg                           ctermfg=darkgreen
:hi  NonText                           ctermfg=blue        cterm=bold
:hi  Question                          ctermfg=green
:hi  Search        ctermbg=White       ctermfg=black       cterm=none
:hi  StatusLine                                            cterm=bold,reverse
:hi  StatusLineNC                                          cterm=reverse
:hi  Title                             ctermfg=yellow      cterm=bold
:hi  Statement                         ctermfg=lightblue
:hi  Visual                                                cterm=reverse
:hi  WarningMsg                        ctermfg=darkblue
:hi  PreProc                           ctermfg=magenta     cterm=none
:hi  Define                            ctermfg=yellow
:hi  Type                              ctermfg=2
:hi  Function                          ctermfg=brown
:hi  Structure                         ctermfg=green
:hi  LineNr                            ctermfg=darkcyan
:hi  Ignore                            ctermfg=gray        cterm=bold
:hi  Directory                         ctermfg=darkcyan
:hi  ErrorMsg      ctermbg=darkblue    ctermfg=gray        cterm=bold
:hi  VisualNOS                                             cterm=bold,underline
:hi  WildMenu      ctermbg=darkcyan    ctermfg=black
:hi  DiffAdd       ctermbg=darkblue
:hi  DiffChange    ctermbg=darkmagenta
:hi  DiffDelete    ctermbg=brown       ctermfg=darkblue    cterm=bold
:hi  DiffText      ctermbg=darkblue    cterm=bold
:hi  Underlined                        ctermfg=darkmagenta cterm=underline
:hi  Error         ctermbg=darkblue    ctermfg=gray        cterm=bold
:hi  SpellErrors   ctermbg=darkblue    ctermfg=gray        cterm=bold
:hi  ColorColumn   ctermbg=White
:hi  MatchParen    ctermbg=None        ctermfg=blue        cterm=bold
