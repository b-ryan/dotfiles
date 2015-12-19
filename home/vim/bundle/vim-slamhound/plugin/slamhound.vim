" vim-slamhound
"
" Author:   guns <self@sungpae.com>
" License:  Eclipse Public License
" Homepage: https://github.com/guns/vim-slamhound
" Requires: slamhound - https://github.com/technomancy/slamhound
"           fireplace.vim - https://github.com/tpope/vim-fireplace

augroup slamhound
        autocmd!
        autocmd FileType clojure
                \ command! -bar -buffer Slamhound
                \ if &modified |
                \       echom "Buffer contains unsaved changes!" |
                \ else |
                \       call slamhound#reconstruct(expand('%'), &textwidth) |
                \ endif
augroup END
