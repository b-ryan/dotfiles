" vim-slamhound
"
" Author:   guns <self@sungpae.com>
" License:  Eclipse Public License
" Homepage: https://github.com/guns/vim-slamhound
" Requires: slamhound - https://github.com/technomancy/slamhound
"           fireplace.vim - https://github.com/tpope/vim-fireplace

function! slamhound#reconstruct(file, textwidth)
    let file = escape(a:file, '"')
    let tw = a:textwidth < 1 ? 80 : a:textwidth
    call fireplace#session_eval(
        \   "(require 'slam.hound 'clojure.pprint)"
        \ . '(binding [clojure.pprint/*print-right-margin* ' . tw . ']'
        \ . '  (slam.hound/swap-in-reconstructed-ns-form (java.io.File. "' . file . '")))'
        \ )
    edit
endfunction
