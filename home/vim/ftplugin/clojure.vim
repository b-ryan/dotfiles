nmap <buffer> <LocalLeader>e :Eval<CR>
setlocal iskeyword+=>
setlocal iskeyword-=,
setlocal iskeyword+=&
setlocal iskeyword+=%
setlocal shiftwidth=2
setlocal lispwords+=facts,fact,wcar,GET,POST,PATCH,DELETE
" setlocal iskeyword-=/
" setlocal iskeyword-=.
