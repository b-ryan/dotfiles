nmap <buffer> <LocalLeader>e :Eval<CR>
setlocal iskeyword+=>
setlocal iskeyword-=,
setlocal iskeyword+=&
setlocal iskeyword+=%
setlocal shiftwidth=2
setlocal lispwords+=facts,fact,wcar,GET,POST,PATCH,DELETE
" setlocal iskeyword-=/
" setlocal iskeyword-=.
"
nmap <localleader>f :!bin/zprint-files %<cr>


" Restore cursor position, window position, and last search after running a
" command.
" https://stackoverflow.com/questions/15992163/how-to-tell-vim-to-auto-indent-before-saving
function! Preserve(command)
  let search = @/
  let cursor_position = getpos('.')
  normal! H
  let window_position = getpos('.')
  call setpos('.', cursor_position)
  execute a:command
  let @/ = search
  call setpos('.', window_position)
  normal! zt
  call setpos('.', cursor_position)
endfunction

function! Indent()
  call Preserve('normal gg=G')
endfunction
