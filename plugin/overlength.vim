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
