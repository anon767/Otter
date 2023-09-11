function! w:select_file()
    let w:line_list = readfile(input("Enter file: "))
    let w:index = 0
    call w:goto_line()
endfunction

function! w:goto_line()
    if exists("w:line_list")
        let current_file = bufname("%")
        let line = w:line_list[w:index]
        let result = matchlist(line, '\(.*\):\(.*\)')
        if empty(result)
            throw "Error parsing line ``".line."''"
        else
            let filename = result[1]
            let linenum = str2nr(result[2])
            if filename != current_file
                exec "e ".filename
            endif
            exec "normal ".linenum."G"
            echo w:index." : ".filename." ".linenum
        endif
    else
        echo "Error: no file loaded"
    endif
endfunction

function! w:next_line()
    let w:index = w:index < len(w:line_list)-1 ? w:index + 1 : w:index
    call w:goto_line()
endfunction

function! w:prev_line()
    let w:index = w:index > 0 ? w:index - 1 : w:index
    call w:goto_line()
endfunction

nmap <C-L><C-L>  :call w:select_file()<CR>
nmap <C-N>  :call w:next_line()<CR>
nmap <C-P>  :call w:prev_line()<CR>
    
