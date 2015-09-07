" 인터페이스 영어 설정

let $LANG = 'en'
language messages en

" 읽어들이는 파일 인코딩 설정(앞에서부터 차례대로 시도)

set fencs=utf-8,cp949

" FDP 설정

if has("autocmd")
    au BufNewFile,BufRead *.sgml,*.ent,*.xsl,*.xml call Set_SGML()
    au BufNewFile,BufRead *.[1-9] call ShowSpecial()
endif " has(autocmd)

function Set_Highlights()
    "match ExtraWhitespace /^\s* \s*\|\s\+$/
    highlight default link OverLength ErrorMsg
    match OverLength /\%71v.\+/
    return 0
endfunction

function ShowSpecial()
    setlocal list listchars=tab:>>,trail:*,eol:$
    hi def link nontext ErrorMsg
    return 0
endfunction " ShowSpecial()

function Set_SGML()
    setlocal number
    syn match sgmlSpecial "&[^;]*;"
    setlocal syntax=sgml
    setlocal filetype=xml
    setlocal shiftwidth=2
    setlocal textwidth=70
    setlocal tabstop=8
    setlocal softtabstop=2
    setlocal formatprg="fmt -p"
    setlocal autoindent
    setlocal smartindent
    " Rewrap paragraphs
    noremap P gqj
    " Replace spaces with tabs
    noremap T :s/        /\t/<CR>
    call ShowSpecial()
    call Set_Highlights()
    return 0
endfunction " Set_SGML()

" 줄 번호 표시

set nu
