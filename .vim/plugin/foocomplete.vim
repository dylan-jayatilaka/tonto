" foocomplete.vim -- Context-sensitive tag completion

if &cp || exists("loaded_foocomplete")
    finish
endif

let loaded_foocomplete = 1

" Store vim arguments?
let s:save_cpo = &cpo
set cpo&vim

command! -nargs=* FooCompleteOn  call foocomplete#On()
command! -nargs=* FooCompleteOff call foocomplete#Off()

" Put back vim arguments?
let &cpo = s:save_cpo
unlet s:save_cpo


finish

