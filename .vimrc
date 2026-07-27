set viminfo=""
set nocompatible
set updatecount=0
set visualbell
set ff=unix
set uc=0
set textwidth=70
set ruler
set history=50
set expandtab

set autoindent shiftwidth=3

set foldmethod=syntax
" set foldclose=all
set foldlevel=1
if &t_Co > 1
   syntax on
endif

let fortran_free_source=1
let fortran_more_precise=0
let fortran_have_tabs=1
" Performance Fixes
set redrawtime=10000
set synmaxcol=133

" -----------------------
" Reopen at last position
" :h last-position-jump
" -----------------------
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

