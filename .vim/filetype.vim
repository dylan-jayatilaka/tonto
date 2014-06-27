augroup filetype
au! BufNewFile,BufRead *.foo set filetype=foo
au! BufNewFile,BufRead macros set filetype=foo
au! BufNewFile,BufRead *.F90 set filetype=fortran
au! BufNewFile,BufRead *.F95 set filetype=fortran
let fortran_dialect="f95"
let fortran_fold="yes"
augroup END
