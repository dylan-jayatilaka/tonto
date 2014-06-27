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

" -----------------------
" Reopen at last position
" :h last-position-jump
" -----------------------
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

" --------------------
"  Taglist
" --------------------
let tlist_foo_settings = 'foo;m:macros;t:types;v:array types;g:module variables;i:interfaces;r:routines'
"let tlist_foo_settings = 'foo;m:macro;a:attribute;i:interface;f:function;s:subroutine'

" let Tlist_Ctags_Cmd = '/home/dylan/bin/ctags'
let Tlist_Auto_Open = 0
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 35
let Tlist_Enable_Fold_Column = 0
let Tlist_Exist_OnlyWindow = 1
let Tlist_Sort_Type = "order" " sort by order or name 
" let Tlist_Display_Prototype = 1
" let Tlist_Display_Tag_Scope = 0
nnoremap <silent> <F2> :TlistToggle<CR>
