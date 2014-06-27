" foocomplete.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-11-02.
" @Last Change: 2007-11-11.
" @Revision:    0.0.197

if &cp || exists("loaded_foocomplete_autoload")
    finish
endif
let loaded_foocomplete_autoload = 1
let &completeopt = 'menu,preview,longest'


" Turn foo completion on
" Make the internal tags table
function! foocomplete#On() "{{{3
    setlocal omnifunc=foocomplete#Complete
    setlocal completefunc=foocomplete#Complete
    " Set up internal tags file with more info
    let s:analysed_tags = foocomplete#analyse_tags()
    " DechoRemOn
endf


" Turn foo completion off
function! foocomplete#Off() "{{{3
    unlet s:analysed_tags
endf


" Analyse the foo tags file and return 
" and internal "tags" dictionary.
" The foo tags file must be *unsorted*
function! foocomplete#analyse_tags() 
    " Get the tags 
    let tags = taglist('^')
    " Annotate the tags with more info
    let type = ''
    let i = 0
    for tag in tags
       " Is it a macro?
       if tag.filename ==# 'macros'
             let tag.kind = 'm'
             let tag.class = 'MACROS'
       " Is it a type or attribute?
       elseif tag.filename ==# 'types.foo'
          if tag.name =~# '^[A-Z_0-9{,}]*$'
             let type = tag.name
             let tag.kind = 't'
             let tag.class = type
          else
             let tag.kind = 'a'
             let tag.class = type
             let tag.type = matchstr(tag.cmd,' ::\s*\zs[A-Z_0-9,{}]\+\ze')
          endif
       " Is it a module variable, function or subroutine?
       else
          let tag.class = toupper(matchstr(tag.filename,'^\zs[a-z_0-9,{}]\+\ze'))
          if tag.cmd =~# ' :: '
             let tag.kind = 'g'
             let tag.type = matchstr(tag.cmd,' :: \s*\zs[A-Z_0-9,{}]\+\ze')
          else
             if tag.cmd =~# 'result\s*(\w\{-\})'
                let tag.kind = 'f'
             else
                let tag.kind = 's'
             endif
          endif
       endif
    endfor
    return tags
endf


" A wrapper for doing the completion
function! foocomplete#Complete(findstart, base) "{{{3
    if a:findstart
        return s:Complete(a:findstart, a:base)
    else
        let tags = s:Complete(a:findstart, a:base)
        return tags
    endif
endf


" Do the completion
function! s:Complete(findstart, base) "{{{3
    " Get current line and positions
    let line = getline('.')
    let start = col('.')
    let type = ''
    if a:findstart
        " First call: return start character
        let start -= 1
        while start > 0 && line[start - 1] =~ '\w'
            let start -= 1
        endwhile
        if line[start - 1] !~ '.'
           let start = col('.')
        endif
        return start
    else
        " Second call: get start character again
        let start -= 1
        while start > 0 && line[start - 1] =~ '\w'
            let start -= 1
        endwhile
        if line[start - 1] !~ '.'
           let start = col('.')
           return [ ]
        endif
        " Next, get context i.e. the part before a:base
        let context = strpart(line, 0, start)
        let context = substitute(context,'^\s*','','')
        let context = substitute(context,'\.\s*$','','')
        " Next, get the type of the a:base object, based on context
        let type = foocomplete#base_type(context)
        " Now, Get the matching tags
      " let constraints = {}
      " let constraints.name = tlib#rx#Escape(a:base)
      " let tags = tlib#tag#Collect(constraints,'',0)
        let tags = foocomplete#collect_tags(type,a:base) 
        " Replace menu tags (dictionary) by a string list
        call map(tags, 'v:val.name')
        " Sort the tags
        call sort(tags)
        " Return sorted menu
        return tags
    endif
endf


" Get the "type" of the parent variable given the "context"
function! foocomplete#base_type(context) "{{{3

    " Type to return
    let type = '?'
    " Variable for parent
    let var = ''
    " Self dotted variable,  returning matched parent
    let sdotvar   = matchstr(a:context,'\C\(^\|[^a-zA-Z_0-9).]\|self\)\.\zs\([a-zA-Z][a-zA-Z_0-9(,).]\+\)\?\ze$')
    " Local dotted variable, returning matched parent
    let ldotvar   = matchstr(a:context, '\C\(^\|[^a-zA-Z_0-9().]\)\zs[a-zA-Z][a-zA-Z_0-9(,).]\+\ze$')

    " Local dot variable
    if !empty(ldotvar) 
        " Set the parent variable "var"
        let var = ldotvar
        " Get the "head" of the variable and any array parts in "full"
        let head = matchstr(var, '\C^\zs[a-zA-Z][a-zA-Z_0-9]\+\ze')
        let full = matchstr(var, '\C^\zs[a-zA-Z][a-zA-Z_0-9(,)]\+\ze')
        " Get the type of the variable
        if head =~ 'self'
            " Set the type from the buffer file name
            let type = toupper(matchstr(butname(1), '\C^\zs[a-z_0-9{,}]\+\ze'))
        else
            " Set the type from the declaration
            let declaration = '\C\<' . head . '\>.\{-\}::\s\+\zs[A-Z_0-9,{}]\+\ze'
            let line = search(declaration, 'bn')
            if line!=0
                let type = matchstr(getline(line), declaration)
            endif
        endif
        " If this is an array variable get type component
        if head !~ full
           let type = matchstr(type,'{\zs[A-Z_0-9{,}]\+\ze}$')
        endif
        " Move to the next component, remove head
        let var = substitute(var, '^' . full . '\.\?', '', '')

    " Must be a self dot variable
    else 
        " Set the parent variable "var"
        let var = sdotvar
        " Set the type from the buffer file name
        let type = toupper(matchstr(bufname(1), '\C^\zs[a-z_0-9{,}]\+\ze'))
    endif

    " Get components of "var", if any
     while !empty(var) && type !~ '?'
        " Get the "head" of the variable and any array parts in "full"
        let head = matchstr(var, '\C^\zs[a-zA-Z][a-zA-Z_0-9]\+\ze')
        let full = matchstr(var, '\C^\zs[a-zA-Z][a-zA-Z_0-9(,)]\+\ze')
        " Find the type of the attribute
        let rx  = '!empty(v:val.name) && v:val.name =~# "^' . head .  '" && ' 
        let rx .= 'v:val.class =~# "^' . type . '" && v:val.kind =~ "[a]"'
        let tags = filter(deepcopy(s:analysed_tags),rx) 
        if !empty(tags)
           let type = tags[0].type
        else
           let type = '?'
        endif
        " If this is an array variable get type component
        if head !~ full
           let type = matchstr(type,'{\zs[A-Z_0-9{,}]\+\ze}$')
        endif
        " Move to the next component, remove head
        let var = substitute(var, '^' . full . '\.\?', '', '')
     endwhile

    return type

endf


" Collect tags that have the right "type" and which 
" match the "base". The base may be empty
function! foocomplete#collect_tags(type,base) "{{{3
    " Filter out wrong types
    let rx = 'v:val.class =~# "^' . a:type . '" && v:val.kind =~ "[asf]"'
    if empty(a:base)
       let tags = filter(deepcopy(s:analysed_tags), rx) 
    else
       let rx   = '!empty(v:val.name) && v:val.name =~# "^' . a:base . '" && ' . rx
       let tags = filter(deepcopy(s:analysed_tags), rx) 
    endif
    " Make menu pop_up ...
    for tag in tags
        " The tag name i.e. menu pop up
        let pop_up = tag.name
        " Add the signature to the tag name
        let signature = matchstr(tag.cmd, '^/\^   \<' . a:base . '\w*\>\zs(\S\{-\})\ze')
        if !empty(signature)
           let tag.name = pop_up . signature . ' ' . tag.kind
        else
           let tag.name = pop_up  . ' ' . tag.kind
        endif
    endfor
    " Any tags?
    if !empty(tags)
       " Make the first menu item 
       let tag = deepcopy(tags[0])
       let tag.name = a:base
       let tag.kind = ''
       " Prepend the first item ...
       let tags = [tag] + tags
    endif
    return tags
endf
