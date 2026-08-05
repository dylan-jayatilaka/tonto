# Editing Tonto with vim

Migrated from the project wiki (2026-08-05).

Your text editor is a personal choice. If you use `vim`
you might want to install

* `universal ctags (u-tags)`
* `cscope`
* `gnuplot`

With these and `vim` you can syntax highlight, jump to
tags under your cursor (e.g. routines, types, or macros)

You can also have Tonto library specific code completion.


## Jumping around in code with `vim`

Tonto is a big library, with many deeply nested modules.

Even though Tonto is well structured and commented, 
to browse it effectively I use the `vim` editor: syntax
highlighting has been set up, as well as completion of routine
names in the library. 

To get started with `vim` copy or link the supplied `.vimrc` and
`.vim` directories in the Tonto root directory to your home directory,
as shown below. Backup or merge with your own files if needed.

```
   cp .vimrc ~
   cp -r .vim ~
   cp -r .ctags.d ~
```

Then start up `vim` on one of the .foo files in `foofiles/` from the Tonto
root directory, say

```
   vim buffer.foo
```

Here are some key points:

* To unfold routines type `zo`. 

* To close or fold a routine type `zc`.

* To jump to different routines while the cursor is placed on top of
  the routine name run the script

```
   ../scripts/cscope_setup
```

  Then, while editing a file `foofiles/<some-file>.foo` with `vim`
  from the Tonto **root directory** type:

```
   g Ctrl-]
```

   when the cursor is on top of the routine name or type component you
   are interested in. If there are multiple routines with the same name,
   choose the one you really want to see. Otherwise you will jump
   straight to the definition of that routine.

To jump back to where you were, Type "Ctrl-t". Wonderful!

## Code completion

Finally, for code completion. This is cool.

When editing a .foo file with vim, set up .ctags as above and type:

```
   :FooCompleteOn
```

Now try adding a dot after a variable, and while still in insert
mode, after the dot type

```
   Ctrl-X Ctrl-O
```

You will see a pop-up menu with the available object methods. Type a
few more characters to narrow down the choice. The method is listed
as a type-component/attribute (a), subroutine (s), function (f),
type-definition (t), macro (m), or global variable (g).  For more
details type

```
   :help completion
```

and look at the omni completion documentation.