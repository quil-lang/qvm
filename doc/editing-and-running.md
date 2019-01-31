# A Primer on Editing & Running Lisp

Perhaps the most unfortunate thing about Lisp is that to *properly*
interact with Lisp code, there's a steep learning curve. Without
purchasing expensive tools, the typical way to both *edit* and
*interact* with Lisp is to use Emacs and a package within Emacs called
[SLIME](https://common-lisp.net/project/slime/). While its webpage
looks scrappy, and it's based on Emacs, it's one of the most powerful
IDEs in the world.

In the following sections, I will assume you've followed the [Lisp
setup](lisp-setup.md) instructions to completion, including the
installation of Paredit. I will go from "simplest" to "most difficult"
in terms of editing and running Lisp.


## Running Lisp like Python

The most bare-bones way to write and run Lisp code doesn't actually
require any special tools. You can open a text editor, write Lisp
code, and run it at the command line. Consider the file `hello.lisp`
which contains only one line:

```commonlisp
(write-line "Hello, world!")
```

To run this, simply do:

```
$ sbcl --script hello.lisp
Hello, world!
```

If writing `--script` is annoying to you, consider adding the
following alias to your `.bashrc` file:

```bash
alias runlisp="sbcl --script"
```

With this, you can write:

```
$ runlisp hello.lisp
```

Lisp files don't have any sort of "main" function, however, it's
preferable to make one. We can rewrite `hello.lisp` so that it is more
encapsulated.

```commonlisp
(defun main ()
  (write-line "Hello, world!"))

;;; Entry point
(main)
```

This is okay for the most basic computations, however, it won't allow
you to easily load libraries, since your `.sbclrc` is not being
loaded. (Feel free to load it manually, though, with `(load
"~/.sbclrc")` at the top of your script.)


## Some Lisp Nomenclature

Lisp has special nomenclature for various ideas.

A **library** is an anthropic concept meaning some useful
functionality provided by a set of systems. Lisp itself doesn't use
the word "library" for anything.

A **system** is a collection of files that make up a coherent piece of
a library. Many libraries contain two systems: the main system and the
test system. The systems themselves are defined by `.asd` files, each
of which specify which files are a part of the system and the order in
which they should be compiled and loaded. Systems are processed by a
library called [ASDF](https://common-lisp.net/project/asdf/) ("Another
System Definition Facility").

Systems are where dependencies are specified. If your system depends
on another system, it is declared in the `.asd` file. Most Lisp code
doesn't have the Python equivalent of `import`. You specify the
dependencies you need in your `.asd` file and you can readily use them
in your own system.

A **package** is a Lisp entity that represents a collection of
names. The package doesn't know what the names actually represent. In
Lisp, a "name" is called a **symbol**. Symbols may be **external** if
they were **exported** by the package file. You do *not* export
functions, you export symbols. This is not unlike math or physics: The
letter *F* may name a function, or may name a scalar value. The letter
*F* could possibly even represent both at different times, although
that could be confusing.

Packages provide *namespaces* for names. External symbols are
accessible by other packages. Internal symbols are also accessible
with a special syntax. Namespaces ultimately are just to help you give
identifying names to things.

Systems may have many packages. Sometimes people like to make a lot of
packages for their systems. Sometimes people even make one package for
every file.

**Question**: If I want to use the `CL-QUIL` package from my package
called `MY-PKG`, how do I do it?

**Answer**: Packages don't specify dependencies, systems do. So
whatever system `MY-PKG` is a part of, say `MY-SYS`, add the
dependency of the system that the package `CL-QUIL` is a part of. (It
just so happens that the package `CL-QUIL` is a part of a system that
is also called `CL-QUIL`.)

**Question**: I'm building a server and want to import the Hunchentoot
library at the top of my file. How do I do that?

**Answer**: You don't import libraries like that. You specify the
system `HUNCHENTOOT` as a dependency to your system's `.asd` file.

**Question**: I made a main system called `MY-SYS` and I also made a
test system called `MY-SYS-TESTS`. I have packages of the same name in
these systems. I want to test a function in `MY-SYS` called
`foo-internal`, but I didn't export it. How do I access it from my
testing package?

**Answer**: If the *package* `MY-SYS` has used the symbol
`foo-internal`, and it has not been exported, then you can use it from
other packages like `MY-SYS-TESTS` by using the special **double
colon** syntax: `my-sys::foo-internal`. Double colon syntax gives you
access to every symbol in every package, but should only be used for
very special cases, like when you're testing internals of one package
from another package.


## Starting a New Lisp Project

You can create a project skeleton, with a system definition and a
package definition by using a library called `quickproject`. Let's say
we want to make a project called `my-proj` in the directory
`~/Scratch/`:

```
$ rlwrap sbcl
[...]
* (ql:quickload :quickproject)
[...]
* (quickproject:make-project "~/Scratch/my-proj/")
"my-proj"
```

Now you'll have in the directory `~/Scratch/my-proj` everything you
need for a Lisp project:

```shell
$ ls -1 ~/Scratch/my-proj/
README.txt     # A project README
my-proj.asd    # The system definition -- add dependencies here!
my-proj.lisp   # A single "main" file
package.lisp   # The package definition -- export symbols here!
```

By default, the system name will be `MY-PROJ` *and* the package name
will be `MY-PROJ`.

If you've made this project in a directory that Lisp knows about, then
you can load it in SBCL with:

```
* (ql:quickload :my-proj)
```

Lisp (specifically Quicklisp and ASDF) knows where to look by what's
in your `.sbclrc` file. In particular, you can add directories to
search for by adding

```
(push "/path/to/include/" ql:*local-project-directories*)
```

to your `.sbclrc`.


## Editing Code with Emacs and SLIME

Now we will get into the necessary ideas to write code in Emacs and
SLIME.

### Some useful Emacs commands

Commands in Emacs are written with a special shorthand syntax. `C-x`
means type "Control" and "x" at the same time. `M-x` means type "Alt"
and "x" at the same time. It's `M` because it used to be called the
"Meta" key. `C-a C-b` is the same as `C-a-b`. On the other hand, `C-a b`
means type "Control" and "a" at the same time, release, then type
"b". `RET` means "press enter".

Here are all the commands you need to know:

#### Basic commands

These are basic editing commands in Emacs.

`C-g`: Quit/abort whatever command is being done. **Remember this!**

`C-x-f <filename> RET`: Open a file. When you're prompted for the
filename, you can tab complete and everything.

`C-x-s`: Save file.

`C-x k RET`: Close file or buffer.

`C-x o`: Move cursor to a different, **o**ther pane.

`M-w`: Copy.

`C-w`: Cut.

`C-y`: Paste.

`C-x 1`: Collapse all other panes except the one that the cursor is
in.

`C-x 2`: Split horizontally.

`C-x 3`: Split vertically.

`C-x 0`: Collapse *just* the pane that the cursor is in.


#### Slime commands

These are basic commands to let you interact with and run your Lisp
code.

`M-x slime RET`: Start SLIME and open a Lisp REPL.

`M-p`: (REPL) Previous command.

`M-n`: (REPL) Next command.

`C-c-c`: (File) When your cursor is over a function or class, compile
and load just that function or class into the REPL.

`C-c-l`: (File) Load the entire file into the REPL.

`M-.`: (REPL or File) When the cursor is over a symbol, jump to the
definition of this symbol. This is like clicking a link in your web
browser.

`M-,`: (REPL or File) Go back to where you where. This is like hitting
the back button in your browser.


#### Paredit commands

These are basic commands for editing parenthetical expressions.

`M-s`: When your cursor is in pair of parentheses, "splice" them,
deleting a pair of parentheses.

`C-<right>`: Slurp up the next object in the pair of parentheses your
cursor is in. For example, suppose your cursor is located at the `|`
symbol.

    Before: (1 (2| 3) 4)
    After:  (1 (2| 3 4))

`C-<left>`: Barf out the last object in parentheses. Does the opposite
of slurping.

`(`: Open a *pair* of parentheses.

`)`: Go to the nearest closing parenthesis after the cursor.


### The Typical Workflow

Here is the typical workflow for editing Lisp code. Here, we'll add a
new function to `MY-PROJ`.

1. Open your Lisp files in Emacs you're interested in working
on. Let's open `~/Scratch/my-proj/my-proj.lisp`. Use `C-x-f` to find
and open this file.

2. Open SLIME. You only need to do this once. Type `M-x slime
RET`. This will pop open a new pane with a REPL that says `CL-USER>`.

3. Load your system with `(ql:quickload :my-proj)`. If there was an
error, it probably means you mistyped *or* you haven't yet told Lisp
where to look. You can get out of the error by clicking on the
appropriate option in the pane that pops up. You can always tell Lisp
where a single `.asd` file is by doing

```
(load "/path/to/my-proj/my-proj.asd")
```

and then do the `quickload` again.

4. Type a function in `my-proj.lisp`. If your cursor is in a different
pane, use `C-x o` to switch to the other pane. Let's do the function

```commonlisp
(defun say-hello (name)
  "Say hello to NAME."
  (check-type name string)
  (format t "Hello, ~A!~%" name))
```

5. Put your cursor anywhere in the function and type `C-c-c`. You
should see `; compiling (DEFUN SAY-HELLO ...)` pop up in the REPL. If
so, you've loaded some Lisp code!

6. Save the file (though that's not necessary) with `C-x-s`. Go into
the REPL. You're in the `CL-USER` namespace. Switch to `MY-PROJ` first
by doing

```
(in-package :my-proj)
```

and you should see `CL-USER>` change to `MY-PROJ>`.

7. Test your function! Try calling it with a name.

```commonlisp
MY-PROJ> (say-hello "Robert")
Hello, Robert!
NIL
```

The `NIL` means the function returned `NIL`.

8. Go back to your file, and change the string to say `Hi` instead of
`Hello`. Recompile that function by doing `C-c-c` again. Save it too.

9. Go back to your REPL and go to the previous entry by typing `M-p`,
and press enter. It should now have different output.

```commonlisp
MY-PROJ> (say-hello "Robert")
Hi, Robert!
NIL
```

Lisp code is edited and interactively tested like this. Very rarely do
you need to reload an entire project or anything like that. It's
almost like a Jupyter notebook, but in your editor and allows you to
open as many files as you please.

### Going Beyond

SLIME is a full IDE. It allows you to inspect, debug, view
callers/callees, trace, etc. Emacs is also a very powerful,
programmable text editor, whose benefits I'll describe another
time. One of the big benefits of Lisp as a programming language is its
unparalleled interactivity. You feel "closer" to your program. In C or
Python, with the whole "edit -> compile -> run" cycle, you feel
divorced, and it's difficult to test and debug smaller features. In
Lisp, you're dynamically switching between editing and execution,
quite seamlessly.
