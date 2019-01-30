# Lisp Setup Instructions

## Install SBCL

For quick running, you can install SBCL from `apt` (Linux) or `brew`
(Mac OS X). A Windows (32- or 64-bit) binary installer can be found in
the table from [this page](http://www.sbcl.org/platform-table.html).

Despite the ease of the above, it's *not* the preferred way to acquire
SBCL. *In fact, some components of the QVM project depend on an
up-to-date SBCL.* The fastest way to get this is to download the
latest binary release from [this page](http://www.sbcl.org/platform-table.html).

**N.B.**: Make sure to get a binary for your platform's processor
architecture, probably AMD64.

**N.B.**: The Windows and Linux binaries are usually up-to-date, but
the Mac ones seem to sometimes lag behind. This is unfortunate,
because you need a relatively up-to-date SBCL to build the QVM (at
least 1.3.6 is known to work). If your platform is not up-to-date, you
will need to install the binaries, and then do an update from the
source release, as described in the next paragraph.

For someone who intends to hack on the QVM project, the preferred way
is to install from source code. This will give you the full compiler
documentation, as well as the full source code. It also allows for
pretty rapid upgrading. First, install the binaries from [this
page](http://www.sbcl.org/platform-table.html), selecting your
platform appropriately. Installation instructions come with the
distribution in an `INSTALL` file. After installing, download the
source code, either from a stable source release, or bleeding-edge
from `git`. The source release can be found on the same page.

The long-story-short of the `INSTALL` file is this:

1. Make sure some version of SBCL is installed.

2. Run `sh make.sh --fancy`.

3. Run `sh install.sh`.

Make sure that `sbcl` is in your path.

### Readline Support

Editing in the SBCL REPL (command loop) can be a pain, because you
can't arrow around. If you spend a lot of time in the SBCL REPL (you
shouldn't, spend your time in Emacs+SLIME), then you might consider
installing `rlwrap`. You can install this from your package
manager. Then just execute `rlwrap sbcl`, and everything will be good.

## Install Quicklisp

Quicklisp is like Python's `pip` for Lisp. 

### Automated Instructions

Change to the `qvm` directory and type

```
make quicklisp
```

This should download and install Quicklisp.

### Manual Instructions

If the automated instructions didn't work or you'd like to do it
manually for an unsupported platform, then do the following.

#### Step 1: Downloading Quicklisp

Follow the instructions on the [Quicklisp
website](https://www.quicklisp.org/beta/) for details. The summary is:

1. Download `quicklisp.lisp`.

2. Run `sbcl` and `(load "quicklisp.lisp")`.

3. Run `(quicklisp-quickstart:install)`.

4. Run `(ql:add-to-init-file)` and press enter.

#### Step 2: Updating Quicklisp

Quicklisp can be periodically updated. Both the Quicklisp client, as
well as all of the Quicklisp software, can be updated. The two
respectively can be done via:

1. Update QL client: `(ql:update-client)`

2. Update all QL software: `(ql:update-all-dists)`

**N.B.**: Once you have installed all of the Rigetti source code (QVM,
ALEXA, ...), you can update by just doing `make deps`.

#### Step 3: Telling Quicklisp Where Your Code Is

In order to be able to simply do `(ql:quickload :qvm)` as a one-liner,
you need to tell Quicklisp where to find your code. Append to your
`.sbclrc` file the following snippet, replacing `/path/to/code/dir/`
with the path which contains the `qvm` directory.

```
#+quicklisp
(progn
(push "/path/to/code/dir/"
      ql:*local-project-directories*)
)
```

## Install Buildapp

Lisp is an *image-based language*. This means that almost all
development is a stateful modification of the Lisp universe, called
the *image*. When you want to turn your program into a binary
executable, you have to *deliver the image*. Image delivery is best
supported by commercial Lisp implementations such as LispWorks (which
has all sorts of options to reduce the binary size, to optimize
things, etc.), but it is nonetheless supported by SBCL.

The easiest way to do image delivery with SBCL is through a companion
program called *buildapp*. Installation is easy. Download buildapp
from [here](http://www.xach.com/lisp/buildapp.tgz). Extract it and
`cd` into it. Simply do:

1. `make`

2. `make DESTDIR=/where/to/install install`. If you omit the `DESTDIR`
   setting, it'll go to `/usr/local/bin`.

## (Optional) Install Emacs and SLIME

Lisp is most optimally edited in Emacs. Emacs, paired with an
extension called SLIME, provides full IDE capabilities. Emacs 24 can
be installed from your favorite package manager, and SLIME can be
installed by starting `sbcl` and running `(ql:quickload
:quicklisp-slime-helper)`. It will give you some stuff that you need
to put into your `~/.emacs` file.

After installing SLIME, and after adding the `(load (expand-file-name
...))` form SLIME asks you to add, add the following configuration to
your `.emacs` file. This will make everything good.

```
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
	  (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))
  ;; Using local-set-key in a mode-hook is a better idea.
  (global-set-key (kbd "RET") 'electrify-return-if-match)

(slime-setup '(slime-fancy
               slime-autodoc
               slime-indentation))

(setq slime-net-coding-system 'utf-8-unix
      slime-truncate-lines nil
      slime-multiprocessing t)

(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment t)
```

### Paredit

Oh dear, all the parentheses! The path to Lisp enlightenment requires
an understanding that the parentheses are there because Lisp code is
actually one giant (semi-)serialized data structure, and parentheses
provide literal syntax for that data structure.

Structural editing of this data structure makes life easier, and this
is done with Paredit. The basic premise of Paredit is that you're not
editing source code as text, but rather source code as
S-expressions. This way, all of your editing operations will only
allow for structurally correct Lisp code.

Paredit can be downloaded
[here](http://mumble.net/~campbell/emacs/paredit.el), and instructions
for installing are in the same file.