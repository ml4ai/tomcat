# LISP Notes

* Author: Clayton T. Morrison
* Created: 2019-05-26
* Last update: 2019-07-24



## Get a Lisp

There are several very good free, open-source Common Lisp implementations. Clay is most familiar (for modest values of familiar) Clozure Common Lisp (CCL), so the focus here will be CCL, but I'll also note SBCL. It is safe to have both on your machine.

### Steel Bank Common Lisp (SBCL)
[http://www.sbcl.org/](http://www.sbcl.org/)

You will need SBCL to build the local Shop3 documentation (although Clay is still working on getting local doc build to work).

#### Intall:
```
$ brew install sbcl
```

### Clozure Common Lisp (CCL)

NOTE: It is NOT recommended that you install via Homebrew/MacPorts. 
Instead, use the following instructions.

#### Install:

Further build instructions: [https://github.com/Clozure/ccl/issues/150](https://github.com/Clozure/ccl/issues/150)

##### (1) Get CCL

NOTE: As of 2019-05, a specific CCL release is recommended:<br>
`https://github.com/Clozure/ccl/releases/tag/v1.12-dev.5`

NOTE: In the following, I'm cloning CCL as `ccl-dev`

```
$ git clone https://github.com/Clozure/ccl.git ccl-dev
$ curl -L -O https://github.com/Clozure/ccl/releases/download/v1.12-dev.5/darwinx86.tar.gz
$ cd ccl-dev
$ tar xf ../darwinx86.tar.gz
```

##### (2) Rebuild the CCL Lisp Image

In CCL, you rebuild the lisp image from within CCL itself.

There are a couple of ways to launch CCL (both of the following assume you're in the ccl root (e.g., `ccl-dev/`) directory):

1. To launch the image directly: `$ ./dx86cl64`

2. To launch from the scripts directory:

	```
	$ cd scripts
	$ ./ccl64  # to launch CCL
	```

Rebuild the lisp kernel (takes about 30 sec).
After launching the CCL lisp image, evaluate the following 
('`?`' indicates the lisp REPL prompt) :

```
? (rebuild-ccl :full t)
```

> Optionally build the cocoa application.
This step is not necessary
> ```
? (require 'cocoa-application)
```

##### (3) Move CCL to `/usr/local/src`

After successfully rebuilding the lisp kernel, I then *copied* the CCL `ccl-dev` directory to: `/usr/local/src/ccl`.<br>
I had to create `/usr/local/src/`, including: sudo chown to my username, sudo chgrp to admin:

```
$ sudo mkdir /usr/local/src/ccl
$ sudo chown claytonm /usr/local/src/ccl
$ sudo chgrp admin /usr/local/src/ccl
$ cp -R <path-to-repo>/lisp/ccl-dev /usr/local/src/ccl
```

##### (4) Add ccl/scripts to PATH

Add to PATH in `~/.bash_profile`:

```
export PATH=$PATH":/usr/local/src/ccl/scripts"
```

After source'ing `~/.bash_profile` or starting a new shell, you should be able to access `ccl64` from the command line:

```
$ which ccl64
/usr/local/src/ccl/scripts/ccl64
```



## Quicklisp

Quicklisp is a lisp package manager.<br>
[https://www.quicklisp.org/](https://www.quicklisp.org/)

### Install

Get quicklisp:

```
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
```

Next, launch CCL in the location where you would like your quicklisp repository to be maintained. You specify the path of the quicklisp repo root by the `:path` argument.
(I chose to launch ccl within my `<repo-root>/lisp/` directory, so I only needed to add the `quicklisp` root dir (the name I chose) as the `:path` argument.)
Evaluate the following within CCL:

```
? (load "<path_to>quicklisp.lisp")
? (quicklisp-quickstart:install :path "quicklisp")
? (ql:add-to-init-file)  # adds lines to ~/.ccl-init.lisp
```

NOTE: Most lisps use some sort of init file facility in order to permit configuration and pre-loading. For CCL, one of these files goes in: `~.ccl-init.lisp`. The last line of the commands above adds a form to `~.ccl-init.lisp` so that quicklisp will automatically be loaded (it's very lightweight) at CCL startup.



## Lisp Editor

A good editor is essential for lisp dev. It should grok paren matching and support canonical lisp formatting ([http://wiki.c2.com/?LispIndentation](http://wiki.c2.com/?LispIndentation)).

It is also good to have direct integration with a Lisp REPL with backtrace navigation support. One of the most widely used general Lisp integration is the Superior Lisp Interaction Mode for Emacs (SLIME: [https://common-lisp.net/project/slime/](https://common-lisp.net/project/slime/)). 

Some helpful notes about SLIME use:<br>
[https://common-lisp.net/project/slime/doc/html/Evaluation.html](https://common-lisp.net/project/slime/doc/html/Evaluation.html)

The focus here will be on setting up Emacs with SLIME. 

NOTE: I did find there is a rework of slime for Atom, although I have not yet tested it.<br>
[http://web.mit.edu/sjlevine/www/projects/atom-slime/](http://web.mit.edu/sjlevine/www/projects/atom-slime/)


### Emacs

If using a mac, highly recommend using Aquamacs.

#### GNU Emacs

[https://www.gnu.org/software/emacs/](https://www.gnu.org/software/emacs/)

Although there is a default Emacs on Mac, I recommend installing the latest.<br>
Install: `$ brew install emacs`

#### Aquamacs

[http://aquamacs.org/download.shtml](http://aquamacs.org/download.shtml)

Setting up Aquamacs theme:<br>
[https://stackoverflow.com/questions/21988671/how-to-make-aquamacs-color-theme-stick](https://stackoverflow.com/questions/21988671/how-to-make-aquamacs-color-theme-stick)<br>
I use (for now) `subtle-hacker` (although not keen on function name color...).<br>
Aquamacs preferences, where things like theme are set, lives in<br>
`~/Library/Preferences/Aquamacs Emacs/Preferences.el`

#### Set up SLIME for Emacs

[https://gist.github.com/BlameOmar/9477900](https://gist.github.com/BlameOmar/9477900)

If you have already installed quicklisp, then you just need to evaluate the following in ccl:

```
? (ql:quickload "quicklisp-slime-helper")
```

At the end of installing quicklisp-slime-helper, it will display a message explaining what to put in your `~/.emacs`,
except this should go where Aquamacs puts such things:

```
~/.emacs.d/init.el
```

In my setup, I've added the following two lines to `~/.emacs.d/init.el`:

```
(setq inferior-lisp-program "/usr/local/src/ccl/scripts/ccl64")
(load (expand-file-name "~/Documents/repository/lisp/quicklisp/slime-helper.el"))
```

##### Optional: Emacs MELPA repo

There is a commonly used Emacs package manager called MELPA ([https://melpa.org/#/](https://melpa.org/#/)).

I had to work a bit to get this to work with Aquamacs, including working with Win from the Aquamacs dev group; here is the issue I followed that provided insight:<br>
[https://github.com/davidswelt/aquamacs-emacs/issues/133](https://github.com/davidswelt/aquamacs-emacs/issues/133)

Here's what I did:

(1) `$ brew install gnutls`

(2) Add the following to Aquamacs Preferences.el in 
`~/Library/Preferences/Aquamacs Emacs/` :

```
(setenv "PATH" (concat "~/bin.sh:/usr/local/bin:" (getenv "PATH")))
```

(3) Then you need to add some additional things to `~/.emacs.d/init.el` -- My current `~/.emacs.d/init.el` looks like this::

```
(message ">> init.el file load START!")

(require 'tls)
(with-eval-after-load 'tls
     (push "/private/etc/ssl/cert.pem" gnutls-trustfiles))
(setq tls-checktrust 'ask)

(setq tls-program
      '("gnutls-cli -p %p --dh-bits=2048 --ocsp --x509cafile=%t --insecure --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM' %h"))

(message ">>>> tls-program: '%s'" tls-program)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;;;; slime
(setq inferior-lisp-program "/usr/local/src/ccl/scripts/ccl64")
(load (expand-file-name "~/Documents/repository/lisp/quicklisp/slime-helper.el"))


(message ">> init.el file load DONE!")
```

(4) Finally, refresh the MELPA repo: Launch Aquamacs and execute the following (where `M-x` is "Meta-x" in Emacs parlance; in Aquamacs, the "Meta" key is bound to the `control` key; in gnu emacs (e.g., from the terminal), "Meta" is bound to. the `Esc` key):

```
M-x package-refresh-contents
```



## SHOP3

SHOP3 is the most actively developed version of the SHOP (Simple Hierarchical Ordered Planner).

- Github: [https://github.com/shop-planner/shop3](https://github.com/shop-planner/shop3)
- SHOP3 Manual: [https://shop-planner.github.io/](https://shop-planner.github.io/)

SHOP3 depends on asdf ([https://common-lisp.net/project/asdf/](https://common-lisp.net/project/asdf/)).<br> 
In particular, fiveam in the asdf contrib: `asdf/contrib/fiveam-asdf`):<br>
[https://gitlab.common-lisp.net/asdf/asdf](https://gitlab.common-lisp.net/asdf/asdf)


### Install

SHOP3 works with quicklisp (recommended), so clone into your quicklisp repo:

```
$ cd to <quicklisp-root>/local-projects/
$ git clone https://github.com/shop-planner/shop3.git
$ git clone https://gitlab.common-lisp.net/asdf/asdf.git
```

Then git checkout all of the SHOP3 submodules, which will be placed in `shop3/jenkins/ext/`. In the root of shop3, run:

```
$ git submodule update --init --recursive
```

Then, in CCL:

```
? (ql:quickload "shop3")
```


### Building local SHOP3 docs (Does not yet work...)

NOTE: Building Shop3 local docs<br>
You will need to install SBCL to build the Shop3 docs.<br>
You will also need to configure ASDF (the lisp build framework).

NOTES so far on attempting to build docs:

```
(1) Install sbcl -- see above
(2) Put the following in <quicklisp>/local-projects/
- cl-dot : https://github.com/michaelw/cl-dot<br>
git clone https://github.com/michaelw/cl-dot.git

(3) Add paths to the root of each of those checkouts to 
        asdf:*central-registry*
    of ~/.sbclrc (you may need to create this file)
    Here's what my ~/.xbclrc currently contains:

========== START ~/.sbclrc ==========
(print ">>> Loading ~/.sbclrc")

(require "asdf")
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/shop3/shop3/" asdf:*central-registry*)
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/asdf/contrib/fiveam-asdf/" asdf:*central-registry*)
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/shop3/jenkins/ext/iterate/" asdf:*central-registry*)
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/shop3/jenkins/ext/trivial-garbage/" asdf:*central-registry*)
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/shop3/jenkins/ext/alexandria/" asdf:*central-registry*)
(push "/Users/claytonm/Documents/repository/lisp/quicklisp/local-projects/cl-dot/" asdf:*central-registry*)

(print ">>> DONE loading ~/.sbclrc")
========== END ~/.sbclrc ==========
```

... That's as far as I've gotten; `$ shop3/docs/make` currently fails with bunch of doc processing errors.

Started email convo with Robert Goldman @SIFT on 2019-07-20


## Common Foreign Function Interface (CFFI)

[https://common-lisp.net/project/cffi/manual/cffi-manual.html](https://common-lisp.net/project/cffi/manual/cffi-manual.html)