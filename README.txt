To install, first put the dodemacs directory from this repo in your home directory.

Next, to get some python stuff working, install virtualenv into your python and then:

    $ cd ~/.emacs.d
    $ cd usr
    $ source bin/activate
    $ cd src

now install all four python packages into the virtualenv by cd'ing into 
each of them and doing python setup.py install

For go development I use one $GOPATH for everything and install these tools:

    $ go get -u -v github.com/nsf/gocode
    $ go get -u github.com/rogpeppe/godef
    $ go get -u golang.org/x/tools/cmd/goimports
    $ go get -u golang.org/x/tools/oracle
    $ sudo mv $GOPATH/bin/oracle $GOROOT/bin/
    $ go get -u github.com/dougm/goflymake

Then run the following (put cursor at end of each block in *scratch* and C-j it).

    (setq package-list '(go-mode
                         auto-complete
                         flycheck
                         flymake-cursor
                         projectile
                         magit
                         magit-gh-pulls
                         go-projectile
                         swiper
                         org-bullets
                         ox-reveal
                         multiple-cursors))

    ; list the repositories containing them
    (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                             ("gnu" . "http://elpa.gnu.org/packages/")
                             ("melpa" . "http://melpa.org/packages/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")))

    ; activate all the packages (in particular autoloads)
    (package-initialize)

    ; fetch the list of packages available 
    (unless package-archive-contents
      (package-refresh-contents))

    ; install the missing packages
    (dolist (package package-list)
      (unless (package-installed-p package)
        (package-install package)))

Install mu4e:

    $ yaourt -S mu (or apt-get install mu, or brew install --with-emacs mu)

Then set up your .offlineimaprc file, then:

    $ brew install gnutls
    $ offlineimap
    $ mu index

Homebrew mu seems broken at the moment.  After installing mu with
the --with-emacs the /usr/local/Cellar/mu/0.9.18_1/share/ directory
was empty (didn't include emacs/site-lisp).

I had to:

    $ cd /tmp
    $ cp /Users/craig/Library/Caches/Homebrew/mu--gmime-2.6.23.tar.xz .
    $ tar xf mu--gmime-2.6.23.tar.xz
    $ cd mu--gmime-2.6.23
    $ ./configure --prefix=/usr/local/Cellar/mu/HEAD-cb0025b_1/gmime --disable-introspection
    $ make install
    $ autoreconf -ivf #dunno what this is
    $ cd ..
    $ cp /Users/craig/Library/Caches/Homebrew/mu-0.9.18.tar.gz .
    $ tar xf mu-0.9.18.tar.gz
    $ cd mu-0.9.18
    $ EMACS=/usr/local/Cellar/emacs/25.3/bin/emacs ./configure --prefix=/usr/local/Cellar/mu/0.9.18_1 --with-lispdir=/usr/local/Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu
    $ make install
    $ cd /usr/local/share
    $ ln -s ../Cellar/mu/0.9.18_1/bin/mu .
    $ cd emacs site-lisp
    $ ln -s ../../../Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu .
    