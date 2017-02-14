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