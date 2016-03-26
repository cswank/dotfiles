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
    $ go get github.com/rogpeppe/godef
    $ go get golang.org/x/tools/cmd/goimports
    $ go get golang.org/x/tools/oracle
    $ sudo mv $GOPATH/bin/oracle $GOROOT/bin/
    $ go get -u github.com/dougm/goflymake

And, in your emacs M-x package-install flycheck
