#! /bin/bash

function install_emacs24 {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y

    sudo apt-get install emacs24 -y
}


function install_package {
    emacs -nw py-autopep8.el -f package-install-from-buffer -f kill-emacs
}


function test_01 {
    emacs -nw ./test_data/test_01_before.py -f py-autopep8-before-save -f save-buffer -f save-buffers-kill-terminal

    diff ./test_data/test_01_before.py ./test_data/test_01_after.py
    if [ $? != 0 ]; then
        exit 1
    fi
}


function main {
    if [ "$TRAVIS" = "true" ]; then
        install_emacs24
    fi

    install_package

    test_01
}


main
