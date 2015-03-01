#! /bin/bash


install_emacs24() {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y

    sudo apt-get install emacs24 -y
}


on_error() {
    local msg=$1

    echo $msg
    exit 1
}


test_01() {
    emacs --no-init-file -nw \
          --load py-autopep8.el \
          ./test_data/test_01/before.py \
          -f py-autopep8-before-save \
          -f save-buffer \
          -f save-buffers-kill-terminal

    diff ./test_data/test_01/before.py ./test_data/test_01/after.py
    if [ $? != 0 ]; then
        on_error "test_01"
    fi
}


test_02() {
    emacs --no-init-file -nw \
          --load ./test_data/test_02/init.el  \
          --load py-autopep8.el \
          ./test_data/test_02/before.py \
          -f py-autopep8-before-save \
          -f save-buffer \
          -f save-buffers-kill-terminal

    diff ./test_data/test_02/before.py ./test_data/test_02/after.py
    if [ $? != 0 ]; then
        on_error "test_02"
    fi
}


test_03() {
    emacs --no-init-file -nw \
          --load ./test_data/test_03/init.el  \
          --load py-autopep8.el \
          ./test_data/test_03/before.py \
          -f py-autopep8-before-save \
          -f save-buffer \
          -f save-buffers-kill-terminal

    diff ./test_data/test_03/before.py ./test_data/test_03/after.py
    if [ $? != 0 ]; then
        on_error "test_03"
    fi
}


test_install_package() {
    emacs -nw py-autopep8.el -f package-install-from-buffer -f kill-emacs
}


main() {
    if [ "$TRAVIS" = "true" ]; then
        install_emacs24
    fi

    test_01
    test_02
    test_03

    if [ "$TRAVIS" = "true" ]; then
        test_install_package
    fi
}


main
