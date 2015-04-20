#!/bin/bash -e

TEST_FILE=/tmp/py-test-file.py


install_emacs24() {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y
    sudo apt-get install emacs24 -y
}


test_01() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests/tests.el \
          --load py-autopep8.el \
          ./tests/01/before.py \
          -f py-autopep8-buffer \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/01/after.py
}


test_02() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests/tests.el \
          --load ./tests/02/init.el  \
          --load py-autopep8.el \
          ./tests/02/before.py \
          -f py-autopep8-buffer \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/02/after.py
}


test_03() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests/tests.el \
          --load ./tests/03/init.el  \
          --load py-autopep8.el \
          ./tests/03/before.py \
          -f py-autopep8-buffer \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/03/after.py
}


test_04() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests/tests.el \
          --load py-autopep8.el \
          ./tests/04/before.py \
          -f py-autopep8-buffer \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/04/after.py
}


test_05() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests/tests.el \
          --load ./tests/05/init.el \
          --load py-autopep8.el \
          ./tests/05/before.py \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/05/after.py
}


test_install_package() {
    emacs --no-init-file -nw \
          py-autopep8.el \
          -f package-install-from-buffer \
          -f kill-emacs
}


main() {
    if [ "$TRAVIS" = "true" ]; then
        install_emacs24
        test_install_package
    fi

    test_01
    test_02
    test_03
    test_04
    test_05
}


main
