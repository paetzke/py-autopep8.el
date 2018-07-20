#!/bin/bash -e

TEST_FILE=/tmp/py-test-file.py

# Run the test:
# - First argument is test folder
# - Second argument defines whether or not
# py-autopep8-buffer should be called
test_autopep() {
  echo $FUNCNAME $1
  rm -f $TEST_FILE

	if [[ -f ./tests/$1/init.el ]]; then
		load_init_file="--load ./tests/$1/init.el"
	fi

	if [ "$2" = true ]; then
		call_autopep8="-f py-autopep8-buffer"
	fi

  emacs --batch \
        --load py-autopep8.el \
        $load_init_file \
        ./tests/$1/before.py \
        $call_autopep8 \
        --eval "(write-file \"$TEST_FILE\")" \

  diff $TEST_FILE ./tests/$1/after.py
}



# The package won't actually be installed
# in the user distribution because we are using --batch
test_install_package() {
  emacs --batch py-autopep8.el -f package-install-from-buffer
}


main() {
  test_install_package

	for i in {1..4}; do
		test_autopep 0$i true
	done

	test_autopep 0$i false
}


main
