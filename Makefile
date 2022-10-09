help:
	@echo "Use a command!"

sbcl:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu sbcl
else ifneq ($(shell command -v pacman),)
    sudo pacman -Sy sbcl
else ifneq ($(shell command -v dnf),)
    sudo dnf install -y sbcl
else ifneq ($(shell command -v apt),)
    sudo apt install -y sbcl
else ifneq ($(shell command -v brew),)
	brew install sbcl
else ifneq ($(shell command -v sbcl),)
	@echo "SBCL found!"
else
	@echo "Could not determine steps to install SBCL! Please install SBCL and try again."
endif

quicklisp:
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --load /tmp/quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"
	sbcl --load ~/quicklisp/setup.lisp --non-interactive --eval "(ql:add-to-init-file)"

binary:
	@echo "Generating binary"
	sbcl --non-interactive --load build.lisp

setup: sbcl quicklisp binary
	@echo "power-guard is now installed."
