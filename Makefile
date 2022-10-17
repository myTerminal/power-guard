SHELL = /bin/sh

ifeq ($(PREFIX),)
    PREFIX := /usr/local
endif
MANPREFIX := $(PREFIX)/share/man
QUICKLISP_DIR := ~/quicklisp

help:
	@echo "Use one of the following options:"
	@echo " - install"
	@echo " - uninstall"
	@echo " - reinstall"
	@echo " - update"

primary-deps:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v sbcl),)
	@echo "SBCL found."
else ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu sbcl
else ifneq ($(shell command -v pacman),)
    sudo pacman -Sy sbcl
else ifneq ($(shell command -v dnf),)
    sudo dnf install -y sbcl
else ifneq ($(shell command -v apt),)
    sudo apt install -y sbcl
else
	@echo "Could not determine steps to install SBCL! Please install SBCL and try again."
	exit 1
endif
	@echo "Looking for external dependencies..."
ifeq ($(shell command -v find),)
	@echo "'find' not found!"
	exit 1
endif
ifeq ($(shell command -v cat),)
	@echo "'cat' not found!"
	exit 1
endif
	@echo "All required dependencies found."

optional-deps:
	@echo "Installing optional dependencies..."
ifneq ($(shell command -v beep),)
	@echo "'beep' found."
else ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu beep
else ifneq ($(shell command -v beep),)
	sudo pacman -Sy sbcl
else ifneq ($(shell command -v beep),)
	sudo dnf install -y sbcl
else ifneq ($(shell command -v beep),)
	sudo apt install -y sbcl
else
	@echo "Could not install optional dependencies! Please install manually."
endif

quicklisp:
ifeq ("$(wildcard $(QUICKLISP_DIR))", "")
	@echo "Setting up Quicklisp..."
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --load /tmp/quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"
	sbcl --load ~/quicklisp/setup.lisp --non-interactive --eval "(ql:add-to-init-file)"
else
	@echo "Quicklisp found."
endif

binary:
	@echo "Generating binary..."
	sbcl --non-interactive --load build.lisp
	@echo "Binary generated."

place:
	@echo "Installing binary..."
	sudo install ./power-guard-bin $(PREFIX)/bin/power-guard
	sudo install ./scripts/* $(PREFIX)/bin/
	@echo "Binary installed."

manpage:
	@echo "Creating manpage..."
	sudo rsync ./man/power-guard.1 $(MANPREFIX)/man1/
	@echo "Manpage created."

service:
	@echo "Looking for a known init system..."
ifneq ($(shell command -v runit),)
	@echo "'Runit' found. Attempting to create a service..."
	sudo mkdir /etc/sv/power-guard
	sudo ln -s $(PREFIX)/bin/power-guard /etc/sv/power-guard/run
	sudo ln -s /etc/sv/power-guard /var/service
	@echo "Runit service created and started."
else
	@echo "No known init system found."
endif

install: primary-deps optional-deps quicklisp binary place manpage service
	@echo "power-guard is now installed."

uninstall:
	@echo "Uninstalling power-guard..."
	sudo rm $(PREFIX)/bin/power-guard*
	sudo rm $(MANPREFIX)/man1/power-guard.1
	sudo rm -rf /var/service/power-guard
	sudo rm -rf /etc/sv/power-guard
	@echo "power-guard has been uninstalled."

reinstall: uninstall install

get-latest:
	git pull origin main

update: get-latest reinstall
