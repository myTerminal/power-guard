# power-guard

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/myTerminal/power-guard.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

A battery protector for mobile computers

## Background

Continuing to use your computer when the battery charge is critically low could lead to serious damage to the battery. You may never face this issue while using operating systems like Windows, macOS, or even "ready-to-use" Linux distributions like Ubuntu and Fedora. However, for minimal setups like Arch, Void, and similar distributions, you may have to be careful about the battery charge as there's mostly nothing in place to take care of it on your behalf.

*power-guard* provides a simple command to fill in the gap by keeping a watch over the remaining battery charge and automatically suspending (or hibernating) your system to protect your battery from damage. You may also run it as a service with supported init systems.

### Suspend vs Hibernate

*power-guard* usually prefers `suspend` over `hibernate`, but performs the latter when there is more than one battery equipped. This could be handy in the absence of charging points nearby and the user can quickly swap the discharged battery with a charged one and resume working without turning the computer off.

## Installation

There are a few different ways to get *power-guard*.

### Compile from source

    # Clone project to the local workspace
    git clone https://github.com/myTerminal/power-guard.git

    # Switch to the project directory
    cd power-guard

    # Install with `make`
    make install

### Automatic installation

Simply execute the below command in a terminal; the rest should be automatic.

    /bin/bash -c "$(curl https://raw.githubusercontent.com/myTerminal/power-guard/main/install)"

### Through a package manager

*power-guard* will soon be available to install from your operating system's package manager.

## How to Use

A simple way to use *power-guard* is to run it in a command-line terminal with no command arguments.

    power-guard

By default, it watches the remaining charge on the battery and suspends (or hibernates) the computer when it drops below 10%. You can also optionally specify the minimum threshold percentage of charge as an argument.

    power-guard 7

The above example only suspends the computer when the remaining charge is below 7%.

### Running as a service

*power-guard* can also be run as an init service, and currently only supports [Runit](http://smarden.org/runit). Support for more init systems will be implemented soon.

### Further help with commands

To learn more about usage, refer to `manpage`:

    man power-guard

## Updating

In order to update *power-guard*, simply run:

    power-guard-update

## Uninstalling

In order to uninstall *power-guard*, simply run:

    power-guard-uninstall

## External Dependencies

Being written with Common Lisp, *power-guard* depends on [SBCL](https://www.sbcl.org). In most cases, it will be automatically installed while generating the binary, but if it doesn't please install it before running the installation.

The other required programs are as follows:

 - [find](https://man.archlinux.org/man/find.1.en)
 - [cat](https://man.archlinux.org/man/cat.1.en)
 - [command](https://man.archlinux.org/man/command.1p.en)

## Optional External Dependencies

Below are a few more optional dependencies that are also attempted to be fetched during the installation:

 - [beep](https://pkgs.org/search/?q=beep)
 - [notify-send](https://man.archlinux.org/man/notify-send.1.en)

If not installed automatically, you may install them manually.

## To-do

* Improve reading of battery level, add support for more hardware types
* Implement alternate methods of suspending the system
* Add support for more init systems like OpenRC, SysVinit, etc.
