# power-guard

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/myTerminal/power-guard.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

A battery protector for mobile computers

## Background

If your operating system does not suspend/hibernate your computer when the remaining power is critically low, it could lead to serious damage to the battery. While using operating systems like Windows, macOS, or even "ready-to-use" Linux distributions like Ubuntu and Fedora, you may never face such an issue. However, for minimal installations like Arch, Void, and similar distributions, you may have to be careful about the battery charge as there's mostly nothing in place to take of it for you.

*power-guard* provides a simple command to fill in the gap by keeping a watch over the remaining battery charge and automatically suspending your system to protect your battery from damage. You may also be able to run it as a service with supported init systems.

## Installation

There are a few different ways to get *power-guard*.

### Compile from source

    # Clone project to the local workspace
    git clone https://github.com/myTerminal/power-guard.git

    # Switch to the project directory
    cd power-guard

    # Install with `make`
    make install

Uninstalling would need only a single command:

    make uninstall

Re-installation is also possible with:

    make reinstall

### Through a package manager

*power-guard* will soon be available to install from your operating system's package manager.

## How to Use

A simple way to use *power-guard* is to run it in a command-line terminal with no command arguments.

    power-guard

By default, it watches the remaining charge on the battery and suspends the computer when it drops below 10%. You can also optionally specify the minimum threshold percentage of charge as an argument.

    power-guard 7

The above example only suspends the computer when the remaining charge is below 7%.

### Running as a service

*power-guard* can also be run as an init service, and currently only supports [Runit](http://smarden.org/runit). Support for more init systems will be implemented soon.

### Further help with commands

To learn more about usage, refer to `manpage`:

    man power-guard

## External Dependencies

Being written with Common Lisp, *power-guard* depends on [SBCL](https://www.sbcl.org). In most cases, it will be automatically installed while generating the binary, but if it doesn't please install it before running the installation.

## Optional External Dependencies

Below are a few more optional dependencies that are also attempted to be fetched during the installation:

 - [beep](https://pkgs.org/search/?q=beep)

 If not installed automatically, you may install them manually.

## To-do

* Improve battery-watching algorithm
* Improve reading of battery level, add support for more hardware types
* Implement alternate methods of suspending the system
* Add support for more init systems like SystemD, OpenRC, SysVinit, etc.
