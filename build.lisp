(load "package.lisp")
(load "src/shell.lisp")
(load "src/hardware.lisp")
(load "src/main.lisp")

(sb-ext:save-lisp-and-die "power-guard-bin"
                          :toplevel 'main:main
                          :executable t)
