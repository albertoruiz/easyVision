hVision Help

# [hVision][project] help system

- - -

Help topics:

- [GUI][gui]

- [command line options][options]

- [interactive point capture][points]

- [tutorial][tutorial]

- - -

The hVision help system uses simple html pages. When you press **F1** in any graphical window the default Internet browser
will open a local web page in the current directory, or in a "help" directory under or above the current one, with the same
name as the program executable and extension .html. If this page does not exist, we try the
name of the window, or finally a generic "help.html" file like this.

We can write the help files using markdown and a simple Makefile and scripts to allow LaTeX (Mathjax) and syntax highlighting (Highlightjs) of code samples.

INCLUDE(LINKS)

