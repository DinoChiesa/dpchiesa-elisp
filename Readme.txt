Wed, 11 May 2011  15:06

These are various emacs libraries and packages.
Also my own emacs.el is here, for your viewing pleasure.

csharp-mode.el
  a major mode for editing C#

vbnet-mode.el
  a major mode for editing VB.NET

vbs-mode.el
  a major mode for editing VBScript (less mature)

powershell.el
  an inferior shell for emacs that runs Powershell.  For various reasons
  you cannot simply specify "powershell.exe" as your shell command.
  This module integrates the emacs interactive shell mode stuff (comint)
  with powershell.

tfs.el
  Various commands you can use from emacs to tickle TFS.  Supported
  actions: checkout, checkin, properties, rename, get, history, undo,
  diff, status, annotate.

rfringe.el
  indicate buffer-relative positions on the fringe (the screen area next
  to the vertical scroll bar on the right hand side of the emacs
  window). This is handy for displaying the buffer-relative positions of
  compiler errors, or flymake status, in the fringe.

jsshell-bundle.el
  interactive javascript shell. Run Javascript code in a shell within
  an emacs buffer.

flymake-cursor.el
  A little thing that displays flymake status in the minibuffer, after
  you've paused for a while with point on a line for which there is a
  flymake error.  It uses a time to avoid flipping the minibuffer
  constantly as you scroll through a buffer.  It displays only after
  you've stopped moving.

iirf-mode.el
  a mode for editing IIRF config files within emacs.
