# Gleitz emacs configuration

Bits and pieces collected over the years. Folder structure and base configuration inspired by [magnars/.emacs.d](https://github.com/magnars/.emacs.d).

Emacs is intalled from [[https://github.com/d12frosted/homebrew-emacs-plus]].

## Setup

To grab all the dependencies:

    git clone git://github.com/gleitz/.emacs.d.git

The first time you start emacs, it will install some additional packages
that are best handled by the package manager.

## Install emacs on mac

I use Cocoa Emacs, installed like this:

    brew install emacs --cocoa

To open it with Alfred or Quicksilver, you have to copy `Emacs.app` into
`/Applications` instead of the symlink that brew places there.

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs +
   [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * The key bindings are optimized for a norwegian keyboard layout.

 * Start by reading up on all the cool stuff in [key-bindings.el](settings/key-bindings.el).

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

 * Find file in project with `C-x C-o`, in dir with `C-x C-f`, recent with `C-x f`

 * Add your user- and project-specific stuff in .emacs.d/users/[machine name]/*.el

 * Autocomplete with `C-.` (autocomplete entire lines with `C-:`)

 * expand-region is your friend. Find its bound key by doing `F1 f er/expand-region`

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * Quickly jump anywhere in the buffer with `C-ø` then the starting letter of a word.

 * Indent and clean up white space in the entire buffer with `C-c n`

 * On a mac, the Meta key `M` is bound to Command.

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

 * Watch [emacsrocks.com](http://emacsrocks.com)

## Survival guide for the first week of emacs

When you start using emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar keybindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to cmd on my mac settings)
* `S      ` Shorthand for the shift-key

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x C-o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation

### Useful Commands
* `C-;`, `C-:`, `C-'` Jumping between windows`
* `M-`    `   For going to where you just edited
* `C-x C-space` Popping the mark
* `M-$` Spellcheck
* `M-s o` Lines matching query
* `C-x $` Reveal mode
* `M-s .` isearch the symbol at point
* `C-@` Widen the selection progressively
* `M-s \` Multi-buffer isearch
* `C-x C-;` Comment/uncomment region
* `C-.` dot-mode to repeat last command
* `C-x z` repeat last command
* `kmacro-edit-lossage` and then `C-x e`
* `M-x name-last-kbd-macro` and then save with `M-x insert-kbd-macro`
* `M-?` Find references
* `C-c ! e/l/etc` Interact with flycheck errors and messages
* `s-tilde` Option+tilde Highlight symbol at point
* `M-s .` isearch-forward-symbol-at-point
* `C-M-p` previous matching paren
* `C-M-n` next matching paren
* `visual-line-mode` to toggle breaking words at the end of lines
* `dired-do-find-regex-and-replace` for [replace operations](https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-text-across-multiple-files.html) across multiple files
* `M-x httpd-start M-x impatient-mode` then navigate to `http://localhost:8080/imp/` to view live webpage of html
* `M-x atomic-chrome-start-server` and then use GhostText to edit with Emacs
* `M-x find-file-literally` to open without any modes (good for long files)
* `C-u C-x =` unicode descriptor for character at poiny

### Copilot

When installing copilot, fund where the lang server installs the `agent.js` (/Users/gleitz/.emacs.d/.cache/copilot) and change `.stop` to have three newlines.

### Useful Packages
* git-link

### Magit
* `c a` to ammend a commit
