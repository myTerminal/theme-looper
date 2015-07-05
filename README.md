# theme-looper

[![MELPA](http://melpa.org/packages/theme-looper-badge.svg)](http://melpa.org/#/theme-looper)
[![Marmalade](https://img.shields.io/badge/marmalade-available-8A2A8B.svg)](https://marmalade-repo.org/packages/theme-looper)
[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)
[![Gratipay](http://img.shields.io/gratipay/myTerminal.svg)](https://gratipay.com/myTerminal)

A color-theme looper for Emacs 24.

## Background
### (you can skip to Usage)

Emacs 24 comes with a set of pre-installed color-themes that can be enabled by a simple command like:

    M-x load-theme RET wombat
    
The above command enables (rather activates) 'wombat' theme. However, when you get lazy like I did, you need something like theme-looper.el. So, if you have a list of your favorite color-themes and you want to cycle thru them with simple key-strokes, you've come to the right place.

## Installation

### Manual

Save the file 'theme-looper.el' to disk and add the directory containing it to 'load-path' using a command in your '.emacs' file like:

    (add-to-list 'load-path "~/.emacs.d/")
    
The above line assumes that you've placed the file into the Emacs directory '.emacs.d'.

Start the package with:

    (require 'theme-looper)

### MELPA / Marmalade

If you have MELPA or Marmalade added as a repository to your Emacs, you can just install *theme-looper* with

    M-x package-install theme-looper RET

## Usage

Optional: Set your list of favorite color themes:

    (theme-looper-set-theme-set (list 'deeper-blue 'tango-dark 'tsdh-dark 'wheatgrass))

Set up your key-bindings:

    (global-set-key (kbd "C-|") 'theme-looper-enable-next-theme)
    (global-set-key (kbd "C-\") 'theme-looper-enable-random-theme)

Optional: Set additional customization to be applied after every theme switch:

    (theme-looper-set-customizations 'my-func)

## Let me know
Let me know your suggestions on improving 'theme-looper.el' at ismail@teamfluxion.com

Thank you!
