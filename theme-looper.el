;;; theme-looper.el --- Loop thru the available color-themes

;; This file is not part of Emacs

;; Author: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Version: 2.3
;; Keywords: convenience, color-themes
;; Maintainer: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Created: 2014/03/22
;; Package-Requires: ((cl-lib "0.5"))
;; Description: Loop thru the available color-themes with a key-binding
;; URL: http://ismail.teamfluxion.com
;; Compatibility: Emacs24


;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'theme-looper)
;;
;; And set a key-bindings for cycling thru themes
;;
;;     (global-set-key (kbd "C-}") 'theme-looper-enable-next-theme)
;;     (global-set-key (kbd "C-{") 'theme-looper-previous-next-theme)
;;
;; Or you can switch to a random theme
;;
;;     (global-set-key (kbd "C-\\") 'theme-looper-enable-random-theme)
;;
;; Or you can choose from a list of themes using ivy's completion interface
;;
;;     (global-set-key (kbd "C-|") 'theme-looper-select-theme)
;;
;; You can also set a list of your favorite themes
;;
;;     (theme-looper-set-favorite-themes '(wombat tango-dark wheatgrass))
;;
;; or to use regular expressions
;;
;;     (theme-looper-set-favorite-themes-regexp "dark")
;;
;; You can alternatively set the themes to be ignored
;;
;;     (theme-looper-set-ignored-themes '(cobalt))
;;
;; or to use regular expressions
;;
;;     (theme-looper-set-ignored-themes-regexp "green")
;;
;; Or you can set both, in which case only the favorite themes that are not
;; within the ones to be ignored are used.
;;
;; If you want to reset your color-theme preferences, simply use
;;
;;     (theme-looper-reset-themes-selection)
;;
;; You can set some script to be run after every theme switch
;;
;;     (theme-looper-set-post-switch-script my-func)
;;

;;; Commentary:

;;     You can use this package to cycle through color themes in Emacs with a
;;     shortcut. Select your favorite themes, unfavorite thmes and key-bindings
;;     to switch color themes in style!
;;
;;  Overview of features:
;;
;;     o   Loop though available color themes conveniently
;;     o   Narrow down the list of color themes to only the ones you like
;;

;;; Code:

(require 'cl-lib)

(defvar theme-looper--favorite-themes)

(defvar theme-looper--ignored-themes
  nil)

(defun theme-looper--post-switch
    nil)

(defvar theme-looper--themes-map-separator
  " | ")

;;;###autoload
(defun theme-looper-set-favorite-themes (themes)
  "Sets the list of color-themes to cycle thru"
  (setq theme-looper--favorite-themes
	themes))

;;;###autoload
(defun theme-looper-set-favorite-themes-regexp (regexp)
  "Sets the list of color-themes to cycle thru, matching a regular expression"
  (setq theme-looper--favorite-themes
        (cl-remove-if-not (lambda (theme)
                            (string-match-p regexp
                                            (symbol-name theme)))
                          (custom-available-themes))))

;;;###autoload
(defun theme-looper-set-ignored-themes (themes)
  "Sets the list of color-themes to ignore"
  (setq theme-looper--ignored-themes
	themes))

;;;###autoload
(defun theme-looper-set-ignored-themes-regexp (regexp)
  "Sets the list of color-themes to ignore, matching a regular expression"
  (setq theme-looper--ignored-themes
        (cl-remove-if-not (lambda (theme)
                            (string-match-p regexp
                                            (symbol-name theme)))
                          (custom-available-themes))))

;;;###autoload
(defun theme-looper-set-post-switch-script (func)
  "Sets script to be run after every theme switch"
  (setq theme-looper--post-switch
        func))

;;;###autoload
(defun theme-looper-reset-themes-selection ()
  "Resets themes selection back to default"
  (theme-looper-set-favorite-themes (custom-available-themes))
  (theme-looper-set-ignored-themes nil))

(defun theme-looper--get-current-theme ()
  "Determines the currently enabled theme"
  (car custom-enabled-themes))

(defun theme-looper--get-current-theme-index ()
  "Finds the currently enabled color-theme in the list of color-themes"
  (cl-position (theme-looper--get-current-theme)
               (theme-looper--get-looped-themes)
               :test #'equal))

(defun theme-looper--get-looped-themes ()
  (cl-remove-if (lambda (theme)
                  (member theme
                          theme-looper--ignored-themes))
                theme-looper--favorite-themes))

(defun theme-looper--get-next-theme-index ()
  "Find the index of the next color-theme in the list, to be moved to"
  (let ((current-theme-index (theme-looper--get-current-theme-index)))
    (cond
     ((equal current-theme-index
	     'nil)
      0)
     ((equal current-theme-index
	     (- (length (theme-looper--get-looped-themes))
		1))
      0)
     ((+ current-theme-index
         1)))))

(defun theme-looper--get-next-theme ()
  "Determines the next color-theme to be enabled"
  (nth (theme-looper--get-next-theme-index)
       (theme-looper--get-looped-themes)))

(defun theme-looper--get-previous-theme-index ()
  "Find the index of the previous color-theme in the list, to be moved to"
  (let ((current-theme-index (theme-looper--get-current-theme-index)))
    (cond
     ((equal current-theme-index
	     'nil)
      0)
     ((equal current-theme-index
	     0)
      (- (length (theme-looper--get-looped-themes))
         1))
     ((- current-theme-index
         1)))))

(defun theme-looper--get-previous-theme ()
  "Determines the previous color-theme to be enabled"
  (nth (theme-looper--get-previous-theme-index)
       (theme-looper--get-looped-themes)))

(defun theme-looper--disable-all-themes ()
  "Disables all the enabled color-themes"
  (mapcar 'disable-theme
	  custom-enabled-themes))

(defun theme-looper--nth-cyclic (index collection)
  (cond ((< index
            0) (theme-looper--nth-cyclic (+ index
                                            (length collection))
            collection))
        ((> index
            (1- (length collection))) (theme-looper--nth-cyclic (- index
                                                                   (length collection))
            collection))
        (t (nth index
                collection))))

(defun theme-looper--print-theme-path (theme)
  (let ((theme-index (cl-position theme
                                  (theme-looper--get-looped-themes)
                                  :test #'equal)))
    (message (concat "theme-looper: "
                     (symbol-name (theme-looper--nth-cyclic (- theme-index 2)
                                                            (theme-looper--get-looped-themes)))
                     theme-looper--themes-map-separator
                     (symbol-name (theme-looper--nth-cyclic (- theme-index 1)
                                                            (theme-looper--get-looped-themes)))
                     theme-looper--themes-map-separator
                     (propertize (symbol-name theme)
                                 'face
                                 '(:inverse-video t))
                     theme-looper--themes-map-separator
                     (symbol-name (theme-looper--nth-cyclic (+ theme-index 1)
                                                            (theme-looper--get-looped-themes)))
                     theme-looper--themes-map-separator
                     (symbol-name (theme-looper--nth-cyclic (+ theme-index 2)
                                                            (theme-looper--get-looped-themes)))))))

(defun theme-looper-enable-theme-with-map (theme)
  (theme-looper-enable-theme theme)
  (theme-looper--print-theme-path theme))

(defun theme-looper-enable-theme-with-log (theme)
  (theme-looper-enable-theme theme)
  (message "theme-looper: %s"
           theme))

;;;###autoload
(defun theme-looper-enable-theme (theme)
  "Enables the specified color-theme"
  (theme-looper--disable-all-themes)
  (load-theme theme
              t)
  (theme-looper--post-switch))

;;;###autoload
(defun theme-looper-enable-next-theme ()
  "Enables the next color-theme in the list"
  (interactive)
  (let ((next-theme (theme-looper--get-next-theme)))
    (theme-looper-enable-theme-with-map next-theme)))

;;;###autoload
(defun theme-looper-enable-previous-theme ()
  "Enables the previous color-theme in the list"
  (interactive)
  (let ((previous-theme (theme-looper--get-previous-theme)))
    (theme-looper-enable-theme-with-map previous-theme)))

;;;###autoload
(defun theme-looper-enable-random-theme ()
  "Enables a random theme from the list"
  (interactive)
  (let ((some-theme (nth (random (length (theme-looper--get-looped-themes)))
                         (theme-looper--get-looped-themes))))
    (theme-looper-enable-theme-with-log some-theme)))

;;;###autoload
(defun theme-looper-select-theme ()
  "Lets user select a theme from a list of favorite ones rendered using ivy API"
  (interactive)
  (if (featurep 'ivy)
      (ivy-read "theme-looper: "
                (theme-looper--get-looped-themes)
                :action (lambda (th)
                          (theme-looper-enable-theme (intern th))))
    (message "theme-looper: package 'ivy' is not installed!")))

(theme-looper-reset-themes-selection)

(provide 'theme-looper)

;;; theme-looper.el ends here
