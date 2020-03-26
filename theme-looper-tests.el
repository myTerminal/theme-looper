;;; theme-looper-tests.el

;; This file is not part of Emacs

;; Author: Ismail Ansari team.terminal@aol.in
;; Keywords: convinience, color-themes
;; Maintainer: Ismail Ansari team.terminal@aol.in
;; Created: 2014/03/22
;; Description: Loop thru the available color-themes with a key-binding
;; URL: http://ismail.teamfluxion.com, http://www.teamfluxion.com
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

;;; Tests:

(ert-deftest tl-test:setting-favorite-themes ()
  (unwind-protect
      (progn
        ;;Should contain all themes as favorite by default
	    (should (equal theme-looper--favorite-themes
		               (custom-available-themes)))
        ;;Should set the themes provided as favorite
	    (theme-looper-set-favorite-themes (list 'wombat
                                                'tango-dark
                                                'wheatgrass))
	    (should (equal theme-looper--favorite-themes
		               (list 'wombat
			                 'tango-dark
			                 'wheatgrass))))
    (setq theme-looper--favorite-themes
	      (custom-available-themes))))

(ert-deftest tl-test:disabling-enabled-themes ()
  (let ((current-theme (car custom-enabled-themes)))
    (unwind-protect
	    (progn
          ;;Should be able to disable all enabled themes
	      (theme-looper--disable-all-themes)
	      (should (equal custom-enabled-themes
			             '())))
      (when current-theme
        (load-theme current-theme t)))))

(ert-deftest tl-test:getting-theme-indices ()
  (let ((current-theme (car custom-enabled-themes)))
    (unwind-protect
	    (progn
	      (theme-looper-set-favorite-themes (list 'wombat
                                                  'tango-dark
                                                  'wheatgrass))
          ;;Should identify the enabled theme
	      (load-theme 'wombat
                      t)
	      (should (equal (theme-looper--get-current-theme)
			             'wombat))
          ;;Should identify the next theme position in the list
	      (should (equal (theme-looper--get-next-theme-index)
			             1))
          ;;Should identify the next theme
	      (should (equal (theme-looper--get-next-theme)
			             'tango-dark))
          ;;Should default to first theme as next theme when
          ;;current theme is not in the list
	      (load-theme 'manoj-dark
                      t)
	      (should (equal (theme-looper--get-next-theme)
			             'wombat))
          ;;Should loop back to the first theme when
          ;;current theme is the last in the list
	      (load-theme 'wheatgrass
                      t)
	      (should (equal (theme-looper--get-next-theme)
			             'wombat)))
      (setq theme-looper--favorite-themes
	        (custom-available-themes))
      (when current-theme
        (load-theme current-theme t)))))

(ert-deftest tl-test:setting-next-theme ()
  (let ((current-theme (car custom-enabled-themes)))
    (unwind-protect
	    (progn
	      (theme-looper-set-favorite-themes (list 'wombat
                                                  'tango-dark
                                                  'wheatgrass))
          ;;Should select first theme when the selected theme in not in the list
	      (load-theme 'tango
                      t)
	      (theme-looper-enable-next-theme)
	      (should (equal custom-enabled-themes
			             '(wombat)))
          ;;Should proceed to the next theme
	      (theme-looper-enable-next-theme)
	      (should (equal custom-enabled-themes
			             '(tango-dark)))
          ;;Should loop back to the first theme when the cycle completes
	      (theme-looper-enable-next-theme)
	      (theme-looper-enable-next-theme)
	      (should (equal custom-enabled-themes
			             '(wombat))))
      (setq theme-looper--favorite-themes
	        (custom-available-themes))
      (when current-theme
        (load-theme current-theme
                    t)))))

(ert-deftest tl-test:adding-customization ()
  (let ((current-theme (car custom-enabled-themes))
	    (current-face-background (face-background 'default)))
    (unwind-protect
	    (progn
          (set-face-background 'default "red")
          (message (concat "current face-background: red"))
          (theme-looper-set-favorite-themes (list 'wombat
                                                  'tango-dark
                                                  'wheatgrass))
          ;;Should apply customizations when specified
          (load-theme 'tango
                      t)
	      (theme-looper-set-post-switch-script (lambda ()
                                                 (set-face-background 'default
                                                                      "green")))
	      (theme-looper-enable-next-theme)
	      (message (concat "found face-background: "
                           (face-background 'default)))
	      (should (equal (face-background 'default) "green")))
      (setq theme-looper--favorite-themes
	        (custom-available-themes))
      (setq theme-looper-post-switch-hook nil)
      (when current-theme
        (load-theme current-theme t))
      (set-face-background 'default current-face-background))))

;;; theme-looper-tests.el ends here
