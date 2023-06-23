;;; mess.el --- Front-end for MAME MESS.

;; Author: Yong <luo.yong.name@gmail.com>
;; URL: https://github.com/Iacob/elmame
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Front-end for MAME MESS.
;; Only cartridge based game consoles are supported currently.


;;; Code:


(provide 'mess)

(require 'widget)
(require 'wid-edit)

(require 'mess-base)
(require 'mess-config)
(require 'mess-machine-info-loader)


(defun mess-make-shell-command (machine-name device-image)
  "Make the shell command to start a machine with MACHINE-NAME."
  (let ((exec (mess-base-get-config 'exec))
	(rompath (mess-base-get-config 'rompath))
	(args (mess-base-get-config 'args))
	args-text)
    (when (not args)
      (setq args-text ""))
    (if (stringp args)
        (setq args-text args)
      (setq args-text
            (string-join (mapcar (lambda (arg) (format "%s" arg)) args) " ")))
    ;; mame <machine-name> -rompath roms -cart <device image>
    (format "%s %s -rompath %s -cart %s %s" exec machine-name rompath device-image args-text)))

(defvar mess-mode-map
  ;;(let ((map (make-sparse-keymap)))
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "g" 'mess)
    (define-key map "c" 'mess-config-open-config-panel)
    (define-key map [menu-bar mess]
      (cons "MESS" (make-sparse-keymap "mess")))
    (define-key map [menu-bar mess refresh]
      '("Refresh" . mess))
    (define-key map [menu-bar mess config]
      '("Config Panel" . mess-config-open-config-panel))
    map)
  "Keymap for `mess-mode'.")

(define-derived-mode mess-mode fundamental-mode "MESS"
  "Major mode for MAME MESS front-end interface.")

;;;###autoload
(defun mess ()
  "Start MESS front-end."
  (interactive)

  (switch-to-buffer "**machine list**")
  (setq truncate-lines 't)
  (let ((inhibit-read-only 't))
    (erase-buffer))

  (mess-base-reload-user-config)

  (widget-insert "\n")
  
  (dolist (path1 (mess-base-get-config 'device-image-path-list))
    
    (let ((machine (car path1))
          (path (cadr path1))
          filelist)
      
      (widget-insert (propertize machine 'face 'info-title-3) "\n")
      
      (condition-case err
          (setq filelist (directory-files path))
        (error nil)
        )

      (dolist (file filelist)
        (when (file-regular-p (concat path "/" file))
          (widget-insert " ")
          (widget-create 'link
                         :notify (lambda (w &rest params)
                                   (message-box (widget-get w :filename)))
                         :machine machine
                         :path path
                         :filename file
                         (concat "💾" file))
          (widget-insert "\n")
          )
        
        )
      
      )
    
    (widget-insert "\n")
    
    )
  
  (mess-mode)

  (beginning-of-buffer)
  
  (use-local-map mess-mode-map)
  (widget-setup))

;;; mess.el ends here
