;;; mess-base.el --- MESS base functions.

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

;; MESS base functions.


;;; Code:

(provide 'mess-base)

(defvar mess-base-user-config nil "The mess.el user config from file.")

(defun mess-base-read-user-config ()
  "Read user config."
  (let (config-text cfg)
    (condition-case err
        (when (file-readable-p "~/.elmess")
          (with-temp-buffer
            (insert-file-contents "~/.elmess")
            (setq config-text
                  (buffer-substring-no-properties (point-min) (point-max))))
          (setq cfg (read config-text)))
      (error (message "Exception: %s" err)))
    cfg))

(defun mess-base-reload-user-config ()
  "Reload user config to variable and return it."
  (setq mess-base-user-config (mess-base-read-user-config)))

(defun mess-base-get-user-config ()
  "Get user config, load it into memory when it's not loaded yet."
  (or mess-base-user-config (mess-base-reload-user-config)))

(defun mess-base-get-config (name)
  "Get config value with a NAME."
  (let ((default-config '(exec "mame" rompath "roms"))
        (user-config (mess-base-get-user-config))
        config-value)
    ;; (if (boundp 'mess-config)
    ;;     (setq config-value (plist-get mess-config name)))
    (if (not config-value)
        (setq config-value (plist-get user-config name)))
    (if (not config-value)
        (setq config-value (plist-get default-config name)))
    config-value))

(defun mess-base-save-user-config (user-config)
  "Save user config to file."
  (save-window-excursion
    (with-temp-buffer
      (insert (or (format "%S" user-config) "()"))
      (write-file "~/.elmess" 't))))

;;; mess-base.el ends here
