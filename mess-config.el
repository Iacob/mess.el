;;; mess-config.el --- Config panel of mess.el

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

;; Configuration panel for mess.el


;;; Code:


(provide 'mess-config)

(require 'widget)
(require 'wid-edit)

(defvar mess-config-user-config-text "" "User config file content.")
(defvar mess-config-form '() "Form fields.")
(defvar mess-config-formvalues (make-hash-table)  "Form values.")

(defvar mess-config-machine-names '() "Machine names.")

(defvar mess-config-path-list-start-line 2 "Device list start line.")

(defun mess-config-fill-field (name value)
  (dolist (field mess-config-form)
    (when (and (equal (plist-get field 'name) name) (plist-get field 'widget))
      (widget-value-set (plist-get field 'widget) value))))

(defun mess-config-field-value (name)
  (let (field-value)
    (dolist (field mess-config-form)
      (when (and (null field-value)
                 (equal (plist-get field 'name) name)
                 (plist-get field 'widget))
        (setq field-value (widget-value (plist-get field 'widget)))))
    field-value))

(defun mess-config--add-path (machine path)
  "Add device image path."
  (let ((path-list (mess-config-field-value 'device-image-path-list)))
    (add-to-list 'path-list (concat machine " - " path) 't)
    (mess-config-fill-field 'device-image-path-list path-list)))

(defun mess-config--save-formvalues ()
  (let (conf)
    (maphash (lambda (k v)
               (setq conf (append conf (list k v))))
             mess-config-formvalues)
    conf))

;;;###autoload
(defun mess-config-open-config-panel ()
  "Open mess.el config panel."
  (interactive)

  (switch-to-buffer "**mame.el configuration**")
  
  (kill-all-local-variables)
  
  (make-local-variable 'mess-config-form)
  (make-local-variable 'mess-config-formvalues)
  (make-local-variable 'mess-config-machine-names)
  
  (let ((inhibit-read-only 't))
    (erase-buffer))
  (remove-overlays)

  (setq mess-config-form '())
  (setq mess-config-formvalues (make-hash-table))
  
  (setq mess-config-machine-names
        (seq-map (lambda (m)
                   (concat (plist-get m 'name) " - " (plist-get m 'desc)))
                 (seq-filter
                  (lambda (m)
                    (seq-some
                     (lambda (d) (and (string= "cartridge" (plist-get d 'name))
                                      (plist-get d 'mandatory)))
                     (plist-get m 'devices)))
                  (mess-machine-info-loader-load))))






  
  
  (insert "\n" (propertize "mess.el configuration" 'face 'info-title-2) "\n\n")
  (widget-create 'link
		 :notify
		 (lambda (w &rest params)
		   nil)
		 "Select working directory")
  (insert "\n")
  (widget-create 'const :format "➥ %v" "")
  (insert "\n")
  (widget-create 'link
		 :notify
		 (lambda (w &rest params)
		   nil)
		 "Select mame executable")
  (insert "\n")
  (widget-create 'const :format "➥ %v" "")
  (insert "\n")
  (widget-create 'link
		 :notify
		 (lambda (w &rest params)
		   nil)
		 "Select rom directory")
  (insert "\n")
  (widget-create 'const :format "➥ %v" "")
  (insert "\n")
  (insert "\nextra arguments:\n")
  (widget-create 'editable-field "")
  (insert "\n")


  (widget-insert "\n" "Device image path:" "\n")

  (add-to-list 'mess-config-form
               (list 'name 'device-image-path-list
         	     'type 'list
         	     'widget (widget-create 'editable-list
                                            :entry-format "%d %v"
                                            :format "%v"
                                            :notify
                                            (lambda (widget &rest ignore)
                                              nil)
                                            :value '("<NOT CONFIGURED YET>")
                                            '(editable-field
                                              :value "three"))))

  (widget-insert "\n")
  
  (widget-create 'link
        	 :notify
        	 (lambda (w &rest params)
                   (let ((name (completing-read
                                "Please enter machine name (TAB to list): "
                                mess-config-machine-names)))
                     (mess-config-fill-field 'machine-name name)))
        	 "Select machine to add")
  (widget-insert "  ")
  (add-to-list 'mess-config-form
               (list 'name 'machine-name
         	     'type 'text
         	     'widget (widget-create 'const :format "➡ %v" "")))
  (widget-insert "\n")
  (widget-create 'link
        	 :notify
        	 (lambda (w &rest params)
                   (let ((path (read-directory-name
                                "Please enter device image path: ")))
                     (mess-config-fill-field 'device-image-path path)))
        	 "Please select device image path")
  (widget-insert "  ")
  (add-to-list 'mess-config-form
               (list 'name 'device-image-path
         	     'type 'text
         	     'widget (widget-create 'const :format "➡ %v" "")))
  (widget-insert "\n")
  (widget-create 'link
        	 :notify
        	 (lambda (w &rest params)
                   (mess-config--add-path
                    (mess-config-field-value 'machine-name)
                    (mess-config-field-value 'device-image-path))
                   ;;(mess-config--save-formvalues)
                   )
        	 "Add")
  (widget-insert " ")
  (widget-create 'link
        	 :notify
        	 (lambda (w &rest params)
                   (mess-config-fill-field 'machine-name "")
                   (mess-config-fill-field 'device-image-path ""))
        	 "Clear")  
  
  (widget-insert "\n\n\n")
  (widget-create 'link :notify (lambda (&rest params) (message "line: %s" (line-number-at-pos (point)))) "Save")
  (widget-create 'link
		 :notify
                 (lambda (w &rest params)
		   (kill-buffer (buffer-name)))
		 "Close")

  
  (use-local-map widget-keymap)
  (widget-setup))

;;; mess-config.el ends here
