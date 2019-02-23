;;; maple-preview.el ---  preview text file.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; preview text file.
;;

;;; Code:

(require 'cl-lib)
(require 'websocket)
(require 'simple-httpd)

(defgroup maple-preview nil
  "Realtime Preview"
  :group 'text
  :prefix "maple-preview:")

(defcustom maple-preview:host "localhost"
  "Preview http host."
  :type 'string
  :group 'maple-preview)

(defcustom maple-preview:port 8080
  "Preview http port."
  :type 'integer
  :group 'maple-preview)

(defcustom maple-preview:websocket-port 8081
  "Preview websocket port."
  :type 'integer
  :group 'maple-preview)

(defcustom maple-preview:browser-open t
  "Auto open browser."
  :type 'boolean
  :group 'maple-preview)

(defcustom maple-preview:auto t
  "Auto open browser."
  :type 'boolean
  :group 'maple-preview)

(defcustom maple-preview:auto-hook nil
  "Hook for user specified auto preview instance.

This hook run within the procedure of `maple-preview:init' when
customized variable `maple-preview:auto' was non-nil.

The internal auto-preview type transferred
`maple-preview:send-to-server' to the `post-self-insert-hook',
this hook providing more customization functional for as."
  :type 'hook
  :group 'maple-preview)


(defcustom maple-preview:finialize-hook nil
  "Hooks for run with `maple-preview:finalize', it's useful to
  remove all dirty hacking with `maple-preview:auto-hook'."
  :type 'hook
  :group 'maple-preview)


(defvar maple-preview:http-server nil
  "`maple-preview' http server.")
(defvar maple-preview:websocket-server nil
  "`maple-preview' websocket server.")
(defvar maple-preview:websocket nil)

(defvar maple-preview:home-path (file-name-directory load-file-name))
(defvar maple-preview:preview-file (concat maple-preview:home-path "index.html"))
(defvar maple-preview:css-file '("/static/css/markdown.css"))
(defvar maple-preview:js-file nil)


(defun maple-preview:send-preview (websocket)
  "Send file content to `WEBSOCKET`."
  (let ((mark-position-percent
         (number-to-string
          (truncate
           (* 100
              (/
               (float (-  (line-number-at-pos) (/ (count-screen-lines (window-start) (point)) 2)))
               (count-lines (point-min) (point-max))))))))
    (setq httpd-root default-directory)
    (websocket-send-text websocket
                         (concat
                          "<div id=\"position-percentage\" style=\"display:none;\">"
                          mark-position-percent
                          "</div>\n"
                          (maple-preview:text-content)))))

(defun maple-preview:send-to-server ()
  "Send the `maple-preview' preview to clients."
  (when (bound-and-true-p maple-preview-mode)
    (when (and maple-preview:websocket
               (buffer-file-name))
      (when (condition-case nil
                (string-match-p
                 "\\([oO][rR][gG]\\|[mM][dD]\\|markdown\\)"
                 (file-name-extension (buffer-file-name)))
              (error nil))
        (maple-preview:send-preview maple-preview:websocket)))))

(defun maple-preview:css-template ()
  "Css Template."
  (mapconcat
   (lambda (x)
     (if (string-match-p "^[\n\t ]*<style" x) x
       (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">" x)))
   maple-preview:css-file "\n"))

(defun maple-preview:js-template ()
  "Css Template."
  (mapconcat
   (lambda (x)
     (if (string-match-p "^[\n\t ]*<script" x) x
       (format "<script src=\"%s\"></script>" x)))
   maple-preview:js-file "\n"))

(defun maple-preview:preview-template ()
  "Template."
  (with-temp-buffer
    (insert-file-contents maple-preview:preview-file)
    (when (search-forward "{{ css }}" nil t)
      (replace-match (maple-preview:css-template) t))
    (when (search-forward "{{ js }}" nil t)
      (replace-match (maple-preview:js-template) t))
    (when (search-forward "{{ websocket }}" nil t)
      (replace-match (format
                      "%s:%s"
                      maple-preview:host
                      maple-preview:websocket-port)
                     t))
    (buffer-string)))

(defun maple-preview:text-content ()
  "Get file content."
  (let ((file-name buffer-file-truename))
    (cond ((eq major-mode 'org-mode)
           (require 'ox-md)
           (org-export-as 'md))
          ((or (eq major-mode 'web-mode)
               (eq major-mode 'html-mode))
           (concat
            (with-temp-buffer
              (insert-file-contents file-name)
              (buffer-string))
            "<!-- iframe -->"))
          (t (buffer-substring-no-properties (point-min) (point-max))))))

(defun maple-preview:init-websocket ()
  "Init websocket."
  (when (not maple-preview:websocket-server)
    (setq maple-preview:websocket-server
          (websocket-server
           maple-preview:websocket-port
           :host maple-preview:host
           :on-message (lambda (ws _frame)
                         (maple-preview:send-preview ws))
           :on-open (lambda (ws)
                      (setq maple-preview:websocket ws)
                      (message "websocket: I'm opened."))
           :on-error (lambda (_websocket _type _err)
                       (message "error connecting"))
           :on-close (lambda (_websocket)
                       (maple-preview:finalize)
                       (setq maple-preview-mode nil)
                       (message "Close maple-preview connects"))))))

(defun maple-preview:init-http-server ()
  "Start http server at PORT to serve preview file via http."
  (when (not maple-preview:http-server)
    (when (process-status "maple-preview")
      (delete-process "maple-preview"))
    (cl-letf (((symbol-function 'httpd-log) 'ignore))
      (setq httpd-root maple-preview:home-path
            httpd-host maple-preview:host
            httpd-port maple-preview:port)
      (setq maple-preview:http-server
            (make-network-process
             :name     "maple-preview"
             :service  httpd-port
             :server   t
             :host     httpd-host
             :family   httpd-ip-family
             :filter   'httpd--filter
             :filter-multibyte nil
             :coding   'binary
             :log      'httpd--log))
      (defservlet preview text/html (_path)
        (insert (maple-preview:preview-template))))))

(defun maple-preview:open-browser ()
  "Open browser."
  (browse-url
   (format "http://%s:%s/preview" maple-preview:host maple-preview:port)))

(defun maple-preview:init ()
  "Preview init."
  (maple-preview:init-websocket)
  (maple-preview:init-http-server)
  (when maple-preview:browser-open (maple-preview:open-browser))
  (when maple-preview:auto
    (add-hook 'post-self-insert-hook #'maple-preview:send-to-server)
    (run-hooks 'maple-preview:auto-hook))
  (add-hook 'after-save-hook #'maple-preview:send-to-server))

(defun maple-preview:finalize ()
  "Preview close."
  (when maple-preview:websocket-server
    (websocket-server-close maple-preview:websocket-server)
    (setq maple-preview:websocket-server nil))
  (when maple-preview:http-server
    (when (process-status "maple-preview")
      (delete-process "maple-preview"))
    ;; close connection
    (dolist (i (process-list))
      (when (and (string-prefix-p "maple-preview" (process-name i))
                 (equal (process-type i) 'network))
        (delete-process i)))
    (setq maple-preview:http-server nil))
  (remove-hook 'post-self-insert-hook 'maple-preview:send-to-server)
  (remove-hook 'after-save-hook 'maple-preview:send-to-server))

;;;###autoload
(defun maple-preview-cleanup ()
  "Cleanup `maple-preview' mode."
  (interactive)
  (maple-preview:finalize))

;;;###autoload
(define-minor-mode maple-preview-mode
  "Maple preview mode"
  :group      'maple-preview
  :init-value nil
  :global     t
  (if maple-preview-mode
      (maple-preview:init)
    (maple-preview:finalize)))

(provide 'maple-preview)
;;; maple-preview.el ends here
