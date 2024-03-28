;;; maple-preview.el ---  preview text file.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2024 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2") (websocket "1.9"))
;; URL: https://github.com/honmaple/emacs-maple-preview


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
(require 'web-server)
(require 'websocket)

(declare-function org-export-as 'ox-html)

(defgroup maple-preview nil
  "Realtime Preview."
  :group 'text
  :prefix "maple-preview:")

(defcustom maple-preview:allow-modes '(org-mode markdown-mode html-mode web-mode)
  "Allow preview modes."
  :type 'list
  :group 'maple-preview)

(defcustom maple-preview:host "127.0.0.1"
  "Preview http host."
  :type 'string
  :group 'maple-preview)

(defcustom maple-preview:port t
  "Preview http port, t means auto select unused port."
  :type 'integer
  :group 'maple-preview)

(defcustom maple-preview:delay 0.1
  "Delay time when auto update preview."
  :type 'float
  :group 'maple-preview)

(defcustom maple-preview:browser-open t
  "Auto open browser."
  :type 'boolean
  :group 'maple-preview)

(defcustom maple-preview:auto-update t
  "Auto update when preview."
  :type 'boolean
  :group 'maple-preview)

(defcustom maple-preview:auto-scroll t
  "Auto scroll when preview."
  :type 'boolean
  :group 'maple-preview)

(defcustom maple-preview:text-content
  '((t . maple-preview:markdown-content))
  "How to preview text, export to markdown or html."
  :type 'cons
  :group 'maple-preview)

(defcustom maple-preview:css-file
  '("/preview/static/css/markdown.css"
    "/preview/static/css/highlight.css")
  "Custom preview css style."
  :type 'list
  :group 'maple-preview)

(defcustom maple-preview:js-file
  '("/preview/static/js/jquery.min.js"
    "/preview/static/js/marked.min.js"
    "/preview/static/js/marked-highlight.min.js"
    "/preview/static/js/highlight.min.js"
    "/preview/static/js/mermaid.min.js")
  "Custom preview js script."
  :type 'list
  :group 'maple-preview)

(defcustom maple-preview:auto-hook nil
  "Hook for user specified auto preview instance.

This hook run within the procedure of `maple-preview:init' when
customized variable `maple-preview:auto-update' was non-nil.

The internal auto-preview type transferred
`maple-preview:send-to-server' to the `post-self-insert-hook',
this hook providing more customization functional for as."
  :type 'hook
  :group 'maple-preview)

(defcustom maple-preview:finialize-hook nil
  "Hooks for run with `maple-preview:finalize'.
It's useful to remove all dirty hacking with `maple-preview:auto-hook'."
  :type 'hook
  :group 'maple-preview)

(defvar maple-preview:server nil
  "`maple-preview' http server.")
(defvar maple-preview:websocket nil)
(defvar maple-preview:sending nil)

(defvar maple-preview:home-path (file-name-directory load-file-name))
(defvar maple-preview:index-file (concat maple-preview:home-path "index.html"))

(defun maple-preview:mime-type(path)
  "Guess mime type from PATH."
  (let ((mime (mm-default-file-type path)))
    (if (and (not mime) (string-suffix-p ".js" path))
        "application/javascript"
      mime)))

(defun maple-preview:position-percent ()
  "Preview position percent."
  (when maple-preview:auto-scroll
    (format
     "<div id=\"position-percentage\" style=\"display:none;\">%s</div>\n"
     (number-to-string
      (truncate (* 100 (/ (float (-  (line-number-at-pos) (/ (count-screen-lines (window-start) (point)) 2)))
                          (count-lines (point-min) (point-max)))))))))

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

(defun maple-preview:template ()
  "Template."
  (with-temp-buffer
    (insert-file-contents maple-preview:index-file)
    (when (search-forward "{{ css }}" nil t)
      (replace-match (maple-preview:css-template) t))
    (when (search-forward "{{ js }}" nil t)
      (replace-match (maple-preview:js-template) t))
    (when (search-forward "{{ websocket }}" nil t)
      (replace-match (maple-preview:listen) t))
    (buffer-string)))

(defun maple-preview:html-content ()
  "Get file html content."
  (concat (cond ((memq major-mode '(org-mode markdown-mode))
                 (unless (featurep 'ox-html) (require 'ox-html))
                 (let ((org-html-postamble nil))
                   (ignore org-html-postamble)
                   (org-export-as 'html)))
                (t (buffer-substring-no-properties (point-min) (point-max))))
          "<!-- iframe -->"))

(defun maple-preview:markdown-content ()
  "Get file markdown content."
  (cond ((eq major-mode 'org-mode)
         (unless (featurep 'ox-md) (require 'ox-md))
         (org-export-as 'md))
        ((memq major-mode '(web-mode html-mode))
         (concat (buffer-substring-no-properties (point-min) (point-max)) "<!-- iframe -->"))
        (t (buffer-substring-no-properties (point-min) (point-max)))))

(defun maple-preview:send-content()
  "Send content to server with delay time."
  (if (> maple-preview:delay 0)
      (unless maple-preview:sending
        (setq maple-preview:sending t)
        (run-with-idle-timer
         maple-preview:delay nil
         (lambda()
           (maple-preview:send-to-server)
           (setq maple-preview:sending nil))))
    (maple-preview:send-to-server)))

(defun maple-preview:send-to-server (&optional ws _string)
  "Send STRING the `maple-preview' preview to WS clients."
  (when (and (bound-and-true-p maple-preview-mode)
             (member major-mode maple-preview:allow-modes))
    (let ((text-content-func (cdr (assoc major-mode maple-preview:text-content))))
      (unless text-content-func
        (setq text-content-func (cdr (assoc t maple-preview:text-content))))
      (process-send-string (or ws maple-preview:websocket)
                           (maple-preview:websocket-text
                            (concat (maple-preview:position-percent) (funcall text-content-func)))))))

(defun maple-preview:websocket-text(text)
  "Decode websocket TEXT,ws-web-socket-frame utf-8 is unsupported."
  (websocket-encode-frame
   (make-websocket-frame :opcode 'text
                         :payload (encode-coding-string
                                   text 'raw-text)
                         :completep t)
   nil))

(defun maple-preview:init-server()
  "Init server."
  (unless maple-preview:server
    (setq maple-preview:server
          (ws-start
           (lambda (request)
             (with-slots (process headers) request
               (if (ws-web-socket-connect request 'maple-preview:send-to-server)
                   (prog1 :keep-alive (setq maple-preview:websocket process))
                 (let ((path (substring (cdr (assoc :GET headers)) 1)))
                   (catch 'close-connection
                     (cond ((string= path "favicon.ico")
                            (ws-send-404 process))
                           ((string= path "preview")
                            (ws-response-header process 200 '("Content-type" . "text/html"))
                            (ws-send process (maple-preview:template)))
                           ((string-prefix-p "preview/" path)
                            (ws-send-file
                             process
                             (expand-file-name (string-trim-left path "preview/") maple-preview:home-path)
                             (maple-preview:mime-type path)))
                           ((ws-in-directory-p default-directory path)
                            (ws-send-file
                             process
                             (expand-file-name path default-directory)
                             (maple-preview:mime-type path)))
                           (t (ws-send-404 process))))))))
           maple-preview:port nil
           :host maple-preview:host
           ;; name is unvalid
           :name "maple-preview-server"))))

(defun maple-preview:listen()
  "Get listen address."
  (unless maple-preview:server
    (error "There is no listen address"))
  (format "%s:%s" maple-preview:host
          (if (booleanp maple-preview:port)
              (process-contact (ws-process maple-preview:server) :service t)
            maple-preview:port)))

(defun maple-preview:open-browser ()
  "Open browser."
  (browse-url
   (format "http://%s/preview" (maple-preview:listen))))

(defun maple-preview:init ()
  "Preview init."
  (maple-preview:init-server)
  (when maple-preview:browser-open (maple-preview:open-browser))
  (when maple-preview:auto-update
    (add-hook 'post-self-insert-hook #'maple-preview:send-content)
    (run-hooks 'maple-preview:auto-hook))
  (add-hook 'after-save-hook #'maple-preview:send-content))

(defun maple-preview:finalize ()
  "Preview close."
  (setq maple-preview:sending nil)
  (when maple-preview:server
    (ws-stop maple-preview:server)
    (setq maple-preview:server nil))
  (when maple-preview:websocket
    (setq maple-preview:websocket nil))
  (remove-hook 'post-self-insert-hook 'maple-preview:send-content)
  (remove-hook 'after-save-hook 'maple-preview:send-content))

;;;###autoload
(defun maple-preview-cleanup ()
  "Cleanup `maple-preview' mode."
  (interactive)
  (maple-preview:finalize)
  (run-hooks 'maple-preview:finialize-hook))

;;;###autoload
(define-minor-mode maple-preview-mode
  "Maple preview mode."
  :group      'maple-preview
  :init-value nil
  :global     t
  (if maple-preview-mode (maple-preview:init) (maple-preview:finalize)))

(provide 'maple-preview)
;;; maple-preview.el ends here
