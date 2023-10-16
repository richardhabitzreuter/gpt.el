;;; gpt.el --- An Emacs package for chatting with OpenAI GPT-3 API
;;
;; Copyright (C) 2023 Richard Habitzreuter
;;
;; Author: Richard Habitzreuter <richardhabitzreuter@icloud.com>
;; Gpttainer: Richard Habitzreuter <richardhabitzreuter@icloud.com>
;; Created: March 27, 2023
;; Modified: March 27, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local gptt mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/richardhabitzreuter
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary: An Emacs package for chatting with OpenAI GPT-3 API


;;; Commentary:
;;

;;; Code:

(require 'json)
(require 'url)

;;You can find your api key in https://platform.openai.com/account/api-keys
(setq gpt-apikey "your api key goes here")

(defun get-chat-completions-from-openai (query)
  "Function to retrieve chat completions from OpenAI API.
Argument QUERY."
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (data (json-encode
                `((model . "gpt-3.5-turbo")
                  (messages . [((role . "user") (content . ,query))]))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " gpt-apikey)))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun parse-json-string (string)
 "Function to parse JSON. Argument STRING."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (get-chat-completions-from-openai string)))

(defun get-content-from-parsed-json (string)
"Function to get content from parsed JSON. Argument STRING."
  (plist-get (plist-get (-first-item (plist-get (parse-json-string string) :choices)) :message) :content))

(defun fetch-chatgpt-response ()
  "Fetch ChatGPT for a response."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      ;; Insert content from parsed GPT JSON
      (insert (get-content-from-parsed-json text)))))

;;Binds the function to the shortcut
(global-set-key (kbd "C-c C-p C-r") 'fetch-chatgpt-response)

(provide 'gpt)

;;; gpt.el ends here
