;;; dogechain-api.el --- Library for working with the dogechain API

;; Copyright (C) 2014 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>
;; Version: 0.1.0
;; Package-Requires: ((json "1.2"))
;; Keywords: dogecoin

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

;; dogechain-api provides library functionality for working with the
;; Dogecoin blockchain

;;; Code:

;; Dependencies

(require 'json)
(require 'url-http)

(defvar url-http-end-of-headers)

;; Configuration

(defgroup dogechain-api nil
  "Dogechain api extension"
  :group 'comms
  :prefix "dogechain-api-")

(defconst dogechain-api-endpoint "http://dogechain.info")
(defconst dogechain-api-simple-endpoint "/chain/Dogecoin/q/")


;; Simple API functions

(defun dogechain-api-get-address-balance (address)
  "Get amount ever received minus amount ever sent by ADDRESS."
  (string-to-number (dogechain-api--get-simple "addressbalance" address)))

(defun dogechain-api-address-to-hash (address)
  "Get the public key hash for ADDRESS."
  (dogechain-api--get-simple "addresstohash" address))

(defun dogechain-api-valid-address-p (address)
  "Check ADDRESS for validity."
  (string= "1E" (dogechain-api--get-simple "checkaddress" address)))

(defun dogechain-api-decode-address (address)
  "Get the version prefix and hash encoded in ADDRESS."
  (let* ((result (dogechain-api--get-simple "decode_address" address))
         (parts (split-string result ":")))
    `((,:version . ,(car parts))
      (,:hash . ,(car (cdr parts))))))

(defun dogechain-api-get-block-count ()
  "Get the current block number."
  (string-to-number (dogechain-api--get-simple "getblockcount")))

(defun dogechain-api-get-difficulty ()
  "Get the difficulty of the last solved block."
  (string-to-number (dogechain-api--get-simple "getdifficulty")))

(defun dogechain-api-get-received-by-address (address)
  "Get the total amount ever received by ADDRESS."
  (string-to-number (dogechain-api--get-simple "getreceivedbyaddress" address)))

(defun dogechain-api-get-sent-by-address (address)
  "Get the total amount ever sent by ADDRESS."
  (string-to-number (dogechain-api--get-simple "getsentbyaddress" address)))

(defun dogechain-api-hash-to-address (hash)
  "Convert HASH to an address."
  (dogechain-api--get-simple "hashtoaddress" hash))

(defun dogechain-api-get-total-currency ()
  "Get the total amount of currency ever mined."
  (string-to-number (dogechain-api--get-simple "totalbc")))

(defun dogechain-api-get-transactions ()
  "Get the amount of transactions for every block."
  (let ((result (dogechain-api--get-simple-json "transactions")))
    (mapcar (lambda (data)
              `((,:block . ,(elt data 0))
                (,:timestamp . ,(elt data 1))
                (,:transactions . ,(elt data 2))))
            result)))

(defun dogechain-api-get-network-statistics (&optional interval start stop)
  "Fetch network statistics, optionally arranged by INTERVAL between START and STOP."
  (let* ((interval (if (null interval) 144 interval))
         (start (if (null start) 0 start))
         (stop (if (null stop) -1 stop))
         (response (dogechain-api--get-simple-json "nethash" interval start stop)))
    (mapcar (lambda (data)
              `((,:block . ,(elt data 0))
                (,:timestamp . ,(elt data 1))
                (,:target . ,(elt data 2))
                (,:average-target . ,(elt data 3))
                (,:difficulty . ,(elt data 4))
                (,:hashes-to-win . ,(elt data 5))
                (,:average-interval . ,(elt data 6))
                (,:hashes-per-second . ,(elt data 7))))
            response)))


;; Internal helpers

(defun dogechain-api--get-simple (method &rest params)
  "Get a none-json result from the chain METHOD with optional PARAMS."
  (with-current-buffer (url-retrieve-synchronously (dogechain-api--build-simple-endpoint method params))
    (delete-region (point-min) (+ 1 url-http-end-of-headers))
    (prog1 (buffer-string)
      (kill-buffer))))

(defun dogechain-api--get-simple-json (method &rest params)
  "Call METHOD on the server with optional PARAMS and return result as JSON."
  (let ((request-url (dogechain-api--build-simple-endpoint method params)))
    (with-current-buffer (url-retrieve-synchronously (contact request-url "?format=json"))
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun dogechain-api--build-simple-endpoint (method &optional params)
  "Create the address endpoint for a simple call to METHOD with optional PARAMS."
  (let ((query-string ""))
    (when (not (null params))
      (setq query-string (mapconcat 'identity (remove nil params) "/")))
    (format "%s%s%s/%s"
            dogechain-api-endpoint
            dogechain-api-simple-endpoint
            method
            query-string)))

(provide 'dogechain-api)
;;; dogechain-api.el ends here
