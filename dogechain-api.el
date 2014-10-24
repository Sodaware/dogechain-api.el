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
  (dogechain-api--get-simple "addressbalance" address))

(defun dogechain-api--get-simple (method &rest params)
  "Get a simple result from the chain."

  ;; http://dogechain.info/chain/Dogecoin/q/addressbalance/DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e
  )


(defun dogechain-api--build-simple-endpoint (method &rest params)
  "Create the address endpoint for a simple call to METHOD with optional PARAMS."
  (let ((query-string ""))
    (when (not (null params))
      (setq query-string (mapconcat 'identity params "/")))
    (format "%s%s%s/%s"
            dogechain-api-endpoint
            dogechain-api-simple-endpoint
            method
            query-string)))

(provide 'dogechain-api)
;;; dogechain-api.el ends here
