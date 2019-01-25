#!r7rs

;;; A test for the "Hello, world!" library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created July 2, 2018
;;; last revised August 8, 2018

(import (scheme base)
        (discrete testing)
        (discrete greeter))

(suite correct-greeting ()
  (test simple

    ;; Collect the output in a string
    ;; instead of sending it to standard output.

    (let ((out (open-output-string)))  
      (parameterize ((current-output-port out))
        (greeting))

      ;; Recover the output from the string port.

      (let ((result (get-output-string out)))
        (close-port out)
        result))
    1 (string?)
    (lambda (result)
      (string=? result "Hello, world!\n"))))

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; along with this program.
;;; If not, it is available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html.
