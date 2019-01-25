#!r7rs

;;; A test program for the (discrete powers) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 19, 2018
;;; last revised August 8, 2018

(import (scheme base)
        (discrete testing)
        (discrete powers))

(suite square ()
  (test zero
    (square 0)
    1 (number?)
    zero?)
  (test small-positive
    (square 7)
    1 (number?)
    (lambda (result)
      (= result 49)))
  (test negative
    (square -413)
    1 (number?)
    (lambda (result)
      (= result 170569)))
  (test large
    (square 99999999)
    1 (number?)
    (lambda (result)
      (= result 9999999800000001)))
  (test complex
    (square 3-4i)
    1 (number?)
    (lambda (result)
      (= result -7-24i))))

(suite cube ()
  (test zero
    (cube 0)
    1 (number?)
    zero?)
  (test small-positive
    (cube 7)
    1 (number?)
    (lambda (result)
      (= result 343)))
  (test negative
    (cube -413)
    1 (number?)
    (lambda (result)
      (= result -70444997)))
  (test large
    (cube 170263)
    1 (number?)
    (lambda (result)
      (= result 4935837394381447)))
  (test complex
    (cube 3-4i)
    1 (number?)
    (lambda (result)
      (= result -117-44i))))

(suite zenzizenzic ()
  (test zero
    (zenzizenzic 0)
    1 (number?)
    zero?)
  (test small-positive
    (zenzizenzic 5)
    1 (number?)
    (lambda (result)
      (= result 625)))
  (test negative
    (zenzizenzic -413)
    1 (number?)
    (lambda (result)
      (= result 29093783761)))
  (test large
    (zenzizenzic 85073)
    1 (number?)
    (lambda (result)
      (= result 52380180644444178241)))
  (test complex
    (zenzizenzic 3-4i)
    1 (number?)
    (lambda (result)
      (= result -527+336i))))

(suite sursolid ()
  (test zero
    (sursolid 0)
    1 (number?)
    zero?)
  (test small-positive
    (sursolid 6)
    1 (number?)
    (lambda (result)
      (= result 7776)))
  (test negative
    (sursolid -413)
    1 (number?)
    (lambda (result)
      (= result -12015732693293)))
  (test large
    (sursolid 22044)
    1 (number?)
    (lambda (result)
      (= result 5205374877983015476224)))
  (test complex
    (sursolid 3-4i)
    1 (number?)
    (lambda (result)
      (= result -237+3116i))))
  
;;; Copyright © 2018 John David Stone

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
