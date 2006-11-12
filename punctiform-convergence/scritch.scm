;; -*- mode: scheme -*-
;;
;; formless scratches in space
;;
;; copyright (C) 2004 FoAM vzw
;;  You are granted the rights to distribute and use this software 
;;  under the terms of the GNU Lesser General Public License as 
;;  published by the Free Software Foundation; either version 2.1 of
;;  the License, or (at your option) any later version. The LGPL is
;;  distributed with this code (see: LICENCE) and available online 
;;  at http://www.gnu.org/copyleft/lesser.html

;; authors
;;  - nik gaffney <nik@fo.am>

;; requirements
;;  - space
;;  - something to scratch with

;; commentary
;;  - scritch, scratch. ..


(module scritch mzscheme
  
  (require "graph.scm"
           "snipets.scm"
           (lib "misc.ss" "swindle") 
           (lib "mred.ss" "mred")
           (lib "class.ss"))

  (provide (all-defined))
  
  ;; quik'n dirty debuggin'
  (define (debug level fstring . fargs)
    (let ([debugging #t] ; toggle #t/#f
          [debug-level 1]) ; higher is more info
      (if (and debugging (>= debug-level level))
          (if (list? fargs)
              (apply printf (cons fstring fargs))
              (printf fstring fargs)))))  



) ;; end of module
           
