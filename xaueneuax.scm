;; -*- mode: scheme -*-
;;
;; x a u e n e u a x  - nqdataflow
;;  
;; copyright (C) 2004 FoAM vzw
;;  You are granted the rights to distribute and use this software 
;;  under the terms of the GNU Lesser General Public License as 
;;  published by the Free Software Foundation; either version 2.1 of
;;  the License, or (at your option] any later version. The LGPL is
;;  distributed with this code (see: LICENCE) and available online 
;;  at http://www.gnu.org/copyleft/lesser.html

;; authors
;;  - nik gaffney <nik@fo.am>

;; requirements
;;  - uses qfwfq for layout and slipulation

;; commentary
;;  a simple dataflow like visual wrapper to the underlying scheme,
;;  using an evaluation model which is not quite dataflow, yet not 
;;  quite scheme -> nqdataflow
;;
;;  keyboard controls
;;   C-n - adds new node, connected to a selected node
;;   C-c - connects 2 selected nodes
;;   C-d - disconnects selected node
;;   C-l - autolayout from selcted node
;;   delete - deletes node
;;   enter - evaluates current node 

;; to do
;;  - deal with evaluation order display
;;  - check directions of node connection with 'C-c'
;;  - multiple connections -> clarify
;;  - deal with circularity

;; changes
;;  2006-09-11
;;  - scraped into coherence from various sources

(require "qfwfq.scm") 

(define xauen-pasteboard%
  (class graph-pasteboard%
    ;; should probably figure out how keymaps work,. 
    (define/override (on-char event)
      (temp-keymap event)
      (super on-char event))
    (super-new)))

;; setup frame and windows.. 
(define f (new frame% [label " } x a u e n e u a x { "]))

(define mb (instantiate menu-bar% (f)))
(define edit-menu (instantiate menu% ("Edit" mb)))
(define font-menu (instantiate menu% ("Font" mb)))
(append-editor-operation-menu-items edit-menu)
(append-editor-font-menu-items font-menu)

(define p (new xauen-pasteboard%))
(define ec (new editor-canvas% (parent f)))

(send ec min-client-width 450)
(send ec min-client-height 450)

(send ec set-editor p)
(define dc (send ec get-dc))

;; text input callback - spit and polish
;;     beware hardcoded node & pasteboard & lack of error checking
(define (parse-text-input tf event)
  (if (eqv? (send event get-event-type) 'text-field-enter)
      (let ([input (read-from-string (send tf get-value))]
            [node n1]
            [pasteboard p])
        (draw-parse-tree input 1 1 node pasteboard)
        (eval-tree node)
        ;(set-node-text node (eval input))
        ;(shadowpi-tree n1 p 0 50))))
        (circles-tree n1 p 100 100 0 6.28 10)))) 

;; textmode input,. .
(define input (new text-field% 
                   [label "inslkon >"] [parent f] [callback parse-text-input]
                   [init-value "(* 3 4 5 (* 7 (+ 1 1 1) 9))"]))

;; keyboard overloading.., 
(define (temp-keymap event)
  (let* ([target p] ;; should get dynamically pasteboard.., 
         [key (send event get-key-code)]
         [selected-snip (send target find-next-selected-snip #f)]
         [Gx (send event get-x)] 
         [Gy (send event get-y)])
    (let-values ([(x y) (send target editor-location-to-dc-location Gx Gy)])
      (debug 3 "key[de]maping->key: ~a ~%" key)
      (debug 1 "selected-snip: ~a ~%" selected-snip)
      (if (send event get-control-down)
          (case key
            [(#\n) ;; C-n fr 'new'
             (debug 1 "add: ~a" key)
             (let ([node (make-node-snip)])
               (send target insert node)
               (if selected-snip
                   (begin (add-links node selected-snip)
                          ;; re.colour the tree, first [grand]child should do... 
                          (colour-tree (car (send selected-snip get-children)) p)))
               (send target move-to node x y)
               )]
            [(#\c) ;; C-c fr 'connect'
             (let ([next (send target find-next-selected-snip selected-snip)])
               (debug 1 "next-snip: ~a ~%" next)
               (add-links selected-snip next))]
            [(#\d) ;; C-d fr 'disconnect'
             (let ([next (send target find-next-selected-snip selected-snip)])
               (send selected-snip remove-child next)
               (send selected-snip remove-parent next)
               (send next remove-parent selected-snip)
               (send next remove-child selected-snip))]
            [(#\z) ;; C-z re.colour
             (colour-tree selected-snip p)]
            [(#\l) ;; C-l re.lapse -> splay
             (shadowpi-tree selected-snip p 0 63)]
            [(#\=) ;; C-= zoom->out
             (send p zoom 1.1)]
            [(#\-) ;; C-- zoom->in
             (send p zoom 0.9)]
            )))))

;; basic nodewrenching
(define n1 (new output-snip%))
(send p insert n1)
(send p move-to n1 15 15)

;; test a recursive node
;; (define r1 (new recursive-snip%))
;; (send p insert r1)
;; (define n2 (new output-snip%))
;; (send r1 insert n1)

(send f show #t)


