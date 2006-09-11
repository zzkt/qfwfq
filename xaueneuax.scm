;; -*- mode: scheme -*-
;;
;; x a u e n e u a x  - nqdataflow
;;
;; copyright (C) 2004 FoAM vzw
;;  You are granted the rights to distribute and use this software
;;  under the terms of the Lisp Lesser GNU Public License, known 
;;  as the LLGPL. The LLGPL consists of a preamble and the LGPL. 
;;  Where these conflict, the preamble takes precedence. The LLGPL
;;  is available online at http://opensource.franz.com/preamble.html 
;;  and is distributed with this code (see: LICENCE and LGPL files)

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
;;   n - adds new node, connected to a selected node
;;   c - conencts 2 selected nodes
;;   d - disconnects selected node
;;   delete - deletes node
;;   enter - evaluates current node 

;; to do
;;  - deal with evaluation order display
;;  - check directions of node connection with 'c'
;;  - multiple connections -> clarify
;;  - deal with circularity

;; changes
;;  2006-09-11
;;  - scraped into coherence from various sources

(require (lib "graph.ss" "mrlib")
         "qfwfq.scm") 

(define xauen-pasteboard%
  (class graph-pasteboard%
    ;; should probably figure out how keymaps work,. 
    (define/override (on-char event)
      (temp-keymap event)
      (super on-char event))
    (super-new)))

;; setup frame and windows.. 
(define f (new frame% [label " } x a u e n e u a x { "]))

(define p (new xauen-pasteboard%))
(define ec (new editor-canvas% (parent f)))

(send ec min-client-width 450)
(send ec min-client-height 450)

(send ec set-editor p)
(define dc (send ec get-dc))

;; text input calllback - spit and polish
;;     beware hardcoded node & pasteboard & lack of error checking
(define (parse-text-input tf event)
  (if (eqv? (send event get-event-type) 'text-field-enter)
      (let ([input (read-from-string (send tf get-value))]
            [node n1]
            [pasteboard p])
        (draw-parse-tree input 1 1 node pasteboard)
        (eval-tree node)
        ;(set-node-text node (eval input))
        (wobble-tree n1 p)))) 

;; textmode input,. .
(define input (new text-field% 
                   [label "inslkon >"] [parent f] [callback parse-text-input]
                   [init-value "(* 3 4 5 (* 7 8 9))"]))

;; keyboard overloading.., 
(define (temp-keymap event)
  (let* ([target p] ;; should get dynamically pasteboard.., 
         [key (send event get-key-code)]
         [selected-snip (send target find-next-selected-snip #f)]
         [Gx (send event get-x)] 
         [Gy (send event get-y)])
    (let-values ([(x y) (send target editor-location-to-dc-location Gx Gy)])
      (debug 1 "key[de]maping->key: ~a ~%" key)
      (debug 1 "selected-snip: ~a ~%" selected-snip)
      (case key
        [(#\n) ;; n fr 'new'
         (debug 1 "add: ~a" key)
         (let ([node (new node-snip%)])
           (send target insert node)
           (if selected-snip
               (add-links node selected-snip))
           ;;(send target move-to node x y)
           )]
        [(#\c) ;; c fr 'connect'
         (let ([next (send target find-next-selected-snip selected-snip)])
           (debug 1 "next-snip: ~a ~%" next)
           (add-links selected-snip next))]
        [(#\d) ;; d fr 'disconnect'
         (let ([next (send target find-next-selected-snip selected-snip)])
           (send selected-snip remove-child next)
           (send selected-snip remove-parent next)
           (send next remove-parent selected-snip)
           (send next remove-child selected-snip))] 
        ))))

;; basic nodewrenching
(define n1 (new output-snip%))
(send p insert n1)

(send p move-to n1 15 15)

(send f show #t)


