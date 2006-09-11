;; -*- mode: scheme -*-
;;
;; a simple setup for testing ideas about visual programming
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
;;  - based on the MrEd environment for PLT scheme
;;  - uses MrLib for graph display and editing

;; commentary
;;  aims to provide a simple framework for testing VPL models, ideas
;;  or techniques. this code is based around the implicit assumption
;;  that a VPL will be a graph based representation which is mapable
;;  to a sexp (or colelction of sexps), so some things should be 
;;  reasonably common between different (although admittedly graph 
;;  based) VPLs, for example tree traversal, node placement and layout.
;;  it is not intended to be complete, exhaustive, or stable.

;; changes
;;  2006-09-11
;;  - scraped into coherence from various sources

(module qfwfq mzscheme
  (require (lib "graph.ss" "mrlib")
           (lib "class.ss")
           (lib "list.ss")         
           (lib "string.ss")
           (lib "math.ss")
           (lib "mred.ss" "mred")
           (lib "contract.ss")) 
  
  (provide graph-pasteboard% 
           node-snip% 
           output-snip%           
           insert-nodes
           set-node-text
           get-node-text
           set-node-value
           get-node-value
           draw-parse-tree
           wobble-tree
           eval-tree
           tree->sexp
           to-string
           debug)
  
  ;; quik'n dirty debuggin'
  (define (debug level fstring . fargs)
    (let ([debugging #t] ; toggle #t/#f
          [debug-level 1]) ; higher is more info
      (if (and debugging (>= debug-level level))
          (printf fstring fargs))))  
  
  ;;;;;;;;; ;; ;;     ;; 
  ;;
  ;; graphs, graphing and graphics
  ;;
  ;;;;;; ; ;;; ; ;;; ; ;       ; 
  
  (define graph-pasteboard%
    (class (graph-pasteboard-mixin pasteboard%)
      (define/augment (on-delete snip)
        ;; remove from parent list of children
        (for-each (lambda (child)
                    (send child remove-parent snip))
                  (send snip get-children))
        ;; remove from child list of parents
        (for-each (lambda (parent)
                    (send parent remove-child snip))
                  (send snip get-parents)))
      (super-new)))
  
  ;; nodes can contain any valid expression, which is stored as text [for now]
  ;; able to be read by read-string.  the field 'value' may contain a precomputed
  ;; value of the nodes subtree, and the 'dirty' flag indicates wheter the subtree
  ;; (ie. any of its children) have changed.
  
  (define node-snip%
    (class (graph-snip-mixin editor-snip%)
      (init-field (value ()))
      (init-field (dirty #f))
      (public set-value besmirch clean)
      (define (set-value v)
        (set! value v))
      (define (besmirch)
        (debug 2 "smirched: ~a ~%" this)
        (set! dirty #t)
        (map (lambda (x) (send x besmirch))
             (send this get-children)))
      (define (clean)
        (set! dirty #f))
      ;; should be more coarse grained than 'on-event', but what?+
      (define/override (on-char dc x y editorx editory event)
        (if (eqv? (send event get-key-code) #\return)
            (begin (send event set-key-code #\nul)
                   (besmirch))
            (set! dirty #t))
        (super on-char dc x y editorx editory event))
      (define/override (own-caret own-it?)
        (if own-it?
            (debug 1 "node: ~a got keybrd focus~%" this)
            (if dirty 
                (begin (debug 1 "node: ~a lost keybrd focus~%" this)
                       (send this besmirch)
                       (send this clean))))
        (super own-caret own-it?))
      (super-new)))
  
  ;; an output snip will display or modify its contents when besmirched.. .
  (define output-snip%
    (class node-snip%
      (define/override (besmirch)
        (send (send this get-editor) erase)
        (eval-tree this))
      (super-new)))
  
  ;; node/graph utils,.
  (define (insert-nodes p . nodes)
    (for-each (lambda (x) (send p insert x)) nodes))
  
  ;; insert non specific data into a node's text-field%
  (define (set-node-text node data)
    (send (send node get-editor) 
          insert (to-string data)))
  
  ;; get the text from a node
  (define (get-node-text node)
    (send (send node get-editor) get-text))
  
  ;; partial [e]valuation
  (define (set-node-value node value)
    (send node set-value value))
  
  (define (get-node-value node)
    (send node value))
  
  ;; decor
  ;; brushes/ pens see -> 6.15  pen%
  ;; colours -> 6.7  color-database<%>
  
  ;; function links -> active/inactive 
  (define pen1 (send the-pen-list find-or-create-pen "Red" 1 'solid))
  (define brush1 (send the-brush-list find-or-create-brush "orange" 'solid))
  (define pen2 (send the-pen-list find-or-create-pen "DarkSeaGreen" 1 'solid))
  (define brush2 (send the-brush-list find-or-create-brush "Gold" 'solid))
  ;; data links -> active/inactive 
  (define pen3 (send the-pen-list find-or-create-pen "orange" 1 'solid))
  (define brush3 (send the-brush-list find-or-create-brush "yellow" 'solid))
  (define pen4 (send the-pen-list find-or-create-pen "DarkSeaGreen" 1 'solid))
  (define brush4 (send the-brush-list find-or-create-brush "Beige" 'solid))
  
  ; convert given object to string
  (define (to-string x)
    (cond ((string? x) x)
          ((char? x) (list->string (list x)))
          ((number? x) (number->string x))
          ((symbol? x) (symbol->string x))
          ((list? x) (apply string-append (map to-string x))) 
          (else (error "don't know how to convert to string: " x))))
  
  ;;;;;;;;; ; ;   ;;      ;
  ;;
  ;; re-traversal
  ;;
  ;;;;;;;;; ;;  ;;  
  
  ;; build a tree from a given list, starting from the node% parent, 
  ;; drawing to the graph-pasteboard% pb .. .
  (define (draw-parse-tree tree x y parent pb)
    (if (list? tree)
        (begin
          (let ((size 5)
                (root (car tree))
                (node (new node-snip%)))          
            (debug 2 "root: ~a ~%" root)         
            (draw-parse-tree root x y parent pb)
            ;; function node  
            (send pb insert node)
            (add-links node parent pen1 pen2 brush1 brush2)
            (send (send node get-editor) insert (to-string root))
            ;; subtrees, or args
            (for-each
             (lambda (child)        
               (if (list? child) 
                   (draw-parse-tree child x y node pb)
                   (let ((sibling (new node-snip%)))         
                     (send pb insert sibling)
                     (add-links sibling node pen3 pen4 brush3 brush4)
                     (send (send sibling get-editor) insert (to-string child)))))
             (cdr tree))))))
  
  ;; basic layout attmepts
  
  ;;  traverse from a node, and s p  r   e   a  d  
  (define (wobble-tree node pb)
    (let* ([parents (reverse (send node get-parents))]
           [n (length parents)]
           [x1 50] 
           [y1 30])
      (debug 2 "~% node.~a " node)
      (debug 2 "~% parents.~a " parents)
      (send pb move node x1 y1) 
      (do ((i 0 (+ i 1)))
        ((= i n))
        (debug 2 ".~a." i)     
        (let* ([parent (list-ref parents i)]
               [nx (box 0)] 
               [ny (box 0)]
               [loco (send pb get-snip-location node nx ny)])
          (debug 2 ".[~a,~a]." (unbox nx) (unbox ny))    
          (send pb move-to parent 
                (+ (* i x1) (/ (unbox nx) n)) 
                (+ y1 (unbox ny)))
          (wobble-tree parent pb)))))
  
  ;; autoslonk
  (define (shuffle-tree node pb x y)
    (let ([parents (send node get-parents)]
          [x1 200]
          [y1 200])
      (debug 2 "grinding: ~a ~%" parents)
      (send pb move node (random x1) (random y1))
      (cond 
        ((= 1 (length parents))
         (shuffle-tree (car parents) pb (random x1) (random y1)))
        ((< 1 (length parents))
         (for-each 
          (lambda (parent)
            (shuffle-tree parent pb (random x1) (random y1)))
          parents)))))
  
  ;; eval [sub]graph from a node. ..
  ;;  absolutely no chekcing or error handling yet.
  (define (eval-tree node)
    (set-node-text node 
                   (eval (tree->sexp (car (send node get-parents))))))
  
  ;; traverse a tree [or graph] to create a corresponding s-expresion
  ;; doesnt cope with cycles, nor muliple children (if a node has muliple
  ;; children, it is translated into separate expressions)  
  (define (tree->sexp node)
    (let ([parents (send node get-parents)]
          [data (get-node-text node)]
          [out ()])
      (if (not (null? parents))
          (set! out (cons (read-from-string data) 
                          (reverse (map tree->sexp parents))))
          (set! out (read-from-string data)))
      (debug 1 "tree->sexp: ~a ~%" out)         
      out))
  
  ) ;; end of module
