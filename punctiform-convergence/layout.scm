;; -*- mode: scheme -*-
;;
;; basic layout attmepts
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
;;  - 

;; commentary
;;  - wobble -> heirarchical rectangular spread 
;;  - shuffle -> randomise positions    
;;  - relax -> pseudo stabilisation using edge lengths 
;;  - shadowpi -> variation on circular parent-centric splay

;; changes
;;  2006-09-11
;;  - scraped into coherence from various sources
;;  2006-11-12
;;  - mottled shadows, multiple beginings


(module layout mzscheme
  (require (lib "misc.ss" "swindle")
           (lib "class.ss")
           (lib "list.ss")
           (lib "math.ss")
           "scritch.scm"
           "snipets.scm"
           "graph.scm")
           
  (provide wobble-tree
           shuffle-tree
           relax-tree
           shadowpi-tree)

  
  ;;;;;; ; ; ;;    ;;;; ;;    ;  ;; 
  ;;
  ;;  traverse from a node, and  s p  r   e   a  d
  ;;
  ;;;;; ; ;  ;    ;       ;
  
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

  
  ;;;;;;;;;;; ;;     ; ;;     ;
  ;;
  ;; autoslonk
  ;;
  ;;;;; ;   ;; ;;    ;   ;
  
  (define (shuffle-tree node pb x y)
    (let ([parents (send node get-parents)]
          [x1 200]
          [y1 200])
      (debug 2 "shuffling: ~a ~%" parents)
      (send pb move node (random x1) (random y1))
      (cond 
       ((= 1 (length parents))
        (shuffle-tree (car parents) pb (random x1) (random y1)))
       ((< 1 (length parents))
        (for-each 
         (lambda (parent)
           (shuffle-tree parent pb (random x1) (random y1)))
         parents)))))

  
  ;;;;; ;;;;;; ;;     ; ;;     ;
  ;;
  ;; energy stabilisation
  ;; - single itteration only, call as reqd.
  ;; - local epsilon & delta only
  ;; 
  ;;;;; ;  ;;;     ;     ;
    
  (define (relax-tree node pb x y)
    (let ([parents (send node get-parents)])
      (debug 2 "minimising: ~a ~%" node)
      ;; move given node
      (send pb move node x y)
      ;; stabilise distance between siblings
      (if (not (empty? parents))
          (for-each 
           (lambda (parent)
             (let ([siblings (send parent get-children)])
               (if(< 1 (length siblings))
                  (send pb move node
                        (shuffle-x node (random-ref siblings)) 
                        (shuffle-y node (random-ref siblings)))))) parents))     
      ;; stabilise distance betwen node and parents
      (cond 
       ((= 1 (length parents))
        (debug 1 "distance between ~a and ~a is ~a~%" 
               node (car parents) (distance node (car parents)))
        (let ([parent (car parents)])
          (relax-tree parent pb 
                      (shuffle-x node parent)
                      (shuffle-y node parent))))
       ((< 1 (length parents))
        (for-each 
         (lambda (parent)
           (debug 1 "distance between ~a and ~a is ~a~%" node parent (distance node parent))
           (relax-tree parent pb 
                       (shuffle-x node parent) 
                       (shuffle-y node parent))) parents)))))
  
  (define (random-ref l)
    (list-ref l (random (length l))))
  
  (define (shuffle-x n1 n2)
    (let ([x1 (snip-x n1)]
          [x2 (snip-x n2)]
          [epsilon 100]
          [d 10])
      (if (> epsilon (distance n1 n2))
          (if (= x2 (max x1 x2)) ;; move outward
              (random d) 
              (-ve (random d)))
          (if (= x2 (max x1 x2)) ;; move inward
              (-ve (random d)) 
              (random d))))) 
  
  (define (shuffle-y n1 n2)
    (let ([y1 (snip-y n1)]
          [y2 (snip-y n2)]
          [epsilon 100]
          [d 10])
      (if (> epsilon (distance n1 n2))
          (if (= y2 (max y1 y2)) ;; move outward
              (random d) 
              (-ve (random d)))
          (if (= y2 (max y1 y2)) ;; move inward
              (-ve (random d)) 
              (random d))))) 
  
  (define (distance n1 n2)
    (let ([x1 (snip-x n1)]
          [x2 (snip-x n2)]
          [y1 (snip-y n1)]
          [y2 (snip-y n2)])
      (sqrt (+ (sq (abs (- x1 x2)))
               (sq (abs (- y1 y2)))))))
  
  (define (sq n)
    (* n n))
  
  (define (-ve n)
    (- 0 n))

  ;;;;;;;;;;; ;;     ; ;;     ;
  ;;
  ;; circular parent centric layout, in the shade of twopi
  ;;
  ;; ref: arxiv:cs.HC/0606007 v1 -> "A parent-centered radial layout algorithm
  ;; for interactive graph visualization and animation" by Andrew Pavlo,
  ;; Christopher Homan & Jonathan Schull
  ;;
  ;; - since each node may have any number of parents, yet only a single child,
  ;; thi sinterpretation of the algorithm is essentially the same, if 'children'
  ;; is read as 'parents'
  ;; 
  ;;;;; ;  ;;;     ;     ;
  
  (define twopi (* 2 pi))

  
  (define (shadowpi-tree node pb x y t1 t2 r)
    ;; node from whcih to draw the layout, centred at x,y on the containment arc
    ;; given by t1->t2 with radius, r
    (let* ([parents (reverse (send node get-parents))]
           [n (length parents)]
           [xi (snip-x node)] 
           [yi (snip-y node)]
           (phi 0.75))
      
      ;; distribute parents of given node evenly along a containment circle
      ;; centered on the node.
      
      (dotimes (k n)
               (let ((parent (list-ref parents k))
                     (x1 (* r (cos (* (+ t1 (- t2 t1)) (/ k n)))))
                     (y1 (* r (sin (* (+ t1 (- t2 t1)) (/ k n))))))
                 (send pb move-to parent
                       (+ xi x1) (+ yi y1))
               
                 ;; draw circles around the node’s parents and evenly distribute their
                 ;; parents along containment arcs. 

                 (debug 1 "----~a~% x1:~a~% y1:~a~% t1':~a~% t2':~a~%"
                        parent
                        x1 y1
                        (- (/ pi 2) (atan (/ (- yi y1) (- xi x1))))
                        (+ (/ pi 2) (atan (/ (- yi y1) (- xi x1)))))
                 
                 (shadowpi-tree parent pb
                                (+ xi x1) (+ yi y1) ;; centred on
                                
                                (- (atan (/ (- yi y1) (- xi x1))) (/ pi 2))
                                (+ (/ pi 2) (atan (/ (- yi y1) (- xi x1))))
                                 
                                (* r phi)) ;; radius
              
                 ;; this proceeds recursively, so that successively distant descendants of
                 ;; the goven node are positioned on successively smaller containment arcs.

                 ))))
  

  ) ;; end of module
