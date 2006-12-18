;; -*- mode: scheme -*-
;;
;; a simple setup for testing ideas about visual programming
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
;;  - based on the MrEd environment for PLT scheme
;;  - uses MrLib for graph display and editing

;; commentary
;;  aims to provide a simple framework for testing VPL models, ideas
;;  or techniques. this code is based around the implicit assumption
;;  that a VPL will be a graph based representation which is mapable
;;  to a sexp (or collection of sexps), so some things should be 
;;  reasonably common between different (although admittedly graph 
;;  based) VPLs, for example graph traversal, node placement and layout.
;;  it is not intended to be complete, exhaustive, or stable.

;; changes
;;  2006-09-11
;;  - scraped into coherence from various sources
;;  2006-11-10
;;  - scattered energy stabilisation attempts
;;  - scattered modules and files


(module qfwfq mzscheme
  (require "punctiform-convergence/graph.scm"
           "punctiform-convergence/eddies.scm"
           "punctiform-convergence/snipets.scm"
           "punctiform-convergence/layout.scm"  
           "punctiform-convergence/scritch.scm")
  
  (provide graph-pasteboard% 
           node-snip% 
           output-snip%
           recursive-snip%
           make-node-snip
           insert-nodes
           add-links
           
           set-node-text
           get-node-text
           set-node-value
           get-node-value
           draw-parse-tree
           colour-tree

           wobble-tree
           relax-tree
           eval-tree
           shadowpi-tree
           
           tree->sexp
           to-string
           debug)
 
  ) ;; end of module
