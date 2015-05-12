;; Copyright (c) 2015, Realworld Systems
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.

;; * Neither the name of smallworld-tramp nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'gis)                          ; The GIS library is required

(defun gis-tramp-find-alias-index (args &optional index)
  "Finds the position of the alias file, based on the arguments list. The
position of the alias file should be after the '-a' position"

  ;; See if the alias file can be found, as it contains the information of
  ;; the command line to execute. As this contains the tramp directive,
  ;; this part needs to be split.

  (when (eq index nil) (setq index 0))

  (message "Attempt to find '-a' in %S" (prin1-to-string args))
  (let ((continue (not (eq args '()))))
    (if continue
	(progn
	  (message "Test car of args %S" (car args))
	  (if (string= (car args) "-a")
	      (+ 1 index)
	    (gis-tramp-find-alias-index (cdr args) (+ 1 index)))))))

(defun gis-tramp-flat-user (user-or-nil)
  "This returns a string for the user to login (user@) or nil, to be
able to reconstruct the tramp string"
  (if (eq user-or-nil nil)
      nil
    (concat user-or-nil "@")))

(defun gis-tramp-get-front (orig-path)
  "This is a function taking any path, and returning, if tramp
compatible, the tramp line separated with the normal path line"
  
  (condition-case nil
      (let ((v (tramp-dissect-file-name orig-path)))
	(message "GIS path is a tramp path")
	(list (concat "/"
		      (elt v 4)
		      (elt v 0) ":" (gis-tramp-flat-user (elt v 1)) (elt v 2) ":")
	      (elt v 3)))
    (error nil)))

(defun gis-tramp-rewrite-path (orig-path)
  "Rewrites the original path, if it is a tramp path and returns the rewritten path
into a tuple (tramp path + real path)"
  (message "Rewrite original path: %S" orig-path)
  (let ((parts (gis-tramp-get-front orig-path)))
    (if (eq parts nil)
	`(() ,orig-path)
      parts)))
  

(defun gis-tramp-inject-path (arguments index-of-alias new-path)
  "Injects the path into the proper position of arguments"
					
  (let ((offset 0)                      ; This is the start index
	(results '()))                  ; Contains the resulting list
    (while arguments

      ;; If the offset equals the index-of-alias, new-path
      ;; needs to be inserted on that position, otherwise, the current
      ;; argument (car of arguments) needs to be inserted

      (setq results
	    (if (eq offset index-of-alias)
		(cons new-path results)
	      (cons (car arguments) results)))
					
      (setq offset (1+ offset))         ; Increment current index
      (setq arguments (cdr arguments))) ; Decrease the arguments
    
    ;; Returns the results, which should be reversed due to consing
    ;; cdrs as cars
    (reverse results)))

(defun gis-tramp-start-file-process (new-path &rest args)
  "start-process will defer to this function to perform the actual logic.
On *nix systems, there is a need to append -i, if the first argument of
the command line is gis. so args has to be split into the first three,
then '-i' and the rest of the arguments"
  (let ((default-directory (car new-path))
	(process nil))
    (message "Starting gis with default directory: %S" default-directory)
    (if (and (not (or (eq system-type 'windows-nt) (or (eq system-type 'ms-dos))))
	     (string= (nth 2 args) "gis"))
	(setq process (apply 'start-file-process
			     (nth 0 args) (nth 1 args) (nth 2 args) "-i" (nthcdr 3 args)))
      (setq process (apply 'start-file-process args)))
    (message "GTSF Process %S" process)
    process))

(defun gis-tramp-process-p (arguments)
  "Tests if the process initiated is a gis process"
  ;;
  ;; It would be nice if there would be knowledge that this has been 
  ;; started from the gis-start-process call
  ;;
  (if (> (length arguments) 3)
      (if (and (string= (car arguments) "gis-process")
	       (string= (nth 2 arguments) "gis"))
	  t)))

(defadvice start-process (around smallworld-tramp-start-process-around)
  "This advice is defined around the start process. It uses the 
current-call-to-start-process-is-gis-p to know whether or not a 
gis-process is intended, this needs a redesign. However, basic 
functionality now works."
  
  (if (gis-tramp-process-p (ad-get-args 0))
      (let* ((arguments (ad-get-args 2))
	     (index-of-alias (gis-tramp-find-alias-index arguments))
	     (orig-path (nth index-of-alias arguments))
	     (new-path (gis-tramp-rewrite-path orig-path))
	     (current-call-to-start-process-is-gis-p nil)
	     (remote-path (car (cdr new-path)))
	     (arg-copied (gis-tramp-inject-path arguments index-of-alias remote-path))
	     (process nil))
	(message "First argument: %S" (ad-get-arg 0))
	(setq process (apply 'gis-tramp-start-file-process new-path (ad-get-arg 0) (ad-get-arg 1) arg-copied))
	(message "SP Process %S" process)
	(setq ad-return-value process))
    ad-do-it))


(ad-activate 'start-process)

(provide 'gis-tramp)
