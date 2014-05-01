;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROBOTS.lisp						    ;;
;; 							    ;;
;; Play the classic game of Robots! All robots move towards ;;
;; the player. Robot collisions cause scrap that is deadly  ;;
;; to other robots. Teleport as a last resort!		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun robots ()
  ;;          by naming it main, we can use "return from" to exit early
  (loop named main
        with directions = '((q . -65) (w . -64) (e . -63) (a . -1) ;; these are the 8 offsets when the
                            (d .   1) (z .  63) (x .  64) (c . 65));; game board is 64 wide
        for pos = 544
        then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                    (force-output)
                    (let* ((c (read))
                           (d (assoc c directions)));; assoc performs a lookup
                      (cond (d (+ pos (cdr d)))
			    ;; the game board is 64x64=1024, (random 1024) picks a random spot
                            ((eq 't c) (random 1024))
                            ((eq 'l c) (return-from main 'bye));; player wants to leave the game
                            (t pos))))
                          ;; change for more robots!
     for monsters = (loop repeat 10
		             collect (random 1024))
        then (loop for mpos in monsters
                   collect (if (> (count mpos monsters) 1)
			       mpos
			       ;; we sort locations based on distance to player
			       ;; then chomp off the closest
                               (cdar (sort (loop for (k . d) in directions
					      for new-mpos = (+ mpos d)
					      ;; this calculates the "manhattan distance"
					      ;; to the player
                                                 collect (cons (+ (abs (- (mod new-mpos 64) 
                                                                          (mod pos 64)))
                                                                  (abs (- (ash new-mpos -6)
                                                                          (ash pos -6))))
                                                               new-mpos))
                                           '<
                                           :key #'car))))
       ;; check if all monsters are scrap
        when (loop for mpos in monsters
                   always (> (count mpos monsters) 1)) ;; more than 1 robot in a spot means they're scrap
        return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"
                   (loop for p
			;; loop through board positions
                         below 1024
		      collect (cond ((member p monsters)
				     ;; bad news if player in the same spot as robot
				     (cond ((= p pos) (return-from main 'player-loses))
					   ;; draw scrap for doubled-up robots
					   ((> (count p monsters) 1) #\#)
					   (t #\A)))
				    ((= p pos) 
				     #\@)
				    (t 
				     #\ ))))))
