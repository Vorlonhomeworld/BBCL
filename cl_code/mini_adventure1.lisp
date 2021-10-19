;; create rooms as a global variable - we'll need this later
;; we can't use "room" as this is reserved in SBCL

(defvar rooms ( ) )


;; we need this as we'll be issuing a statement stating room = nextroom to
;; move from room to room

(defvar nextroom () )

;; create exits as a global variable - we'll need this later too

(defvar exits () )

;; create describe as a global variable - we'll refer to this later too
;; we can't use "describe" as this is also reserved in SBCL

(defvar description () )


;; create the prompt - this will be called each time the player needs to take an action

(defun prompt ()
  (read (rooms))
  (read (exits))
  (let ((choice 1))
    (format t "~% >  ")
    (let ((response (read-line)))
      (cond ((string-equal response exits)
	     (if  (exits) (equal exits nextroom)))
	      (t (format t "You can't go that way!"))))))

;; create the Menzoberazzen , no exits here, so exits are set to false

(defun menz ()
  (room (menz))
  (f (exits))
  (description (format t "~%~% You exit out of  the corridor into the largest cave you've ever seen! It's literally as large as a city, a BIG city!
You see creatures that appear to be elves, excep that their skin is as black as onyx in color. The women appear to be 
taller than the men and stronger as well!  You also see unknown creatures that look like a cross between one of  the 
black-colored elves and a spider!  ......This city fills you with a feeling of dread. ~%~%  Suddenly, you realize where 
you are......... you've found  The Meonzoberazzen!! ~%~%")))

;; create the sloping path
;; ther are two exits here - south to (cave) and north to (menz)

(defun sloping_path ()
  (room (sloping_path))
  (exits (n (menz))
	(s (cave)))
  (description (format t "~%~%You're on a path sloping down to the north. Strangely enough, the path is well lit from torches
mounted along the path. You can hear voices and smell cooking to the north.~%"))
 (prompt))

;; create cave - there is one exit here, north to (sloping room)

(defun cave ()
  (room (cave))
  (exits (n (sloping_path)))
  (description (format t "~%~%You wake up on the floor of a deep, dark cave. Above you, way out of  reach is the hole you fell
through. The only path avilable to you is the path to the north that goes down further into the cave.~%"))
 (prompt)

 ;; now create the main part of  the program

(defun main ()
  (room '(cave))
  (description '(room)
  (read exits)
  (prompt))


(main)
