;; A better version of the adventure game
;; this version incorporates a prompt routine
;; that is written once and called again.

;; First we're going to prepare the room and the
;; description '((room ( description)) is the format
;; for this command

;; Starts out with a defparameter.  Defparameter is a
;; global variable that can be called from anywhere
;; in the program. All defparameter values have names
;; that start with *somename*, i.e (defparameter
;; *count* 0 )

;; We'll call the defparamenter value *roomdesc* because
;; that's what it's going to contain. The next defparameter
;; is going to be called *roomexit* because that's
;; what it will contain

(defparameter *roomdesc* '((menz (You exit out of  the corridor into the largest cave you've ever seen! It's literally as large as a city, a big city! You see creatures that appear to be elves, except that their skin is onyx black in color. The women appear to be taller than the men and stronger as well!   You also see unknown creatures that look like a cross between one of  the black-colored elves and a spider! This city fills you with a feeling of dread! Suddenly,  you realize where you are .... you have found ....  The Menzoberazzen!!))
			   (sloping_path (You're on a path sloping down to the north. Strangely enough, the path is well lit from torches mounted along the path. You can hear voices and smell cooking  to the north.))
			   (cave (You wake up on the floor of a deep dark cave. Above you, way out of reach, is the hole you fell through. The only path available to you is a path to the north that goes down further into the cave.))))

;; now we create a function that describes the rooms
;; we just created. We're going to use cadr here because
;; this refers to the second part of our list. This is
;; what's going to be printed. We won't print "menz",
;; for example, but we'll print "You exit out....")
;; We'll use assoc because we want menz to be associated
;; with the description of the Menzoberazzen, not the
;; sloping cave or the cave !

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;; Now we give the program the room names and the exits
;; it's going to be in the format of:
;; ((current room (next room direction
;; a third item can be added after direction if there's
;; a device that needs to be used, like a door
;; a ladder, a trap door..etc...
;; Once again - we're going to call this roomexits
;; because that's what this shows

(defparameter *roomexits* ((menz ( ))
			   (sloping_path (menz north))
			   (sloping_path (cave south))
			   (cave ( sloping_path north))))


;;Now we need to let Common Lisp know where these
;;exits are and how to read them.

;; First we need to input the exits as part of the
;; path.
  
;; We're going to get Common Lisp to describe them
;; for us on screen we'll be using the cadr function because
;; the cadr return the second item on the first  part of our
;; list. (sloping_path (menz NORTH)). We'll also be using
;; caddr because it will be returning first item
;; of the second list (MENZ north).
;; Our list is named *roomexits* so by using
;; caddr roomexits we're telling to return the first
;; word in the second parantheses!


(defun describe-path (edge)
  '(there is a , (caddr edge) going , (cadr edge) from here.))



  
