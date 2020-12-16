
# More inputs

I thought we might have a little bit of fun while we're coding and write up an adventure game. I'm not kidding, we're really going to code an adventure game right now!
Why not, it's going to use some of the things we've learned already like (format t) functions and (read-line) and string reading functions. Plus all of these skills can
be used in more *serious* projects later on!

Now this isn't going to be much, it's three rooms, no monsters, no combat, just you on a mini dungeon crawl! Be aware this code is *really* basic and we'll be refining
this code as we go along to make it easier to code and debug!   With that in mind, here's the first draft:

```

(defun menz ()
  (format t "~%~% You exit out of  the corridor into the largest cave you've ever seen! It's literally as large as a city, a big city! You see creatures that appear to be elves, except that their skin is onyx black in color. The women appear to be taller than the men and stronger as well!   You also see unknown creatures that look like a cross between one of  the black-colored elves and a spider! This city fills you with a feeling of dread! ~%~%")
  (format t "Suddenly,  you realize where you are .... you have found ~%~% The Menzoberazzen!!~%~%"))





(defun sloping_path ()
  (format t "~%~%You're on a path sloping down to the north. Strangely enough, the path is well lit from torches mounted along the path. You can hear voices and smell cooking  to the north.~%")
  (let ((choice 1))
    (format t " ~% Enter command:  ")
    (let ((response (read-line)))
      (cond ((string-equal response "n")
	     (format t "~%")(menz))
	    ((string-equal response "s")
	    (format t "~%")(cave))
	    (t (format t "~% You bang your head against the wall!~%~%")(sloping_path))))))
	     
	  




(defun cave ()
  (format t "~%~%You wake up on the floor of a deep dark cave. Above you, way out of reach, is the hole you fell through. The only path available to you is a path to the north that goes down further into the cave~%")
  (let ((choice 1))
    (format t "~%Enter command:    ")
    (let ((response (read-line)))
      (cond ((string-equal response "n")
	     (format t "~%")(sloping_path))
	    (t (format t "~% You bang your head against the wall!~%")(cave))))))

(cave)

```

You can save this, then run it in the terminal by typing in:

```

sbcl --script mini_adventure.lisp

```

(mini_adventure.lisp) is the name I have it on on my system, but you can save this under any name you'd like! Run it and you'll see:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/mini_adventure.jpg" height="600" width="1200"></a>


