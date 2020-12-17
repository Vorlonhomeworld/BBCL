
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

In this code, we define a function called (menz), this is also our last room.  We start with the last room because Common Lisp reads the way we read. It starts at the top and
goes to the bottom. If we were to write the first room first then  the second room second, we'd get an error going from room to to room because the first room wouldn't have
seen the second room and so on. Therefore we start with the last room. We set it up as a function so that we can call it easily from the room before.  The room does nothing
except introduce you the city. Once that's done the program exits normally.

The next function creates the sloping corridor, now, it's called sloping_corridor, but I could have just as easily called it room2, s_room, a, z, 1, 2, it doesn't really
matter to  Common Lisp's interpreter, but it **does** matter  to the human that's debugging it. For that reason, calling it the same name as the room we'll be going in 
makes it easier to find in the code and therefore easier to debug.

This room also uses a prompt and accepts two directions, north and south. Because we want to be safe about the user input, we use (read-line) rather than (read) and 
we use ((string-equal response) to compare the user's entry to one of three possible outcomes. The first outcome would be the user pressing "n", the game would
print a carriage return (format t "~%") then run the defined function (menz) which prints the information for the city of Menzoberazzen. Going south bring the user to the
cave, which is the starting point and defined in the function (cave).  There's a third reponse for everything else, and that's to have the program print a carraige return
and print out "You bang your head against the wall!", then it returns back to the same function it started in , (sloping_cave).

The next function calls our first room (cave). This has only one direction that you can go which is north.  Going north causes the program to print a carriage return then run 
the function (sloping_path). Typing anything else gives you the response "You bang your head against the wall!". 

The last line tells Common Lisp to run the routine (cave) once everything's loaded. This causes the first room to populate and start running. From there this links to another 
room and another one, and so on!  Pretty easy right? You can run this by typing it in to emacs, then run it in the terminal by typing in:

```

sbcl --script mini_adventure.lisp

```

mini_adventure.lisp is the name I have it on on my system, but you can save this under any name you'd like! Run it and you'll see:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/mini_adventure.jpg" height="600" width="1200"></a>

While it runs, this really isn't the best way to code this. For example, did you notice that the prompt has to be recoded each time? Surely there's got to be a way to 
code the prompt just once so that we don't have to keep calling coding it, right?  Tr
