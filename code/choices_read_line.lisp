(defun welcome ()
  (format t "~% Status: Logged In   Agent ID: 007   License to Kill:Active     Messages from M: 0~%~%"))


(defun getchoice3 ()
  (let ((choice 1))
    (format t  "~%Enter your password:  ")
    (let ((response (read-line)))
      (cond ((equal response '007)
	     (format t "~%~%Welcome James Bond, 007 ~%")(welcome))
	    (t (format t "~%Incorrect Reponse.~%") (getchoice3))))))

(getchoice3)
