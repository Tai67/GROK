;First of all : you lost the game
;dependences : drakma cl-ppcre


;fonction déterminant une sélection à partir d'un genre
(defun get-titles (genre  max)
	   (cl-ppcre:all-matches-as-strings 
	    (format nil "<a href=\"/title/.{0,~a}\">.*</a>" max) 
	    (drakma:http-request (concatenate 'string "http://www.imdb.com/genre/" genre))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun split-by-one-comma (string)
	   (ppcre:split ", " string))

;fonction déterminant un genre à partir d'un film 
(defun get-genre (film)
	   (let ((gstr
		  (first (ppcre:all-matches-as-strings 
			  "\"Genre\":\".*\",\"Director\""
			  (drakma:http-request
			   (concatenate 
			    'string "http://www.imdbapi.com/?i=&t="
			    (replace-all film " " "+")))))))
	    (subseq gstr 9 (- (length gstr) 12))))

;fonction déterminant une sélection à partir d'un film
(defun selection (film) 
	     (get-titles
	      (first (split-by-one-comma
		      (get-genre film
				 )))
	      30))

;fonction déterminant une sélection à partir d'une liste de film.
; ex: (select-mf (list "equilibrium" "Ice Age" "The Descendants"))(defun select-mf (film-list)
	   (defun select-mf (film-list)
	   (let ((genres nil)
		 (best-value 0)
		 (best-key nil)
		 (classement (make-hash-table :test #'equal)))
	     (dolist (film film-list)
	       (setf genres (split-by-one-comma(get-genre film)))
	       (dolist (genre genres)
		 (if (null (gethash genre classement))
		     (setf (gethash genre classement) 1)
		     (incf (gethash genre classement)))))
	     (maphash (lambda (key value)
			(if (< best-value value)
			    (setf best-value value)
			    (setf best-key key )))
		      classement)
	     (get-titles best-key  20)))

;A partir d'une list de film, crache une liste de titre en rapport avec le genre des fils envoyés
;(imdb-request (list film1 film2 ...))
(defun imdb-request (film-list)
	   (let ((top nil))
	     (dolist (film (select-mf film-list))
	       (let ((nom-film (subseq film 28 (-(length film) 4))))
		 (if (not (find nom-film film-list :test #'equal)) 
		     (push nom-film top))))
	     (reverse top)))

(defun amazon-request (film, foo)
	   (concatenate 'string (concatenate 'string "http://www.amazon.fr/s/ref=nb_sb_noss?__mk_fr_FR=%C5M%C5Z%D5%D1&url=search-alias%3Daps&field-keywords=" (replace-all film " " "+")) "&x=0&y=0"))
