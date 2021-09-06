(rule (big-shot ?person ?division)
	(and (job ?person (?division . ?x))
		 (not (and (supervisor ?person ?boss)
				   (job ?boss (?division . ?y))))))
		 
	 

