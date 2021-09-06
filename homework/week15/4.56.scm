;;; (a
(and 
(supervisor ?who (Bitdiddle Ben))
(address ?who ?where))

;;; (b
(and
(salary (Bitdiddle Ben) ?ben_amount)
(salary ?who ?amount)
(lisp-value < ?amount ?ben_amount))

;;; (c
(and 
(supervisor ?who ?boss)
(not (job ?boss (computer . ?what)))
(job ?boss ?job))