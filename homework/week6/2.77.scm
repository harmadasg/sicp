#lang simply-scheme

;;2.77
;Previously the "magnitude" procedure was only defined for the types "rectangular" and "polar".
;In this example Louis tries to call the method with the type "complex", that's why she gets the error.
;Alyssa's solution works because it unwraps the underlying type information from "complex" and calls the "apply-generic" for the second time
; with either the "polar" or the "rectangular" type.