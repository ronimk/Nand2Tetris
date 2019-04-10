(defpackage "ASSEMBLER"
		    (:use "PARSER" "CL")
			(:export "ASSEMBLE") )
(in-package "ASSEMBLER")


;;; Functions and data for handling the symbol table:

; A symbol is either a label or a variable name.
; We use symbols to identify certain memory locations
; (in the program's ROM for lables or in the data RAM
; for variables) that the program uses.

; *RAM-TABLE* is an association table that maps
; a variable to its memory location in RAM
(defparameter *RAM-TABLE* nil)
(defparameter *FIRST-FREE-MEMLOC* 16)
(defparameter *LAST-FREE-MEMLOC* 16383)

; *ROM-TABLE* is an association table that maps
; a lable to its memory location in ROM
(defparameter *ROM-TABLE* nil)

;; functionality to handle the ROM- and RAM-tables:

; init-ram-table creates the initial ram-symbol table with
; all the predefined variables and maps them to their memory
; locations. The ra,-table also works as the reserved-keyword
; checker for the translate-label function.
(defun init-ram-table ()
  (setf *RAM-TABLE*
  (acons "SP" 0
  (acons "LCL" 1
  (acons "ARG" 2
  (acons "THIS" 3
  (acons "THAT" 4
  (acons "R0" 0
  (acons "R1" 1
  (acons "R2" 2
  (acons "R3" 3
  (acons "R4" 4
  (acons "R5" 5
  (acons "R6" 6
  (acons "R7" 7
  (acons "R8" 8
  (acons "R9" 9
  (acons "R10" 10
  (acons "R11" 11
  (acons "R12" 12
  (acons "R13" 13
  (acons "R14" 14
  (acons "R15" 15
  (acons "SCREEN" 16384
  (acons "KBD" 24576 *RAM-TABLE*)))))))))))))))))))))))))

; reset-env resets the assembler environment back to its
; init-state.
(defun reset-env ()
 (setf *FIRST-FREE-MEMLOC* 16)
 (setf *RAM-TABLE* nil)
 (setf *ROM-TABLE* nil)
 (init-ram-table)
 nil )
 
; Symbol RAM-address Memory-table Boolean -> Symbol-table
; appends a Symbol-table with the given symbol and the corresponding address
; if :error-if-duplicate is T (default value), an error occurs if the given symbol
; already has an association in the Symbol-table - if :error-if-duplicate is explicitly
; set to nil, no error occurs and the symbol-table stays the same.
; Examples:
; 		given: "LOOP" 25 nil 												 expect: '(("LOOP" . 25))
;		given: "LOOP" 34 '(("LOOP" . 25))									 expect: (error "LOOP: symbol already introduced")
;		given: "i" 51 '(("LOOP" . 25)) :error-if-duplicate nil				 expect '(("i" . 51) ("LOOP" . 25))
;		given: "i" 40 '(("i" . 51) ("LOOP" . 25)) :error-if-duplicate nil	 expect '(("i" . 51) ("LOOP" . 25))
(defun append-table (symbol address symbol-table &key (error-if-duplicate T))
  (if (assoc symbol symbol-table :test #'string=)
	  (if error-if-duplicate
	     (error "~A: symbol already introduced" symbol)
		 symbol-table )
	  (acons symbol address symbol-table) ) )

; String -> ROM-table RAM-table
; Finds the address of an a-expression variable
; An a expression variable can refer either to a
; ROM-instruction memory location or a RAM-data
; memory location.
; examples:
;	given "LOOP" '(("START" . 0) ("LOOP" . 15) ("END" . 20)) '(("i" . 10)), expect 15
;	given "i"  '(("START" . 0) ("LOOP" . 15) ("END" . 20)) '(("i" . 10)), expect 10
;	given "LOOP2" '(("START" . 0) ("LOOP" . 15) ("END" . 20)) '(("i" . 10)), expect nil
;	given "$Var"  '(("START" . 0) ("LOOP" . 15) ("END" . 20)) '(("i" . 10)), expect nil
(defun variable-address (val rom-table ram-table)
  (or (cdr (assoc val rom-table :test #'string=))
	  (cdr (assoc val ram-table :test #'string=)) ) )

;;; The assembler functions:

; String -> 'ASSEMBLED
; Assembles the given .asm file into a .hack file
(defun assemble (file)
  (reset-env)
  (if (probe-file file)
      (assemble-file file (translate-labels file *ROM-TABLE*) *RAM-TABLE*)
      (error "File not found") ) )
  
; String ROM-table -> ROM-table 
; Translates all the labels in the given .asm source file into their corresponding
; ROM-memory addresses.
(defun translate-labels (file rom-table)
  (flet ((code-line (exp-type)
		    (or (equal exp-type 'C-EXP)
			    (equal exp-type 'A-EXP) ) )
		 (label-line (exp-type)
			(equal exp-type 'L-EXP) ) )
    (with-open-file (readf file :direction :input)
      (do* ((line (read-line readf nil)
				  (read-line readf nil) )
		    (trimmed-line (trim-line line)
						  (trim-line line) )
			(exp-type (exp-type trimmed-line)
					  (exp-type trimmed-line) )
            (curr-line-num (if (code-line exp-type) 0 -1) (if (code-line exp-type) (1+ curr-line-num) curr-line-num)) )
            ((null line) rom-table)
        (if (label-line exp-type)
		    (let ((label (label->symbol trimmed-line)))
			  (if (variable-address label nil *RAM-TABLE*)
				  (error "[illegal label name ~a]: ~a" label trimmed-line)
				  (setf rom-table (append-table label
												(1+ curr-line-num)
												rom-table )) ) )
			nil ) ) ) ) )
			
; String ROM-table RAM-table
; assembles the given .asm file into an identically named .hack-file
; with the given ROM-Memory table that contains all the label->code-address
; translations and with the given RAM-memory table that contains all the
; variable->data-address translations.
(defun assemble-file (file rom-table ram-table)
  (labels ((assemble-a-val (val)
			(if (numberp val)
				(fill-zeroes (write-to-string val :base 2))
				(let ((addr (variable-address val rom-table ram-table)))
				  (if addr
					  (assemble-a-val addr)
					  (if (<= *FIRST-FREE-MEMLOC* *LAST-FREE-MEMLOC*)
						(progn
						  (setf ram-table (append-table val *FIRST-FREE-MEMLOC* ram-table))
						  (setf *FIRST-FREE-MEMLOC* (1+ *FIRST-FREE-MEMLOC*))
						  (assemble-a-val (1- *FIRST-FREE-MEMLOC*)) )
						(progn
						  (error "Insufficient memory") ) ) ) ) ) )
		   (assemble-c-exp (c-exp)
		     (concatenate 'string "111"
								  (comp->bits (comp c-exp))
								  (dest->bits (dest c-exp))
								  (jump->bits (jump c-exp)) ) )
		   (fill-zeroes (bin#str)
		     (concatenate 'string (make-string (- 16 (length bin#str)) :initial-element #\0)
						  bin#str ) ) )
    (with-open-file (readf file :direction :input)
      (with-open-file (writef (target-file file)
				              :direction :output
							  :if-exists :supersede
							  :if-does-not-exist :create )
        (do* ((line (read-line readf nil)
				    (read-line readf nil) )
			  (trimmed-line (trim-line line)
						    (trim-line line) )
			  (exp-type (exp-type trimmed-line)
					    (exp-type trimmed-line) )
			  (total-line-num 1 (1+ total-line-num))
              (code-line-num  1 (if (or (equal exp-type 'A-EXP)
							            (equal exp-type 'C-EXP) )
							        (1+ code-line-num)
							        code-line-num )) )
              ((null line) 'ASSEMBLED)
          (case exp-type
		    ((NO-EXP L-EXP) nil )
		    (A-EXP
		      (write-line (assemble-a-val (a-value trimmed-line)) writef) )
		    (C-EXP (write-line (assemble-c-exp trimmed-line) writef))
		    (INVALID-EXP (error "[Line ~a] BAD OPCODE: ~a" total-line-num trimmed-line)) ) ) ) ) ) )
