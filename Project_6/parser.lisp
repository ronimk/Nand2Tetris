;;;; This is a parser for the Hack-machine assembler
;;;; Its public interface consists of the following
;;;; symbols and functions:
;;;;
;;;; symbols used to identify the type of the current expression:
;;;; C-EXP, A-EXP, L-EXP, NO-EXP and INVALID-EXP where NO-EXP is a comment line or an empty line.
;;;;
;;;; public functions:
;;;; target-file    - creates a proper .hack link file name from the given .asm file name
;;;; label->symbol  - extracts the symbol of an L-EXP
;;;; trim-line      - trims the current line of a .asm file
;;;; a-value        - extracts the value out of an A-EXP
;;;; exp-type       - returns the type of the expression (C-EXP, A-EXP, L-EXP, NO-EXP or INVALID-EXP)
;;;; dest           - extracts the dest part of a C-EXP
;;;; comp           - extracts the comp part of a C-EXP
;;;; jump           - extracts the jump part of a C-EXP
;;;; dest->bits     - transforms the dest part to its associated hack-machine language bits
;;;; comp->bits     - transforms the comp part to its associated hack-machine language bits
;;;; jump->bits     - transforms the jump part to its associated hack-machine language bits

;; The package definition:
(defpackage "PARSER"
  (:export "TARGET-FILE" "LABEL->SYMBOL" "TRIM-LINE"
           "A-VALUE" "EXP-TYPE" "DEST" "COMP" "JUMP"
           "DEST->BITS" "COMP->BITS" "JUMP->BITS"
           "C-EXP" "A-EXP" "L-EXP" "NO-EXP" "INVALID-EXP" ) )
(in-package "PARSER")

;; The dest, comp and jump translation tables for C-EXPressions:
(defparameter *DEST-TABLE*
  '((NIL . "000")
    ("M" . "001")
    ("D" . "010")
    ("MD" . "011")
    ("A" . "100")
    ("AM" . "101")
    ("AD" . "110")
    ("AMD" . "111") ) )
    
(defparameter *COMP-TABLE*
  '(("0" . "0101010")
    ("1" . "0111111")
    ("-1" . "0111010")
    ("D" . "0001100")
    ("A" . "0110000")
    ("M" . "1110000")
    ("!D" . "0001101")
    ("!A" . "0110001")
    ("!M" . "1110001")
    ("-D" . "0001111")
    ("-A" . "0110011")
    ("-M" . "1110011")
    ("D+1" . "0011111")
    ("A+1" . "0110111")
    ("M+1" . "1110111")
    ("D-1" . "0001110")
    ("A-1" . "0110010")
    ("M-1" . "1110010")
    ("D+A" . "0000010")
    ("D+M" . "1000010")
    ("D-A" . "0010011")
    ("D-M" . "1010011")
    ("A-D" . "0000111")
    ("M-D" . "1000111")
    ("D&A" . "0000000")
    ("D&M" . "1000000")
    ("D|A" . "0010101")
    ("D|M" . "1010101") ) )
    
(defparameter *JUMP-TABLE*
  '((NIL . "000")
    ("JGT" . "001")
    ("JEQ" . "010")
    ("JGE" . "011")
    ("JLT" . "100")
    ("JNE" . "101")
    ("JLE" . "110")
    ("JMP" . "111") ) )

; String -> String
; changes the ending of a file named source-file to .hack.
; Examples:
;       given "test.asm", expect "test.hack"
;       given "test.", expect "test.hack"
;       given "test", expect "test.hack"
(defun target-file (source-file)
  (concatenate 'string (strip-file-end source-file) ".hack") )

; String -> String
; strips a file name of its ending.
; Examples:
;       given "test.asm", expect "test"
;       given "test.", expect "test"
;       given "test", expect "test"
(defun strip-file-end (fname)
  (subseq fname 0 (position #\. fname)) )
  
; String -> String
; Trims a line of code of any leading and trailing
; white spaces, including comments starting with "//".
; Examples:
;       given "        ",          expect ""
;       given "    // A comment ", expect ""
;       given "  0;JMP  // ...",   expect "0;JMP"
(defun trim-line (line)
  (string-trim '(#\Space #\Tab) (remove-comments line)) )
  
(defun remove-comments (line)
  (let ((pos (search "//" line)))
    (if pos
        (subseq line 0 pos)
        line ) ) )
        
; String -> Boolean
; Checks whether the given string has digits only.
(defun number-string-p (val)
  (reduce #'(lambda (acc c)
              (and acc (digit-char-p c)) )
          val
          :initial-value T ) )

; String -> Boolean
; Checks whether the given string is a proper symbol.
; A proper symbol is a sequence of alphanumeric characters
; plus a few special characters (_, ., $ and :) only,
; and must not begin with a digit.
; Examples:
;       given "LooP1", expect T
;       given "1:LOOP", expect T
;       given "LOOP_LABEL", expect T
;       given "Var$1", expect T
;       given "Var#1", expect nil
;       given "", expect error
(defun proper-symbol-p (symbol)
  (and
    ; Does the symbol start with an alphabetic character?
    (not (digit-char-p (char symbol 0)))
    ; are the rest of the symbol's characters alphanumeric only:
    (reduce #'(lambda (acc c)
                (and acc
                     (or (alphanumericp c)
                         (member c '(#\_ #\. #\$ #\:) :test #'char=) ) ) )
            symbol  ; Micro-optimization note: faster to check for every char than subseq'ing...
            :initial-value T ) ) )
        
; L-EXP -> Symbol
; extracts the label symbol from an L-EXP.
; label->symbol should never be called if
; it has not been validated as an L-EXP by
; either l-exp-p or exp-type.
; Examples:
;       given "(LOOP)", expect "LOOP"
;       given "(x1)",   expect "x1"
;       given "(1X)",   expect "1X" (not a proper symbol!)
;       given "(LOOP-LABEL)", expect "LOOP-LABEL" (not a proper symbol!)
(defun label->symbol (L-EXP)
  (subseq L-EXP 1 (1- (length L-EXP))) )

; A-EXP -> [Integer|Variable]
; extracts the a-value of an A-EXP,
; which is either an Integer or a Variable.0
; Raises an error if the integer value of an A-EXP is out of illegal.
; Examples:
;       given "@20",   expect 20
;       given "@LOOP", expect "LOOP"
;       given "@0R
(defun a-value (a-exp)
  (let* ((val (subseq a-exp 1))
         (parsed-number (parse-integer val :junk-allowed T)) )
    (if parsed-number
        (if (and (>= parsed-number 0)
                 (<= parsed-number 32767) )
            parsed-number
            (error "A-VALUE out of bounds: ~a" a-exp) )
        (if (proper-symbol-p val)
            val
            (error "Illegal variable: ~a" a-exp) ) ) ) )
  
; A TRIMMED-LINE is a line in the .asm file with
; all spaces and comments removed:
;       ""
;       "(LOOP)"
;       "M=D-1"
;       "@100"
;       "(LOOP" <- not a valid hack-asm expression.

; TRIMMED-LINE -> Boolean
; Checks whether the given trimmed line equals "". 
(defun empty-line-p (trimmed-line)
  (if (string= trimmed-line "")
      T
      NIL ) )
        
; TRIMMED-LINE -> Boolean
; checks whether the given trimmed line is
; a valid L-EXP. Returns T if it is, nil otherwise.
; Examples:
;       given "(LOOP)", expect T
;       given "(T2K3)", expect T
;       given "(12345)", expect nil
;       given ")NOT-AN-L-EXP), expect nil
;       given "M=D-1", expect nil
(defun l-exp-p (trimmed-line)
  (let ((len (length trimmed-line)))
    (and (> len 2)
         (char= (char trimmed-line 0) #\()
         (proper-symbol-p (subseq trimmed-line 1 (1- len)))
         (char= (char trimmed-line (1- len)) #\)) ) ) )

; TRIMMED-LINE -> Boolean
; Checks whether the given trimmed line is
; a valid A-EXP. A valid A-EXP starts
; with a @ and follows either with a number
; or with a proper symbo
; Examples:
;       given "@100", expect T
;       given "M=D-1", expect nil
;       given "@100M=D-1", expect nil
(defun a-exp-p (trimmed-line)
  (and (> (length trimmed-line) 1)
       (char= (char trimmed-line 0) #\@)
       (let ((val (subseq trimmed-line 1)))
         (or (number-string-p val)
             (proper-symbol-p val) ) ) ) )

; TRIMMED-LINE -> Boolean
; Checks whether the given trimmed line is
; a valid C-expression. A valid C-expression is of the form
;       "dest=comp;jump"
; Examples:
;       given "M=D-1",  expect T
;       given "@100",   expect nil
;       given "(LOOP)", expect nil
;       given "\\ Comment here", expect nil
;       given "MK=D+1;JMP", expect nil
(defun c-exp-p (trimmed-line)
  (let ((dest (dest trimmed-line))
        (comp (comp trimmed-line))
        (jump (jump trimmed-line)) )
    (if (and (not dest) (not jump))
        nil
        (and (validate-dest dest)
             (validate-comp comp)
             (validate-jump jump) ) ) ) )

; TRIMMED-LINE -> Boolean
; Gives the type of the expression.
;
; The type of an expression can be either an A-EXP,
; an L-EXP or a C-EXP - assuming it is a valid code.
; Examples:
;       given "@100",   expect A-EXP
;       given "(LOOP)", expect L-EXP
;       given "0;JMP",  expect C-EXP
;       given "@M=D+1;JMP",      expect 'INVALID-EXP
;       given "// Comment line", expect 'INVALID-EXP
(defun exp-type (trimmed-line)
  (cond ((empty-line-p trimmed-line) 'NO-EXP)
        ((a-exp-p trimmed-line) 'A-EXP)
        ((l-exp-p trimmed-line) 'L-EXP)
        ((c-exp-p trimmed-line) 'C-EXP)
        (T 'INVALID-EXP) ) )

; The following three triplets of functions extract all the necessary information
; out of a C-EXP:
;   1st triplet:
;       dest          - extracts the dest part of a C-EXP
;       validate-dest - validates the dest part of a C-EXp once it has been extracted
;       dest->bits    - transforms the dest part to its associated hack-machine language bits
;   2nd triplet:
;       comp          - extracts the comp part of a C-EXP
;       validate-comp - validates the comp part of a C-EXp once it has been extracted
;       comp->bits    - transforms the comp part to its associated hack-machine language bits
;   3rd triplet:
;       jump          - extracts the jump part of a C-EXP
;       validate-jump - validates the jump part of a C-EXp once it has been extracted
;       jump->bits    - transforms the jump part to its associated hack-machine language bits
;
; if the C-ECP is valid (after validating all of its parts) the hack machine translation is achieved by
; simply concatenating the bit-translations of each part:
; "111"++COMP-PART++DEST-PART++JUMP-PART
(defun dest (c-exp)
 (let ((dest-end (position #\= c-exp)))
   (if dest-end
        (subseq c-exp 0 dest-end)
        nil ) ) )
        
(defun validate-dest (dest)
 (if (null dest)
      T
      (assoc dest *DEST-TABLE* :test #'string=) )  )
              
(defun dest->bits (dest)
  (cdr (assoc dest *DEST-TABLE* :test #'string=)) )
  
(defun comp (c-exp)
 (let* ((dest-end (position #\= c-exp))
        (comp-begin (if dest-end (1+ dest-end) 0))
        (jump-begin (position #\; c-exp))
        (comp-end (if jump-begin jump-begin (length c-exp))) )
    (subseq c-exp comp-begin comp-end) ) )
    
(defun validate-comp (comp)
  (if (null comp)
      T
      (assoc comp *COMP-TABLE* :test #'string=) )  )
  
(defun comp->bits (comp)
  (cdr (assoc comp *COMP-TABLE* :test #'string=)) )

(defun jump (c-exp)
  (let ((jump-start (position #\; c-exp)))
    (if jump-start
        (subseq c-exp (1+ jump-start) (length c-exp))
        nil ) ) )
        
(defun validate-jump (jump)
  (if (null jump)
      T
      (assoc jump *JUMP-TABLE* :test #'string=) )  )
    
(defun jump->bits (jump)
  (cdr (assoc jump *JUMP-TABLE* :test #'string=)) )
