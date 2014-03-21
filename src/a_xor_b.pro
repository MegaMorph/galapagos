; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	a_xor_b
;
; PURPOSE:
;	This function applies the boolean XOR operator to two vectors.
;	It returns a vector with those elements which occur in one or the other vector
;	but not in both.
;
; CATEGORY:
;	MATH
;
; CALLING SEQUENCE:
;	Result = a_xor_b(vec1, vec2)
;
; INPUTS:
;	vec1, vec2: The two vectors to which the XOR operation should be applied.
;
; KEYWORD PARAMETERS:
;	COUNT = count: If set to a named variable the resulting number
;		of elements of the boolean operation is returned.
;
; OUTPUTS:
;	It returns a vector with those elements which occur in one or the other vector
;	but not in both.
;	In the case of an empty output vector, -1 is returned.
;
; RESTRICTIONS:
;	Repeated elements in the input vectors are returned only once in the output vector
;	(see example).
;
; EXAMPLE:
;	A = [1, 2, 2, 2, 3, 4, 13, 6, 7, 8, 9, 14, 20]
;	B = [0, 2, 2, 4, 6, 10, 12, 13]
;	C = a_xor_b(A, B) = [0, 1, 3, 7, 8, 9, 10, 12, 14, 20]
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 02.12.97
;	History:	29.10.98:	debug statement removed
;	21.11.2000:	Added keyword COUNT
;-

FUNCTION a_xor_b, v1, v2, COUNT = count
	and_vek = a_and_b(v1, v2)
	or_vek = a_or_b(v1, v2)
	IF (and_vek[0] NE -1) AND (or_vek[0] NE -1) THEN BEGIN
		vek = a_not_b(or_vek, and_vek)
		vek = vek[UNIQ(vek, SORT(vek))]
		count = N_ELEMENTS(vek)
	ENDIF ELSE BEGIN
		vek = -1L
		count = 0L
	ENDELSE
	RETURN, vek
END