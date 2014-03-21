; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	a_not_b
;
; PURPOSE:
;	This function applies the negation of the boolean AND operator to two vectors.
;	It returns a vector with those elements which occur in the first but not in the second vector.
;
; CATEGORY:
;	MATH
;
; CALLING SEQUENCE:
;	Result = a_not_b(vec1, vec2)
;
; INPUTS:
;	vec1, vec2: The two vectors to which the negation of the AND operation should be applied.
;
; KEYWORD PARAMETERS:
;	COUNT = count: If set to a named variable the resulting number
;		of elements of the boolean operation is returned.
;
; OUTPUTS:
;	It returns a vector with those elements which occur in the first but not in the second vector.
;	In the case of an empty output vector, -1 is returned.
;
; RESTRICTIONS:
;	Repeated elements in the input vectors are returned only once in the output vector
;	(see example).
;
; EXAMPLE:
;	A = [1, 2, 2, 2, 3, 4, 13, 6, 7, 8, 9, 14, 20]
;	B = [0, 2, 2, 4, 6, 10, 12, 13]
;	C = a_not_b(A, B) = [1, 3, 7, 8, 9, 14, 20]
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 02.12.97
;	History:	29.10.98:	FOR i = 0, ... changed to FOR i = 0L, ...
;								debug statement removed
;	21.11.2000:	Added keyword COUNT
;-

FUNCTION a_not_b, v1, v2, COUNT = count
	w_vek = v1
	FOR i = 0L, N_ELEMENTS(v1) - 1 DO $
		w_vek[i] = (WHERE(v2 EQ v1[i]))[0]
	ci = WHERE(w_vek EQ -1, ni)
	IF ni GT 0 THEN BEGIN
		vek = v1[ci]
		vek = vek[UNIQ(vek, SORT(vek))]
		count = N_ELEMENTS(vek)
	ENDIF ELSE BEGIN
		vek = -1L
		count = 0L
	ENDELSE
	RETURN, vek
END