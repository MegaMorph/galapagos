; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	a_or_b
;
; PURPOSE:
;	This function applies the boolean OR operator to two vectors.
;	It returns a vector with those elements which occur in one or the other vector.
;
; CATEGORY:
;	MATH
;
; CALLING SEQUENCE:
;	Result = a_or_b(vec1, vec2)
;
; INPUTS:
;	vec1, vec2: The two vectors to which the OR operation should be applied.
;
; KEYWORD PARAMETERS:
;	COUNT = count: If set to a named variable the resulting number
;		of elements of the boolean operation is returned.
;
; OUTPUTS:
;	It returns a vector with those elements which occur in one or the other vector.
;	In the case of an empty output vector, -1 is returned.
;
; RESTRICTIONS:
;	Repeated elements in the input vectors are returned only once in the output vector
;	(see example).
;
; EXAMPLE:
;	A = [1, 2, 2, 2, 3, 4, 13, 6, 7, 8, 9, 14, 20]
;	B = [0, 2, 2, 4, 6, 10, 12, 13]
;	C = a_or_b(A, B) = [0, 1, 2, 3, 2, 6, 7, 8, 9, 10, 12, 13, 14, 20]
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 02.12.97
;	History:	29.10.98:	debug statement removed
;	21.11.2000:	Added keyword COUNT
;
;-

FUNCTION a_or_b, v1, v2, COUNT = count
	cv = [v1, v2]
	vek = cv[UNIQ(cv, SORT(cv))]
	count = N_ELEMENTS(vek)
	RETURN, vek
END