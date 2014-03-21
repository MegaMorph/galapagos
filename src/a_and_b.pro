; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	a_and_b
;
; PURPOSE:
;	This function applies the boolean AND operator to two vectors.
;	It returns a vector with those elements which occur in both vectors.
;
; CATEGORY:
;	MATH
;
; CALLING SEQUENCE:
;	Result = a_and_b(vec1, vec2)
;
; INPUTS:
;	vec1, vec2: The two vectors to which the AND operation should be applied.
;
; KEYWORD PARAMETERS:
;	/NO_SORT: If set the returned vector is not sorted. However, in that case
;		multiple instances of single elements in the shorter of the two input
;		vectors will show up in the output vector.
;	COUNT = count: If set to a named variable the resulting number
;		of elements of the boolean operation is returned.
;
; OUTPUTS:
;	This function returns a vector with those elements which occur in both input vectors.
;	In the case of an empty output vector, -1 is returned. The elements are sorted in
;	increasing order.
;
; RESTRICTIONS:
;	Repeated elements in the input vectors are returned only once in the output vector
;	(see example).
;
; EXAMPLE:
;	A = [1, 2, 2, 2, 3, 4, 13, 6, 7, 8, 9, 14, 20]
;	B = [0, 2, 2, 4, 6, 10, 12, 13]
;	C = a_and_b(A, B) = [2, 4, 6, 13]
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 02.12.97
;	History:	29.10.98:	FOR i = 0, ... changed to FOR i = 0L, ...
;								debug statement removed
;	25.02.2000:	New keyword parameter /NO_SORT
;	21.11.2000:	Added keyword COUNT
;-

FUNCTION a_and_b, v1, v2, NO_SORT = no_sort, COUNT = ni
	IF N_ELEMENTS(v1) LE N_ELEMENTS(v2) THEN BEGIN
		short_vek = v1
		long_vek = v2
	ENDIF ELSE BEGIN
		short_vek = v2
		long_vek = v1
	ENDELSE
	IF KEYWORD_SET(no_sort) EQ 0 THEN short_vek = short_vek[UNIQ(short_vek, SORT(short_vek))]
	w_vek = short_vek
	FOR i = 0L, N_ELEMENTS(short_vek) - 1 DO $
		w_vek[i] = (WHERE(long_vek EQ short_vek[i]))[0]
	ci = WHERE(w_vek NE -1, ni)
	IF ni GT 0 THEN vek = short_vek[ci] ELSE vek = -1
	RETURN, vek
END
