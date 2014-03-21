;$Id: //depot/idl/IDL_63_RELEASE/idldir/lib/igamma.pro#1 $
;
; Copyright (c) 1994-2006, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       IGAMMA
;
; PURPOSE:
;       This function computes the incomplete gamma function, Px(a).
;
; CATEGORY:
;       Special Functions.
;
; CALLING SEQUENCE:
;       Result = Igamma(a, x)
;
; INPUTS:
;       A:    A scalar or array of any basic type that
;             specifies the parametric exponent of the integrand.
;             A may be complex.
;
;       X:    A scalar or array of any basic type that
;             specifies the upper limit of integration.
;             X may be complex.
;       Note: For X < 0 you should convert X to complex first,
;             otherwise the Result will be 0.
;
; KEYWORD PARAMETERS:
;
;   DOUBLE = Set this keyword to return a double-precision result.
;
;   EPS = relative accuracy, or tolerance.  The default tolerance
;         is 3.0d-12.
;
;   ITER = Set this keyword equal to a named variable that will contain
;          the actual number of iterations performed.
;
;   ITMAX = Set this keyword to specify the maximum number of iterations.
;           The default value is 100000.
;
;   METHOD:  Use this keyword to specify a named variable which returns
;            the method used to compute the incomplete gamma function.
;            A value of 0 indicates that a power series representation
;            was used. A value of 1 indicates that a continued fractions
;            method was used.
;            This keyword is obsolete and will return 0.
;
; EXAMPLE:
;       Compute the incomplete gamma function for the corresponding elements
;       of A and X.
;       Define the parametric exponents.
;         A = [0.10, 0.50, 1.00, 1.10, 6.00, 26.00]
;       Define the the upper limits of integration.
;         X = [0.0316228, 0.0707107, 5.00000, 1.04881, 2.44949, 25.4951]
;       Compute the incomplete gamma functions.
;         result = Igamma(A, X)
;       The result should be:
;         [0.742026, 0.293128, 0.993262, 0.607646, 0.0387318, 0.486387]
;
; PROCEDURE:
;       IGAMMA computes the incomplete gamma function, Px(a), using either
;       a power series representation or a continued fractions method. If X
;       is less than or equal to A+1, the power series representation is used.
;       If X is greater than A+1, the continued fractions method is used.
;
; REFERENCE:
;       Numerical Recipes, The Art of Scientific Computing (Second Edition)
;       Cambridge University Press
;       ISBN 0-521-43108-5
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, September 1994
;                    IGAMMA is based on the routines: gser.c, gcf.c, and
;                    gammln.c described in section 6.2 of Numerical Recipes,
;                    The Art of Scientific Computing (Second Edition), and is
;                    used by permission.
;       Modified:    GGS, RSI, January 1996
;                    Corrected the case of IGAMMA(a, 0) for a > 0.
;            DMS, Sept, 1999, Added arrays, and more accurate
;           results for double.
;            CT, RSI, March 2000, added DOUBLE, ITER keywords.
;   CT, RSI, Dec 2001: Convert to internal C code, added complex support.
;                      Remove restriction on A>0.
;                      Made METHOD keyword obsolete.
;-

FUNCTION igamma, a, x, _REF_EXTRA=_extra, $
    METHOD = method   ; Obsolete keyword.

    ON_ERROR, 2

    ; Note: IDL_IGAMMA is an undocumented internal routine, and is subject
    ; to change in the future. The function is based on the routines
    ; gser.c and gcf.c described in Numerical Recipes in C (2nd ed, sec 6.2).
    result = IDL_IGAMMA(a, x, _STRICT_EXTRA=_extra)

    ; Check obsolete keyword.
    if ARG_PRESENT(method) then $
        method = REPLICATE(0, N_ELEMENTS(result))

    return, result
end
