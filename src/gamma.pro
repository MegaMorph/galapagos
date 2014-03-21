; $Id: //depot/idl/IDL_63_RELEASE/idldir/lib/gamma.pro#1 $
;
; Copyright (c) 1995-2006, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;       GAMMA
;
; PURPOSE:
;   Return the Gamma function of (possibly complex) Z.
;
; CALLING SEQUENCE:
;   Result = GAMMA(Z)
;
; INPUTS:
;   Z: The expression for which the gamma function will be evaluated.
;      If Z is double-precision, the result is double-precision,
;      otherwise the result is floating-point. Z may be complex.
;
; KEYWORD PARAMETERS:
;   None
;
; MODIFICATION HISTORY:
;   3 July 1995, AB, RSI.
;   AB, 5/4/2001, Switch from using _EXTRA to _STRICT_EXTRA, so that
;       incorrect keywords will cause issue proper messages to
;       be issued instead of being silently ignored.
;   CT, RSI, Jan 2001: Added complex support.
;-
function gamma, z, _REF_EXTRA=_extra

    ON_ERROR, 2

    ; Note: NR_GAMMA is an undocumented internal routine, and is subject
    ; to change in the future. Despite the NR_ prefix, the routine is not
    ; based on Numerical Recipes, but instead uses the specfun package,
    ; ACM Algorithm 715 (Cody 1993).
    return, NR_GAMMA(z, _STRICT_EXTRA=_extra)

end
