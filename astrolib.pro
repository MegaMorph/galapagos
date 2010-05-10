PRO ASTROLIB
;+
; NAME:
;       ASTROLIB
; PURPOSE:
;       Add the non-standard system variables used by the IDL Astronomy Library
; EXPLANATION: 
;       Also defines the environment variable ASTRO_DATA pointing to the 
;       directory containing data files  associated with the IDL Astronomy 
;       library (system dependent -- user must edit the first line in the
;       program below).
;
; CALLING SEQUENCE:
;       ASTROLIB
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; METHOD:
;       The non-standard system variables !PRIV, !DEBUG, !TEXTUNIT, and 
;       !TEXTOUT are added using DEFSYSV.
;
; REVISION HISTORY:
;       Written, Wayne Landsman, July 1986.
;       Use DEFSYSV instead of ADDSYSVAR           December 1990
;       Test for system variable existence before definition    July 2001
;       Assume since V55, remove VMS support  W. Landsman   Sep 2006
;-
  On_error,2   
  setenv,'ASTRO_DATA=/export/home/ftp/pub/data/'

  defsysv, '!DEBUG', exist = exist
     if not exist then defsysv, '!DEBUG', 0
  defsysv, '!PRIV', exist = exist   
     if not exist then defsysv, '!PRIV', 0    
  defsysv, '!TEXTUNIT', exist = exist
     if not exist then  defsysv, '!TEXTUNIT', 0
  defsysv, '!TEXTOUT', exist = exist 
     if not exist then defsysv, '!TEXTOUT', 1 

; The following code needs to modified for each particular installation

 
  message,'Astronomy Library system variables have been added',/INF

  return
  end
 
