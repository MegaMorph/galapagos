pro fits_cd_fix,hdr, REVERSE = reverse
;+
; NAME:
;    FITS_CD_FIX
;
; PURPOSE:
;    Convert between different representations of the CD matrix in a FITS header   
;
; EXPLANATION:
;    According the paper, "Representations of Celestial Coordinates in FITS"
;    by Griesen and Calabretta, available at 
;    http://www.cv.nrao.edu/fits/documents/wcs/wcs.html
;    the rotation of an image from standard coordinates is represented by a 
;    coordinate description (CD) matrix.    However, there have been several
;    different representations proposed for the CD matrix.   Currently, 
;    (April 2000), the preferred form is CDn_m (as used in IRAF), which 
;    contains both rotation & plate scale info.    However,
;    an earlier draft of Griesen & Calabretta proposed the CD00n00m form.
;    containing only rotation (and skew) info, with the plate scale stored in
;    the CDELT* keywords.
;
;    FITS_CD_FIX converts from the representation of the CD matrix with an 
;    underscore (e.g. CDn_m) to that with all integers (e.g. CD00n00m).    Users
;    will more commonly go in the reverse direction (since the CDn_m format
;    is now prefered) using the /REVERSE keyword.  
;
; CALLING SEQUENCE:
;    FITS_CD_FIX, Hdr, [/REVERSE]
;
; INPUT-OUTPUT: 
;       HDR - FITS header, 80 x N string array.   If the header does not
;           contain the CDn_m keywords then it is left unmodified.  Other-
;           wise the CDn_m keywords are removed and the CD00n00m keywords
;           inserted (with the same values).
;   
; OPTIONAL KEYWORD INPUT
;      /REVERSE - If this keyword is set and non-zero, then the process is
;               reversed, i.e. CD00n00m keywords are removed from the header
;               and CDn_m keywords are inserted.
; PROCEDURES USED:
;    SXADDPAR, SXDELPAR, SXPAR
; REVISION HISTORY:
;    Written   W. Landsman             Feb 1990
;    Major rewrite                     Feb 1994
;    Converted to IDL V5.0   W. Landsman   September 1997
;    Use double precision formatting of CD matrix   W. Landsman  April 2000
;-
 On_error,2

 if N_params() LT 1 then begin
        print,'Syntax - FITS_CD_FIX, hdr, [/REVERSE]'
        return
 endif

 cd00 = ['CD001001','CD001002','CD002001','CD002002']
 cd_ = ['CD1_1','CD1_2','CD2_1','CD2_2']
 comment = [' DL/DX',' DL/DY',' DM/DX',' DM/DY']

 if keyword_set( REVERSE ) then begin

 for i= 0 ,3 do begin
 cd = sxpar(hdr,cd00[i], COUNT = N )
 if N GE 1 then begin
        sxaddpar,hdr,cd_[i],cd,comment[i],cd00[i]
        sxdelpar,hdr,cd00[i]
 endif
 endfor

 endif else begin

 for i= 0 ,3 do begin
 cd = sxpar(hdr,cd_[i], COUNT = N )
 if N GE 1 then begin
        sxaddpar,hdr,cd00[i],cd,comment[i],cd_[i]
        sxdelpar,hdr,cd_[i]
 endif
 endfor

 endelse

 return
 end
                                
