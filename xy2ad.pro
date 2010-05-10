pro xy2ad, x, y, astr, a, d
;+
; NAME:
;     XY2AD
;
; PURPOSE:
;     Compute R.A. and Dec from X and Y and a FITS astrometry structure
; EXPLANATION:
;     The astrometry structure must first be extracted by EXTAST from a FITS
;     header.   A tangent (gnomonic) projection is computed directly; other
;     projections are computed using WCSXY2SPH.   Angles are returned in 
;     degrees.   XY2AD is meant to be used internal to other procedures.  
;     For interactive purposes use XYAD.
;
; CALLING SEQUENCE:
;     XY2AD, x, y, astr, a, d   
;
; INPUTS:
;     X     - row position in pixels, scalar or vector
;     Y     - column position in pixels, scalar or vector
;           X and Y should be in the standard IDL convention (first pixel is
;           0), and not the FITS convention (first pixel is 1). 
;     ASTR - astrometry structure, output from EXTAST procedure containing:
;        .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;               in DEGREES/PIXEL                                   CD2_1 CD2_2
;        .CDELT - 2 element vector giving physical increment at reference pixel
;        .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;               (def = NAXIS/2)
;        .CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;               in DEGREES
;        .CTYPE - 2 element vector giving projection types 
;        .LONGPOLE - scalar longitude of north pole (default = 180) 
;        .PROJP1 - Scalar parameter needed in some projections
;        .PROJP2 - Scalar parameter needed in some projections
;
; OUTPUT:
;     A - R.A. in DEGREES, same number of elements as X and Y
;     D - Dec. in DEGREES, same number of elements as X and Y
;
; RESTRICTIONS:
;       Note that all angles are in degrees, including CD and CRVAL
;       Also note that the CRPIX keyword assumes an FORTRAN type
;       array beginning at (1,1), while X and Y give the IDL position
;       beginning at (0,0).
;       No parameter checking is performed.
;
; REVISION HISTORY:
;       Written by R. Cornett, SASC Tech., 4/7/86
;       Converted to IDL by B. Boothman, SASC Tech., 4/21/86
;       Perform CD  multiplication in degrees  W. Landsman   Dec 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Understand reversed X,Y (X-Dec, Y-RA) axes,   W. Landsman  October 1998
;       Consistent conversion between CROTA and CD matrix W. Landsman Oct. 2000
;-
 if N_params() LT 4 then begin
        print,'Syntax -- XY2AD, x, y, astr, a, d'
        return
 endif
 radeg = 180.0d/!DPI                  ;Double precision !RADEG

 cd = astr.cd
 crpix = astr.crpix
 cdelt = astr.cdelt
 if cdelt[0] NE 1.0 then begin 
         cd[0,0] = cd[0,0]*cdelt[0] & cd[0,1] = cd[0,1]*cdelt[0]
         cd[1,1] = cd[1,1]*cdelt[1] & cd[1,0] = cd[1,0]*cdelt[1]
  endif


 xdif = x - (crpix[0]-1)            
 ydif = y - (crpix[1]-1)
 xsi = cd[0,0]*xdif + cd[0,1]*ydif   ;Can't use matrix notation, in
 eta = cd[1,0]*xdif + cd[1,1]*ydif   ;case X and Y are vectors

 ctype = astr.ctype
 crval = astr.crval
 coord = strmid(ctype,0,4)
 reverse = ((coord[0] EQ 'DEC-') and (coord[1] EQ 'RA--')) or $
           ((coord[0] EQ 'GLAT') and (coord[1] EQ 'GLON')) or $
           ((coord[0] EQ 'ELON') and (coord[1] EQ 'ELAT'))

 if reverse then begin
     crval = rotate(crval,2)
     temp = xsi & xsi = eta & eta = temp
 endif

 if (strmid(ctype[0],5,3) EQ 'TAN') or (ctype[0] EQ '') then begin  
         xsi = xsi / radeg
         eta = eta / radeg
         crval = crval / radeg

         beta = cos(crval[1]) - eta * sin(crval[1])
         a = atan(xsi, beta) + crval[0]
         gamma = sqrt((xsi^2) +(beta^2))
         d = atan(eta*cos(crval[1])+sin(crval[1]) , gamma)
         a = a*RADEG   &  d = d*RADEG

 endif else wcsxy2sph, xsi, eta, a, d, CTYPE = ctype[0:1], PROJP1 = astr.projp1, $
        PROJP2 = astr.projp2, LONGPOLE = astr.longpole, CRVAL = crval

 return
 end
