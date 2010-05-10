pro adxy, hdr, a, d, x, y, PRINT = print	;Ra, Dec to X,Y
;+
; NAME:
;	ADXY
; PURPOSE:
;	Use a FITS header to convert celestial (RA,Dec) to pixel coordinates
; EXPLANATION:
;	Use an image header to compute X and Y positions, given the
;	RA and Dec in decimal degrees.  
;
; CALLING SEQUENCE:
;	ADXY, HDR		;Prompt for Ra and DEC 
;	ADXY, hdr, a, d, x, y, [ /PRINT ]
;
; INPUTS:
;	HDR - FITS Image header containing astrometry parameters
;
; OPTIONAL INPUTS:
;	A - Right ascension in decimal DEGREES, scalar or vector
;	D - Declination in decimal DEGREES, scalar or vector        
;
;	If A and D are not supplied, user will be prompted to supply
;	them in either decimal degrees or HR,MIN,SEC,DEG,MN,SC format.
;
; OPTIONAL OUTPUT:
;	X     - row position in pixels, same number of elements as A and D
;	Y     - column position in pixels
;
;       X and Y will be in standard IDL convention (first pixel is 0) and not
;       the FITS convention (first pixel is 1).
; OPTIONAL KEYWORD INPUT:
;	/PRINT - If this keyword is set and non-zero, then results are displayed
;		at the terminal.
;
; OPERATIONAL NOTES:
;	If less than 5 parameters are supplied, or if the /PRINT keyword is
;	set, then then the X and Y positions are displayed at the terminal.
;
;	If the procedure is to be used repeatedly with the same header,
; 	then it would be faster to use AD2XY.
;
; PROCEDURES CALLED:
;	AD2XY, ADSTRING(), EXTAST, GETOPT()
;
; REVISION HISTORY:
;	W. Landsman                 HSTX          January, 1988
;	Use astrometry structure   W. Landsman   January, 1994	
;	Changed default ADSTRING format   W. Landsman    September, 1995
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 npar = N_params()

 if ( npar EQ 0 ) then begin
	print,'Syntax - adxy, hdr, [a, d, x, y ]'
        print,'If supplied, A and D must be in decimal DEGREES'
	return
 endif                                                                  
 
 extast, hdr, astr, noparams   ;Extract astrometry from FITS header
 if ( noparams LT 0 ) then return 
 
 if npar lt 3 then begin
   RD: print,'Coordinates must be entered with either 2 or 6 parameters'
   print,'Either RA,DEC or  HR,MIN,SEC,DEG,MIN,SEC
   inp = ''
   read,'ADXY: Enter coordinates: ',inp
   radec = getopt(inp,'F')
   case N_elements(radec) of 
      2: begin 
         a = radec[0] & d = radec[1]
         end
      6: begin
         a = ten(radec[0:2]*15.) & d = ten(radec[3:5])
	 end
   else: begin
         print,string(7b),'ADXY: ERROR - Illegal Format'
	 return
	 end
   endcase 
 endif

 case strmid( astr.ctype[0], 5,3) of
 'GSS': gsssadxy, astr, a, d, x, y       ;HST Guide star astrometry
 else:  ad2xy, a, d, astr, x, y          ;All other cases
 endcase

 if (npar lt 5) or keyword_set( PRINT ) then begin
	npts = N_elements(a)
        fmt = '(2F9.4,A,2X,2F8.2)'
        str = adstring(a,d,1)
	print,'    RA       DEC       RA         DEC          X       Y'
	for i = 0l, npts-1 do $
	print,FORMAT = fmt, a[i], d[i], str[i], x[i], y[i] 
  endif
 
 return
 end
