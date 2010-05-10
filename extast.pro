pro extast,hdr,astr,noparams
;+
; NAME:
;     EXTAST
; PURPOSE:
;     Extract astrometry parameters from a FITS image header.
; EXPLANATION:
;     The astrometry in the header can be in either CD (Coordinate
;     description) format, or CROTA and CDELT (AIPS-type) format.
;     However, the output astrometry will always be in CD format.
;
; CALLING SEQUENCE:
;     EXTAST, hdr, [ astr, noparams ]   
;
; INPUT:
;     HDR - variable containing the FITS header (string array)
;
; OUTPUTS:
;     ASTR - Anonymous structure containing astrometry info from the FITS 
;             header ASTR always contains the following tags (even though 
;             some projections do not require all the parameters)
;      .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;               in DEGREES/PIXEL                                   CD2_1 CD2_2
;      .CDELT - 2 element vector giving physical increment at reference pixel
;      .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;               (def = NAXIS/2) in FITS convention (first pixel is 1,1)
;      .CRVAL - 2 element double precision vector giving R.A. and DEC of 
;             reference pixel in DEGREES
;      .CTYPE - 2 element string vector giving projection types, default
;             ['RA---TAN','DEC--TAN']
;      .LONGPOLE - scalar longitude of north pole (default = 180) 
;      .PROJP1 - Scalar parameter needed in some projections
;      .PROJP2 - Scalar parameter needed in some projections
;
;       NOPARAMS -  Scalar indicating the results of EXTAST
;             -1 = Failure - Header missing astrometry parameters
;             0 = Success - Header contains CD00n00m + CDELT* astrometry
;             1 = Success - Header contains CROTA + CDELT (AIPS-type) astrometry
;             2 = Success - Header contains CDn_m astrometry.    
;             3 = Success - Header contains PC00n00m + CDELT astrometry. As of
;                           December 2001, this is the recommended format 
;             4 = Success - Header contains ST  Guide Star Survey astrometry
;                           (see gsssextast.pro )
; PROCEDURE:
;       EXTAST checks for astrometry parameters in the following order:
;
;       (1) the CD matrix PC001001,PC001002...plus CDELT*, CRPIX and CRVAL
;       (2) the CD matrix CD001001,CD001002...plus CDELT*, CRPIX and CRVAL
;       (3) the CD matrix CD1_1,CD1_2... plus CRPIX and CRVAL.   
;       (3) CROTA2 (or CROTA1) and CDELT plus CRPIX and CRVAL.
;
;       See the preprint: Representations of Celestial Coordinates in FITS by
;       Griesen and Calabretta, available at 
;       http://www.aoc.nrao.edu/~egreisen
;
; NOTES:
;       (1) An anonymous structure is created to avoid structure definition
;               conflicts.    This is needed because some projection systems
;               require additional dimensions (i.e. spherical cube
;               projections require a specification of the cube face).
;
; PROCEDURES CALLED:
;       FITS_CD_FIX, GSSSEXTAST, SXPAR(), ZPARCHECK
; REVISION HISTORY
;      Written by B. Boothman 4/15/86
;      Accept CD001001 keywords               1-3-88
;      Accept CD1_1, CD2_1... keywords    W. Landsman    Nov. 92
;      Recognize GSSS FITS header         W. Landsman    June 94
;      Converted to IDL V5.0   W. Landsman   September 1997
;      Get correct sign, when converting CDELT* to CD matrix for right-handed
;      coordinate system                  W. Landsman   November 1998
;      Consistent conversion between CROTA and CD matrix  October 2000
;      CTYPE = 'PIXEL' means no astrometry params  W. Landsman January 2001
;      Don't choke if only 1 CTYPE value given W. Landsman  August 2001
;      Recognize PC00n00m keywords again (sigh...)  W. Landsman December 2001
;      Recognize GSSS in ctype also       D. Finkbeiner Jan 2002
;-
 On_error,2

 if ( N_params() LT 2 ) then begin
     print,'Syntax - extast, hdr, astr, [ noparams ]'
     return
 endif

 radeg = 180.0D0/!DPI

 zparcheck,'EXTAST',hdr,1,7,1,'FITS image header'   ;Make sure valid header
 noparams = -1                                    ;Assume no astrometry to start

 ctype = strtrim( sxpar( hdr, 'CTYPE*', Count = N_ctype), 2)

; If the standard CTYPE* astrometry keywords not found, then check if the
; ST guidestar astrometry is present

 check_gsss = (N_ctype EQ 0)
 if N_ctype GE 1 then check_gsss = (strmid(ctype[0], 5, 3) EQ 'GSS')

 if check_gsss then begin

        gsss = sxpar( hdr,'PPO1', COUNT = N_ppo1)
        if N_ppo1 EQ 1 then begin 
                gsssextast, hdr, astr, gsssparams
                if gsssparams EQ 0 then noparams = 4
                return
        endif
        ctype = ['RA---TAN','DEC--TAN']
  endif

  if (ctype[0] EQ 'PIXEL') then return
  if N_ctype EQ 2 then if (ctype[1] EQ 'PIXEL') then return

  crval = sxpar( hdr, 'CRVAL*', Count = N )
     if N LT 2 then return              ;No CRVAL parameters

  crpix = sxpar( hdr, 'CRPIX*', Count = N )
     if N lt 2 then return                 ;No CRPIX parameters?

 CD11 = sxpar( hdr,'PC001001', COUNT = N_PC001 )
 if N_PC001 EQ 1 then begin 
        CD12 = sxpar( hdr, 'PC001002')
        CD21 = sxpar( hdr, 'PC002001')  
        CD22 = sxpar( hdr, 'PC002002')
        CDELT = sxpar( hdr, 'CDELT*', Count = N )
        if N LT 2 then cdelt = [1.0D, 1.0D] 
        noparams = 3
 endif else begin 

   CD11 = sxpar( hdr, 'CD001001', COUNT = N_CD001 )

   if N_CD001 EQ 1 then begin 
        CD12 = sxpar( hdr, 'CD001002')
        CD21 = sxpar( hdr, 'CD002001')  
        CD22 = sxpar( hdr, 'CD002002')

; Get CDELT values.   If no CDELT keywords found, then assume that they
; are incorporated in the CD00n00m values, and convert to CDn_m

        CDELT = sxpar( hdr, 'CDELT*', Count = N )
        if N lt 2 then begin  
                fits_cd_fix, hdr, /REV
                cdelt = [1.0D, 1.0D ]
                noparams = 2
        endif else noparams = 0

  endif else begin
    CD11 = sxpar( hdr,'CD1_1', Count = N_CD1 )
    if N_CD1 EQ 1 then begin        ;If CD parameters don't exist, try CROTA
        CD12 = sxpar( hdr, 'CD1_2' )
        CD21 = sxpar( hdr, 'CD2_1' )  
        CD22 = sxpar( hdr, 'CD2_2' )
        cdelt = [1.0D, 1.0D ]
        noparams = 2
    endif else begin

; Now get rotation, first try CROTA2, if not found try CROTA1, if that
; not found assume North-up.   Then convert to CD matrix - see Section 5 in
; Griesen and Calabretta

        CDELT = sxpar( hdr, 'CDELT*', Count = N )
        if N lt 2 then return       ;Must have CDELT1 and CDELT2

        CROTA = sxpar( hdr,'CROTA2', Count = N)  
        if N EQ 0 then  crota = sxpar(hdr, 'CROTA1', COUNT = N )
        if N EQ 0 then crota = 0.0
        crota = crota /  RADEG 
        CD11 = cos(crota)
        CD12 = sin(crota)
        CD21 = -sin(crota)
        CD22 = cos(crota)

       noparams = 1           ;Signal AIPS-type astrometry found
     

    endelse

  endelse
  endelse

  cd = [ [ cd11, cd21 ] , [ cd12, cd22] ]    ;Error in array order corrected
 
  if N_elements( cdelt ) LT 2 then begin
        cdelt = sxpar( hdr, 'CDELT*', Count = N)
        if N LT 2 then cdelt = [1.0D,1.0D]
  endif 

  projection = strmid( ctype[0], 5, 3)
  if (projection EQ 'TAN') then begin
        longpole = 180.0
        projp1 = -1.
        projp2 = -2.
   endif else begin
        longpole = sxpar( hdr, 'LONGPOLE', COUNT = n)
        if n EQ 0 then longpole = 180.0
        projp1 =  sxpar( hdr,'PROJP1')
        projp2 = sxpar( hdr,'PROJP2')
  endelse

; Note that the dimensions and datatype of each tag must be explicit, so that
; there is no conflict with structure definitions from different FITS headers

  ASTR = {CD: double(cd), CDELT: double(cdelt), $
                CRPIX: float(crpix), CRVAL:double(crval), $
                CTYPE: string(ctype), LONGPOLE: float( longpole[0]),  $
                PROJP1: float(projp1[0]), PROJP2: float(projp2[0])}

  return
  end
