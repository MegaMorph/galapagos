;+
; NAME:
;     WCSSPH2XY 
; PURPOSE:
;     Convert spherical coordinates to x and y (map) angular coordinates
; EXPLANATION:
;     Convert spherical (longitude and latitude -- sky) coordinates to x 
;     and y (map) angular coordinates.  This procedure is the inverse of 
;     WCSXY2SPH.    See WCS_DEMO for example of use.
;
; CATEGORY:
;     Mapping and Auxiliary FITS Routine
;
; CALLING SEQUENCE:
;      wcssph2xy, longitude, latitude, x, y, [ map_type , CTYPE = ,
;               FACE =,PROJP1 = , PROJP2= , CRVAL = , CRXY = , LONGPOLE = ,
;               NORTH_OFFSET =, SOUTH_OFFSET =, BADINDEX =]
;
; INPUT PARAMETERS:
;     longitude - longitude of data, scalar or vector, in degrees 
;     latitude - latitude of data, same number of elements as longitude, 
;               in degrees
;     map_type - optional positional parameter, numeric scalar (0-25) 
;               corresponding to a particular map projection.  This is not a 
;               FITS standard, it is simply put in to allow function similar 
;               to that of less general map projection procedures (eg AITOFF).
;               The following list gives the map projection types and their 
;               respective numbers.
;
;  FITS  Number  Name                       Comments
;  code   code
;  ----  ------  -----------------------    -----------------------------------
;   DEF     0    Default = Cartesian
;   AZP     1    Zenithal perspective       projp1 required
;   TAN     2    Gnomic                     AZP w/ projp1 = 0
;   SIN     3    Orthographic               AZP w/ projp1 = Infinity (>10^14)
;   STG     4    Stereographic              AZP w/ projp1 = 1
;   ARC     5    Zenithal Equidistant
;   ZPN     6    Zenithal polynomial        prop1-projp9 required, useless
;   ZEA     7    Zenithal equal area
;   AIR     8    Airy                       projp1 required
;   CYP     9    Cylindrical perspective    projp1 and projp2 required
;   CAR    10    Cartesian
;   MER    11    Mercator
;   CEA    12    Cylindrical equal area     projp1 required
;   COP    13    Conical perspective        projp1 and projp2 required
;   COD    14    Conical equidistant        projp1 and projp2 required
;   COE    15    Conical equal area         projp1 and projp2 required
;   COO    16    Conical orthomorphic       projp1 and projp2 required
;   BON    17    Bonne's equal area         projp1 required
;   PCO    18    Polyconic
;   GLS    19    Sinusoidal
;   PAR    20    Parabolic
;   AIT    21    Hammer-Aitoff
;   MOL    22    Mollweide
;   CSC    23    Cobe Quadrilateralized     convergence of inverse is poor
;                Spherical Cube
;   QSC    24    Quadrilateralized 
;                Spherical Cube
;   TSC    25    Tangential Spherical Cube
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;
;     CTYPE - One, two, or three element vector containing 8 character 
;              strings corresponding to the CTYPE1, CTYPE2, and CTYPE3 
;              FITS keywords: 
;
;               CTYPE(0) - first four characters specify standard system
;               ('RA--','GLON' or 'ELON' for right ascension, Galactic 
;               longitude or ecliptic longitude respectively), second four 
;               letters specify the type of map projection (eg '-AIT' for 
;               Aitoff projection)
;               CTYPE(1) - first four characters specify standard system
;               ('DEC-','GLAT' or 'ELAT' for declination, galactic latitude
;               or ecliptic latitude respectively; these must match 
;               the appropriate system of ctype1), second four letters of 
;               ctype2 must match second four letters of ctype1.
;               CTYPE(2) - if present must be the 8 character string,'CUBEFACE',
;                only used for spherical cube projections to identify an axis 
;               as containing the face on which each x and y pair of 
;               coordinates lie.
;       PROJP1 - scalar with first projection parameter, this may
;               or may not be necessary depending on the map projection used
;       PROJP2 - scalar with second projection parameter, this may
;               or may not be necessary depending on the map projection used
;       CRVAL - 2 element vector containing standard system coordinates (the 
;               longitude and latitude) of the reference point
;       CRXY - 2 element vector giving the x and y coordinates of the 
;               reference point, if this is not set the offset is [0,0]
;               This is not a FITS standard -- it is similar to CRPIX but in 
;               angular X,Y coordinates (degrees) rather than pixel coordinates
;       LONGPOLE - native longitude of standard system's North Pole, default
;               is 180 degrees
;       NORTH_OFFSET - offset (radians) added to input points near north pole.
;       SOUTH_OFFSET - offset (radians) added to input points near south pole.
;       BADINDEX     - vector, list of transformed points too close to poles.
;
;
; OUTPUT PARAMETERS:
;
;       x - x coordinate of data, same number of elements as longitude, in 
;               degrees; if CRXY is set, then x will be returned offset by 
;               crxy(0).  NOTE: x in all map projections increases to the 
;               left, not the right.
;       y - y coordinate of data, same number of elements as longitude, in 
;               degrees; if CRXY is set, y will be returned offset by crxy(1)
;       bad - vector returning index to transformed points close to pole.
;
; OPTIONAL OUTPUT KEYWORD PARAMETERS:
;       FACE - a output variable used for spherical cube projections to 
;               designate the face of the cube on which the x and y 
;               coordinates lie.   Will contain the same number of elements as
;               X and Y.    Must contain at least 1 arbitrary element on input
;               If FACE is NOT defined on input, it is assumed that the
;               spherical cube projection is laid out over the whole sky
;               in the "sideways T" configuration.
; NOTES:
;       The conventions followed here are described in more detail in 
;       "Representations of Celestial Coordinates in FITS" by Mark Calabretta
;       and Eric Greisen (2002, A&A, 395, 1077; also  see  
;       http://www.aoc.nrao.edu/~egreisen).  The general 
;       scheme outlined in that article is to first use WCS_ROTATE to convert 
;       coordinates in one of three standard systems (celestial, galactic, 
;       or ecliptic) into a "native system" of latitude and longitude.  The 
;       latitude and longitude are then converted into x and y coordinates 
;       which depend on the map projection which is performed.   The rotation 
;       from standard to native coordinates can be skipped if one so desires.
;       This procedure necessitates two basic sections.  The first converts 
;       "standard" coordinates to "native" coordinates while the second converts
;       "native" coordinates to x and y coordinates.  The first section is 
;       simply a call to WCS_ROTATE, while the second contains the guts of 
;       the code in which all of the map projection is done.  This procedure 
;       can be called in a form similar to AITOFF, EQPOLE, or QDCB by calling 
;       wcssph2xy with a fifth parameter specifying the map projection by 
;       number and by not using any of the keywords related to the map 
;       projection type (e.g. CTYPE).
;
; PROCEDURE:
;
;       The first task of the procedure is to do general error-checking to 
;       make sure the procedure was called correctly and none of the 
;       parameters or keywords conflict.  This is particularly important 
;       because the procedure can be called in two ways (either using 
;       FITS-type keywords or using a number corresponding to a map projection
;       type).  All variables are converted into double precision values and 
;       angular measurements are converted from degrees into radians.
;       If necessary, longitude values are converted into the range -pi to pi.
;       Any latitude points close to the  of the poles are mapped to a specific
;       latitude of  from the pole so that the map transformations become
;       completely invertible.  The magnitude of this correction is given by 
;       the keywords NORTH_OFFSET and SOUTH_OFFSET and a list of affected 
;       points is optionally returned in the "badindex" output parameter.
;       The next task of the procedure is to convert the "standard" 
;       coordinates to "native" coordinates by rotating the coordinate system.
;       This rotation is performed by the procedure WCS_ROTATE and is governed
;       by the keywords CRVAL and LONGPOLE.   The final task of the WCSSPH2XY 
;       is to take "native" latitude and longitude coordinates and convert 
;       them into x and y coordinates.  Any map specific error-checking is 
;       done at this time.  All of the equations were obtained from 
;       "Representations of Celestial Coordinates in FITS" and cases needing 
;       special attention are handled appropriately (see the comments with 
;       individual map projections for more information on special cases). 
;
;       Note that a further transformation (using the CD matrix) is required
;       to convert the (x,y) coordinates to pixel coordinates. 
; COMMON BLOCKS:
;
;       none
;
; PROCEDURES CALLED:
;       WCS_ROTATE
;
; COPYRIGHT NOTICE:
;
;       Copyright 1993, The Regents of the University of California. This
;       software was produced under U.S. Government contract (W-7405-ENG-36)
;       by Los Alamos National Laboratory, which is operated by the
;       University of California for the U.S. Department of Energy.
;       The U.S. Government is licensed to use, reproduce, and distribute
;       this software. Neither the Government nor the University makes
;       any warranty, express or implied, or assumes any liability or
;       responsibility for the use of this software.
;
; AUTHOR:
;
;       Rick Balsano
;
; MODIFICATIONS/REVISION LEVEL:
;
;       1.1     8/31/93
;       2.3     9/15/93  W. Landsman (HSTX) Update quad cube coords, vectorize
;                        keywords
;       2.4     12/29/93 I. Freedman (HSTX) Eliminated LU decomposition
;       2.5     1/5/93   I. Freedman (HSTX) Offset keywords / bad point index
;       2.6     Dec 94   Compute pole for transformations where the reference
;                       pixel is at the native origin    W. Landsman (HSTX)
;       2.7     May 95  Change internal variable BETA for V4.0 compatibility
;       2.8     June 95 Change loop indices from integer to long
;       2.9     3/18/96 Change FACE usage for cube projections to match WCSLIB
;                       C/FORTRAN software library.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       2.10    02/18/99 Fixed implementation of ARC algorithm
;-

PRO wcssph2xy,longitude,latitude,x,y,map_type, ctype=ctype,$
              face=face,projp1=projp1,projp2=projp2,$
              crval=crval,crxy=crxy,longpole=longpole, $
              north_offset=north_offset, south_offset=south_offset, $
              badindex=badindex

; DEFINE ANGLE CONSTANTS 
 pi = !DPI
 pi2 = pi/2.d0
 radeg = 57.295779513082323d0
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','GLS',$
            'PAR','AIT','MOL','CSC','QSC','TSC']
 origin = [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,  0, 0, 0, $
            1, 1, 1, 1, 1, 1,1, 1 , 1]
; check to see that enough parameters (at least 4) were sent
 if (N_params() lt 4) then begin
    print,'Syntax - WCSSPH2XY, longitude, latitude, x, y, [ map_type, 
    print,'             CTYPE= ,FACE=, PROJP1=, PROJP2=, CRVAL=, CRXY=, 
    print,'             LONGPOLE= ,NORTH_OFFSET=, SOUTH_OFFSET=, BADINDEX=]'
    return
 endif 
   
; GENERAL ERROR CHECKING
; find the number of elements in each of the data arrays

 n_long = n_elements( longitude )
 n_lat = n_elements( latitude )

; convert all scalar data values into arrays with one element each
if (n_long eq 1) then longitude = dblarr(1) + longitude
if (n_lat eq 1) then latitude = dblarr(1) + latitude

; check to see that the data arrays have the same size
 if (n_long ne n_lat) then begin
     message,$
       'LONGITUDE and LATITUDE must have the same number of elements.'
 endif

 if (N_params() eq 5) then begin

  if (keyword_set(ctype1) or keyword_set(ctype2)) then message,$
'Use either the MAP_TYPE positional parameter or set the projection type with CRVAL1 and CRVAL2, but not both.'
; set projection_type string using map_type parameter (a number) 
  if ((map_type ge 0) and (map_type le 25)) then begin
      projection_type=map_types[map_type]
  endif else message,'MAP_TYPE must be >= 0 and <= 25, it was set to '+map_type

endif else if (n_params() eq 4) then begin

; check to see that CTYPE is set correctly 

  if N_elements( ctype ) GE 1 then begin
        ctype1 = ctype[0]        
        projection_type = strupcase(strmid(ctype1,5,3))
  endif

  if N_elements( ctype ) GE 2  then begin
        ctype2 = ctype[1]
        if ((strlen(ctype1) ne 8) or (strlen(ctype2) ne 8)) then $
              message,'CTYPE1 and CTYPE2 (if set) must be 8 character strings.'
        if (projection_type ne strupcase(strmid(ctype2,5,3))) then begin
      message,'The same map projection type must be in the last',/continue
      print,'four characters of both CTYPE1 and CTYPE2.'
      return
    endif
    if (((strupcase(strmid(ctype1,1,2)) eq 'RA') and $
         (strupcase(strmid(ctype2,1,3)) ne 'DEC')) or $
        ((strupcase(strmid(ctype1,1,4)) eq 'GLON') and $
         (strupcase(strmid(ctype2,1,4)) ne 'GLAT')) or $
        ((strupcase(strmid(ctype1,1,4)) eq 'ELON') and $
         (strupcase(strmid(ctype2,1,4)) ne 'ELAT'))) $
    then begin message,'The same standard system must be in the first 4',$
                       /continue
      print,'characters of both CTYPE1 and CTYPE2.'
      return
    endif
  endif else projection_type = 'DEF'
endif 

; this sets the default map projection type for the cases when map_type or
; projection_type is set to 'DEF' or if projection_type is not set at this
; point.  As suggested in 'Representations of Celestial Coordinates in FITS'
; the default type is set to CAR (Cartesian) the simplest of all projections.
 if ((n_elements(projection_type) eq 0) or $
     (projection_type eq 'DEF') ) then begin 
           projection_type='CAR'
        message,'Projection type not supplied, set to default (Cartesian)',/INF
 endif

; Check to make sure all the correct parameters and keywords are set for 
; spherical projections.
if (keyword_set(ctype3) or keyword_set(face) or (projection_type eq 'CSC') or $
    (projection_type eq 'QSC') or (projection_type eq 'TSC')) then begin

  if (n_elements(face) eq 0) then noface=1 else noface=0

endif

; check to see if the x and y offsets are set properly.  If not, break out
; of program.  If the x and y offsets are not set then assume they are zero.
if (((n_elements(crx) eq 1) and (n_elements(cry) eq 0)) or $
     ((n_elements(crx) eq 0) and (n_elements(cry) eq 1))) then $
    message,'If either CRX or CRY is set, the other must also be set'

if (((n_elements(crval1) eq 1) and (n_elements(crval2) eq 0)) or $
    ((n_elements(crval1) eq 0) and (n_elements(crval1) eq 1))) then $
    message,'If either CRVAL1 or CRVAL2 is set, the other must also be set'


; Convert all longitude values into the range -180 to 180 so that equations
; work properly.
  lng = double( longitude )   & lat = double( latitude )
  sz_long = size(lng)
  temp = where(lng ge 180.0, Ntemp)
  if Ntemp GT 0 then lng[temp] = lng[temp] - 360.0d0

; Make small offsets at poles to allow the transformations to be 
; completely invertible.  These generally introduce a small fractional error
; but only at the poles.  They are necessary since all maps 
; lose information at the poles when a rotation is applied, because all points 
; within NORTH_ or SOUTH_OFFSET of the poles are mapped to the same points.

  IF N_elements(north_offset) EQ 0 then north_offset = 1.d-7
  IF N_elements(south_offset) EQ 0 then south_offset = 1.d-7

  bad = where(abs(lat - 90.0) lt north_offset*radeg)
  IF (bad[0] ne -1) THEN BEGIN
      ;; commented out this message
  ;;MESSAGE,/INFORM,'Some input points are too close to the NORTH pole.'
   lat[bad] = 90.0 - north_offset*RADEG
   IF KEYWORD_SET(badindex) THEN badindex = bad
  ENDIF
  bad = where(abs(lat + 90.0) lt south_offset*radeg)
  IF (bad[0] ne -1) THEN BEGIN
      ;; commented out this message
   ;;MESSAGE,/INFORM,'Some input points are too close to the SOUTH pole.'
   lat[bad] = south_offset*radeg - 90.0
   IF KEYWORD_SET(badindex) THEN BEGIN
    badindex = [badindex, bad]
    badindex = badindex[sort(badindex)]
   ENDIF
  ENDIF

; Convert from standard coordinate system to "native" coordinate system
; if the CRVAL keyword is set.  Otherwise, assume the latitude and longitude 
; given are in "native" coordinates already (this is  essentially what is done
; in the procedure AITOFF).

   if N_elements(crval) GE 2 then begin
           if N_elements(map_type) EQ 0 then $
           map_type = where(projection_type EQ map_types)
           orgn = origin[map_type[0]] 

; Rotate from standard celestial coordinates into the native system.

        wcs_rotate, lng, lat, phi, theta, crval, $
                longpole=longpole, origin = orgn
        phi = phi/radeg
        theta = theta/radeg
 endif else begin
        phi = lng/radeg
        theta = lat/radeg
 endelse

; BRANCH BY MAP PROJECTION TYPE
case strupcase(projection_type) of
  'AZP':begin
    if (n_elements(projp1) eq 0) then message,$
      'AZP map projection requires the keyword PROJP1 to be set'
    if (projp1 lt 0) then message,$
      'AZP map projection requires the keyword PROJP1 >= 0'
    r_theta = radeg*cos(theta)*(projp1 + 1.d0)/(projp1 + sin(theta))
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end  

  'TAN':begin
    r_theta = radeg/tan(theta)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'SIN':begin
    r_theta = radeg*cos(theta)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'STG':begin
    r_theta = 2.d0*radeg*tan((pi2-theta)/2.d0)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'ARC':begin
    r_theta = radeg*( pi2 - theta )
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'ZPN':begin
    message,'ZPN is not implemented due to its general uselessness.'
  end

  'ZEA':begin
    r_theta = 2.d0*radeg*sin((pi2 - theta)/2.d0)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'AIR':begin
    if not(keyword_set(projp1)) then begin
      message,/informational,$
          'PROJP1 not set, using default of PROJP1 = 90 for AIR map projection'
      projp1 = 9.d1
    endif
    theta_b = projp1/radeg

    xi = (pi2 - theta)/2.d0

; When theta_b (aka projp1 in radians) is equal to pi/2 the normal equations
; for the AIR projection produce infinities.  To avoid the problem, values
; of theta_b equal to pi/2 cause a different set of equations to be used.
    if (theta_b eq pi2) then begin

; AIR produces the same radii for different latitudes, causing some overlap.  To
; avoid this problem, if latitudes which are far enough south to be a problem 
; are included in the data, the routine will stop.

      if (min(theta) lt -36/radeg) then begin
        message,'AIR produces overlap of native latitudes south of ',/continue
        print,'-36 with the PROJP1 = 90'
        return
      endif

; points with xi too small are labelled as bad to prevent poor behavior of the
; equation for r_theta
      bad = where(abs(xi) lt 1.d-10)
      good = where(abs(xi) ge 1.d-10)
      r_theta = make_array(size = sz_long)
      if (bad[0] ne -1) then r_theta[bad] = 0.d0
      if (good[0] ne -1) then $
        r_theta[good] = -radeg*alog(cos(xi[good]))/tan(xi[good])

    endif else begin
      xi_b = (pi2 - theta_b)/2.d0

; AIR produces the same radii for different latitudes, causing some overlap.  To
; avoid this problem, if latitudes which are far enough south to be a problem 
; are included in the data, the routine will stop.

      xi_temp = (findgen(90) + 1)/radeg
      radius=-!radeg*(alog(cos(xi_temp))/tan(xi_temp)+alog(cos(xi_b))/$
                                                      tan(xi_b)*tan(xi_temp))
      i = 0
      repeat i = i + 1 $
      until ((radius[i + 1] le radius[i]) or (i eq n_elements(radius) - 2))
      if (i lt (n_elements(radius)- 2)) then min_lat = 90 - 2*radeg*xi_temp[i] $
      else min_lat = -90
      if (min(theta) lt min_lat[0]/radeg) then begin
        message,'AIR produces overlap of native latitudes south of ',/continue
        print,format='(i3,a21,i3)',min_lat[0],' with the PROJP1 = ',projp1
        return
      endif

; points with xi too small are labelled as bad to prevent poor behavior of the
; equation for r_theta
      bad = where(abs(xi) lt 1.d-10)
      good = where(abs(xi) ge 1.d-10)
      r_theta = make_array(size = sz_long)
      if (bad[0] ne -1) then r_theta[bad] = 0.d0
      if (good[0] ne -1) then r_theta[good] = -radeg*(alog(cos(xi[good]))/$
        tan(xi[good]) + alog(cos(xi_b))/tan(xi_b)*tan(xi[good]))
    endelse
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'CYP':begin
    if (n_elements(projp1) eq 0) then begin
      message,/informational,$
           'PROJP1 not set, using default of PROJP1 = 0 for CYP map projection'
      projp1 = 0.d0
    endif
    if (n_elements(projp2) eq 0) then begin
      message,/informational,$
           'PROJP2 not set, using default of PROJP2 = 1 for CYP map projection'
      projp2 = 1.d0
    endif
    if (projp1 eq -projp2) then message,$
      'PROJP1 = -PROJP2 is not allowed for CYP map projection.'

    x = projp2*radeg*phi
    y = radeg*(projp1 + projp2)*sin(theta)/(projp1 + cos(theta))
  end
  
  'CAR':begin
    x = radeg*phi
    y = radeg*theta
  end
  
  'MER':begin
    x = radeg*phi
    y = radeg*alog(tan((pi2 + theta)/2.d0))
  end
  
  'CEA':begin
    if not(keyword_set(projp1)) then message,$
      'CEA map projection requires that PROJP1 keyword be set.'
    if ((projp1 le 0) or (projp1 gt 1)) then message,$
      'CEA map projection requires 0 < PROJP1 <= 1'
    x = radeg*phi
    y = radeg*sin(theta)/projp1
  end
  
  'COP':begin
    if not(keyword_set(projp1)) then message,$
      'COP map projection requires that PROJP1 keyword be set.'
    if not(keyword_set(projp2)) then begin 
      message,/informational,$
      'PROJP2 not set, using default of PROJP2 = PROJP1 for COP map projection'
      projp2=projp1
    endif
    if ((projp1 lt -90) or (projp2 gt 90) or (projp1 gt projp2)) then message,$
 'PROJP1 and PROJP2 must satisfy -90<=PROJP1<=PROJP2<=90 for COP map projection'
    if (projp1 eq -projp2) then message,$
 'COP projection with PROJP1=-PROJP2 is better done as a cylindrical projection'
    theta_1 = projp1/radeg
    theta_2 = projp2/radeg
    theta_a = (theta_1 + theta_2)/2.d0
    bad = where((theta ge theta_a + pi2) or (theta le theta_a - pi2))
    if (bad[0] ne -1) then begin 
      message,/continue,$
  'COP map projection diverges for native latitude = (PROJP1 + PROJP2)/2 +- 90.'
      message,'Remove these points and try again.'
    endif

    alpha = (theta_2 - theta_1)/2.d0
    r_theta = radeg*cos(alpha)*(1.d0/tan(theta_a)-tan(theta-theta_a))
    a_phi = phi*sin(theta_a)
    y_0 = radeg*2.d0*cos(alpha)/sin(2.d0*theta_a)
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)
  end
  
  'COD':begin
    if not(keyword_set(projp1)) then message,$
      'COD map projection requires that PROJP1 keyword be set.'
    if not(keyword_set(projp2)) then begin
      message,/informational,$
     'PROJP2 not set, using default of PROJP2 = PROJP1 for COD map projection'
      projp2 = projp1
    end
    if ((projp1 lt -90) or (projp2 gt 90) or (projp1 gt projp2)) then message,$
 'PROJP1 and PROJP2 must satisfy -90<=PROJP1<=PROJP2<=90 for COD map projection'
    if (projp1 eq -projp2) then message,$
    'COD gives divergent equations for PROJP1 = -PROJP2'
    theta_1 = projp1/radeg
    theta_2 = projp2/radeg

; when projp1 not = projp2 use regular equations
  if (projp1 ne projp2) then begin
      theta_a = (theta_1 + theta_2)/2.d0
      alpha = (theta_2 - theta_1)/2.d0
      r_theta = radeg*(theta_a - theta + alpha/(tan(alpha)*tan(theta_a)))
      a_phi = sin(theta_a)*sin(alpha)*phi/alpha
      y_0 = radeg*(theta_a - pi2 + alpha/(tan(alpha)*tan(theta_a)))
; if the two parameters projp1 and projp2 are equal use the simpler set of
; equations
    endif else begin 
      r_theta = radeg*(theta_1 - theta + 1.d0/tan(theta_1))
      a_phi = phi*sin(theta_1)
      y_0 = radeg*(theta_1 - pi2 + 1.d0/tan(theta_1))

    endelse
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)
  end
  
  'COE':begin
    if not(keyword_set(projp1)) then message,$
      'COE map projection requires that PROJP1 keyword be set.'
    if not(keyword_set(projp2)) then begin
      message,/informational,$
      'PROJP2 not set, using default of PROJP2 = PROJP1 for COE map projection'
      projp2 = projp1
    end
    if ((projp1 lt -90) or (projp2 gt 90) or (projp1 gt projp2)) then message,$
 'PROJP1 and PROJP2 must satisfy -90<=PROJP1<=PROJP2<=90 for COE map projection'
    if (projp1 eq -projp2) then message,$
    'COE gives divergent equations for PROJP1 = -PROJP2'
    theta_1 = projp1/radeg
    theta_2 = projp2/radeg
    gamma = sin(theta_1) + sin(theta_2)
 r_theta=radeg*2.d0*sqrt(1.d0+sin(theta_1)*sin(theta_2)-gamma*sin(theta))/gamma
    a_phi = phi*gamma/2.d0
    y_0 = radeg*2.d0*sqrt(1.d0+sin(theta_1)*sin(theta_2)-gamma)/gamma
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)
  end
  
  'COO':begin
    if not(keyword_set(projp1)) then message,$
      'COO map projection requires that PROJP1 keyword be set.'
    if not(keyword_set(projp2)) then begin
      message,/informational,$
      'PROJP2 not set, using default of PROJP2 = PROJP1 for COO map projection'
      projp2 = projp1
    end
    if ((projp1 lt -90) or (projp2 gt 90) or (projp1 gt projp2)) then message,$
 'PROJP1 and PROJP2 must satisfy -90<=PROJP1<=PROJP2<=90 for COO map projection'
    if (projp1 eq -projp2) then message,$
    'COO gives divergent equations for PROJP1 = -PROJP2'
    theta_1 = projp1/radeg
    theta_2 = projp2/radeg

; for cases where projp1 = projp2, use a simpler formula to calculate c,
; otherwise use the regular formula
    if (projp1 eq projp2) then c = sin(theta_1) else $
    c = alog(cos(theta_2)/cos(theta_1))/alog(tan((pi2-theta_2)/2.d0)/$
    tan((pi2-theta_1)/2.d0))

    alpha = radeg*cos(theta_1)/(c*(tan((pi2-theta_1)/2.d0))^c)
    r_theta = alpha*(tan((pi2-theta)/2.d0))^c
    a_phi = c*phi
    x = r_theta*sin(a_phi)
    y = - r_theta*cos(a_phi)
  end
 
  'BON':begin
    if (n_elements(projp1) eq 0) then message,$
      'BON map projection requires that PROJP1 keyword be set.'
    if ((projp1 lt -90) or (projp1 gt 90)) then message,$
      'PROJP1 must satisfy -90 <= PROJP1 <= 90 for BON map projection'
    if (projp1 eq 0) then message,$
      'PROJP1 = 0 for BON map projection is better done with GLS map projection'

    theta_1 = projp1/radeg
    s = theta_1/abs(theta_1)
    y_0 = 1.d0/tan(theta_1) + theta_1
    a = phi*cos(theta)/(y_0 - theta)
    x = radeg*(y_0 - theta)*sin(a)
    y = radeg*(y_0 - (y_0 - theta)*cos(a))
  end
  
  'PCO':begin
; The equations for x and y are poorly behaved for theta = 0.  Avoid this by
; explicitly assigning values for x and y when theta = 0.
    zero_ind = where(theta eq 0)

; create x and y with same structure as longitude
    x = make_array(size = sz_long)  & y = x
    if (zero_ind[0] ne -1) then begin
      x[zero_ind] = radeg*phi[zero_ind]
      y[zero_ind] = 0.d0
    endif
    good_ind = where(theta ne 0)
    x[good_ind] = radeg*sin(phi[good_ind]*sin(theta[good_ind]))/$
                  tan(theta[good_ind])
    y[good_ind] = radeg*(theta[good_ind]+$
        (1.d0 - cos(phi[good_ind]*sin(theta[good_ind])))/tan(theta[good_ind]))
  end
  
  'GLS':begin
    x = radeg*phi*cos(theta)
    y = radeg*theta
  end
  
  'PAR':begin
    x = phi*(2.d0*cos(2.d0*theta/3.d0) - 1.d0)
    y = pi*sin(theta/3.d0)
  end
  
  'AIT':begin
    alpha = radeg*sqrt(2.d0/(1.d0 + cos(theta)*cos(0.5d0*phi)))
    x = 2.d0*alpha*cos(theta)*sin(0.5d0*phi)
    y = alpha*sin(theta)
  end
  
  'MOL':begin
; Use Newton's method to find a numerical solution to the equation:
;  alpha + 1/2*sin(2*alpha) - 1/2*pi*sin(theta) = 0
    tolerance = 1.0d-14
    alpha = make_array(size = sz_long) 
    repeat begin
    alpha_old = alpha
    alpha = alpha_old - (alpha_old + 0.5*sin(2.d0*alpha_old) - $
            0.5*pi*sin(theta))/(1.d0 + cos(2.d0*alpha_old))
    endrep until (max(abs(alpha - alpha_old)) lt tolerance)

    x = 2.d0^1.5*phi*radeg*cos(alpha)/pi
    y = sqrt(2.d0)*radeg*sin(alpha)
  end
  
  'CSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho =  make_array(size = sz_long) 
    face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the 
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters alpha and
; beta1
    alpha = make_array(size = sz_long) 
    beta1 = alpha
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          alpha[i] = l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
        1:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = n[i]/m[i]
        end
        2:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = n[i]/l[i]
        end
        3:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = -n[i]/m[i]
        end
        4:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = -n[i]/l[i]
        end
        5:begin
          alpha[i] = -l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
      endcase
    end

; define all of the numerical constants to use in determining x and y
    r_0 = 0.577350269
    gam_s = 1.37484847732
    em = 0.004869491981
    gam = -0.13161671474
    ome = -0.159596235474
    d_0 = 0.0759196200467
    d_1 = -0.0217762490699
    c_00 = 0.141189631152
    c_10 = 0.0809701286525
    c_01 = -0.281528535557
    c_20 = -0.178251207466
    c_11 = 0.15384112876
    c_02 = 0.106959469314
    fconst = 45.0d0
    x = fconst*(alpha*gam_s+alpha^3*(1-gam_s)+alpha*beta1^2*(1-alpha^2)*$
        (gam+(em-gam)*alpha^2+(1-beta1^2)*(c_00+c_10*alpha^2+c_01*beta1^2+$
        c_20*alpha^4+c_11*alpha^2*beta1^2+c_02*beta1^4))+alpha^3*(1-alpha^2)*$
        (ome-(1-alpha^2)*(d_0+d_1*alpha^2)))
    y = fconst*(beta1*gam_s+beta1^3*(1-gam_s)+beta1*alpha^2*(1-beta1^2)*$
        (gam+(em-gam)*beta1^2+(1-alpha^2)*(c_00+c_10*beta1^2+c_01*alpha^2+$
        c_20*beta1^4+c_11*beta1^2*alpha^2+c_02*alpha^4))+beta1^3*(1-beta1^2)*$
        (ome-(1-beta1^2)*(d_0+d_1*beta1^2)))


    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=x+xf[face]
        y=y+yf[face]
    endif
  end
  
  'QSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho = make_array( size = sz_long, /NOZERO) 
    face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the 
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters alpha and
; beta1
    alpha = make_array(size = sz_long, /NOZERO) 
    beta1 = make_array(size = sz_long, /NOZERO)
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          alpha[i] = l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
        1:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = n[i]/m[i]
        end
        2:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = n[i]/l[i]
        end
        3:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = -n[i]/m[i]
        end
        4:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = -n[i]/l[i]
        end
        5:begin
          alpha[i] = -l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
      endcase
    end

    x = make_array( size =  sz_long )
    y = x &  xi = y

    s = 2.d0*(((alpha gt abs(beta1)) or (beta1 ge abs(alpha))) - 0.5d0)

    case_1 = where(abs(alpha) gt abs(beta1))
    case_2 = where((abs(alpha) le abs(beta1)) and (beta1 ne 0.d0))
    case_3 = where((alpha eq 0.d0) and (beta1 eq 0.d0))
    if (case_1[0] ne -1) then xi[case_1] = beta1[case_1]/alpha[case_1]
    if (case_2[0] ne -1) then xi[case_2] = alpha[case_2]/beta1[case_2]
    if (case_3[0] ne -1) then xi[case_3] = 0.d0

    fconst=45.0d0
    u = fconst*s*sqrt((1.d0 - rho)/(1.d0 - 1.d0/sqrt(2.d0 + xi^2)))
    v = (u/1.5d1)*radeg*(atan(xi) - asin(xi/sqrt(2.d0*(1.d0 + xi^2))))
    if (case_1[0] ne -1) then begin
      x[case_1] = u[case_1]
      y[case_1] = v[case_1]
    endif
    if (case_2[0] ne -1) then begin
      x[case_2] = v[case_2]
      y[case_2] = u[case_2]
    endif
    if (case_3[0] ne -1) then begin
      x[case_3] = 0.d0
      y[case_3] = 0.d0
    endif
    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=(x+xf[face])
        y=(y+yf[face])
    endif
  end
  
  'TSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho = make_array(size = sz_long, /NOZERO)
    face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the 
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters eta and xi
    eta = make_array(size = sz_long, /NOZERO)
    xi = eta
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          eta[i] = -m[i]
          xi[i] = l[i]
        end
        1:begin
          eta[i] = n[i]
          xi[i] = l[i]
        end
        2:begin
          eta[i] = n[i]
          xi[i] = -m[i]
        end
        3:begin
          eta[i] = n[i]
          xi[i] = -l[i]
        end
        4:begin
          eta[i] = n[i]
          xi[i] = m[i]
        end
        5:begin
          eta[i] = m[i]
          xi[i] = l[i]
        end
      endcase
    endfor
    fconst = 45.0d0
    r_theta = fconst/tan(asin(rho))
    a_phi = atan(xi,-eta)
    x = r_theta*sin(a_phi)
    y = -r_theta*cos(a_phi)
    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=(x+xf[face])
        y=(y+yf[face])
    endif
  end
  else:message,strupcase(projection_type)+$
               ' is not a valid projection type.  Reset CTYPE1 and CTYPE2'
endcase

 if keyword_set(crxy) then begin
          x = x - crxy[0]
          y = y - crxy[1]
 endif

 END
