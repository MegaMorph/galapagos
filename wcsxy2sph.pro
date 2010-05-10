;+
; NAME:
;      WCSXY2SPH  
;
; PURPOSE:
;      Convert x and y (map) coordinates to spherical coordinates
; EXPLANATION:
;      To convert x and y (map) coordinates to spherical (longitude and 
;      latitude or sky) coordinates.    This procedure is the inverse of
;      WCSSPH2XY.
;
; CATEGORY:
;      Mapping and Auxilary FITS Routine
;
; CALLING SEQUENCE:
;
;      wcsxy2sph, x, y, longitude, latitude, [map_type], [ CTYPE = ,$
;             FACE = ,PROJP1 = , PROJP2 = ,CRVAL =, CRXY =, LONGPOLE= ]
;
; INPUT PARAMETERS:
;
;       x - x coordinate of data, scalar or vector, in degrees, NOTE: x 
;               increases to to the left, not the right
;       y - y coordinate of data, same number of elements as x, in degrees
;       map_type - optional positional parameter, scalar corresponding to a 
;               particular map projection.  This is not a FITS standard, it is 
;               simply put in to allow function similar to that of less general 
;               map projection procedures (eg AITOFF).  The following list gives
;               the map projection types and their respective numbers.
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
;   CSC    23    Cobe Quadrilateralized     inverse converges poorly
;                Spherical Cube
;   QCS    24    Quadrilateralized
;                Spherical Cube
;   TSC    25    Tangential Spherical Cube
;   HCT    26    HealCart (Cartesian approximation of Healpix)
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       CTYPE - One, two, or three element vector containing 8 character 
;               strings corresponding to the CTYPE1, CTYPE2, and CTYPE3 
;               FITS keywords: 
;
;               CTYPE[0] - first four characters specify standard system
;               ('RA--','GLON' or 'ELON' for right ascension, galactic 
;               longitude or ecliptic longitude respectively), second four 
;               letters specify the type of map projection (eg '-AIT' for 
;               Aitoff projection)
;               CTYPE[1] - first four characters specify standard system
;               ('DEC-','GLAT' or 'ELAT' for declination, galactic latitude
;               or ecliptic latitude respectively; these must match 
;               the appropriate system of ctype1), second four letters of 
;               ctype2 must match second four letters of ctype1.
;               CTYPE(2) - if present must be the 8 character string,'CUBEFACE',
;                only used for spherical cube projections to identify an axis 
;               as containing the face on which each x and y pair of 
;               coordinates lie.
;       FACE - a input variable used for spherical cube projections to 
;               designate the face of the cube on which the x and y 
;               coordinates lie.   Must contain the same number of elements
;               as X and Y.
;       CRVAL - 2 element vector containing standard system coordinates (the 
;               longitude and latitude) of the reference point
;       CRXY - 2 element vector giving the x and y coordinates of the 
;               reference point, if this is not set the offset of the x 
;               coordinate is assumed to be 0.
;       LONGPOLE - native longitude of standard system's North Pole, default
;               is 180 degrees, numeric scalar
;       PROJP1 - scalar with first projection parameter, this may
;               or may not be necessary depending on the map projection used
;       PROJP2 - scalar with second projection parameter, this may
;               or may not be necessary depending on the map projection used
;
; OUTPUT PARAMETERS:
;
;       longitude - longitude of data, same number of elements as x, in degrees
;       latitude - latitude of data, same number of elements as x, in degrees
;
; NOTES:
;       The conventions followed here are described in more detail in the paper
;      "Representations of Celestial Coordinates in FITS" by Calabretta &
;       Greisen (2002, A&A, 395, 1077, also see 
;       http://www.aoc.nrao.edu/~egreisen).   The general scheme 
;       outlined in that article is to convert x and y coordinates into a 
;       "native" longitude and latitude and then rotate the system into one of 
;       three generally recognized systems (celestial, galactic or ecliptic).
;
;       This procedure necessitates two basic sections.  The first converts 
;       x and y coordinates to "native" coordinates while the second converts 
;       "native" to "standard" coordinates.  The first section contains the 
;       guts of the code in which all of the map projection is done.  The 
;       second step is performed by WCS_ROTATE and only involves rotation of 
;       coordinate systems.  WCSXY2SPH can be called in a form similar to 
;       AITOFF, EQPOLE, or QDCB by calling wcsxy2sph with a fifth parameter 
;       specifying the map projection by number and by not using any of the 
;       keywords related to the map projection type (eg ctype1 and ctyp2).
;
; PROCEDURE:
;       The first task of the procedure is to do general error-checking to 
;       make sure the procedure was called correctly and none of the 
;       parameters or keywords conflict.  This is particularly important 
;       because the procedure can be called in two ways (either using 
;       FITS-type keywords or using a number corresponding a map projection 
;       type).  All variables are converted into double precision values.
;
;       The second task of the procedure is to take x and y coordinates and 
;       convert them into "native" latitude and longitude coordinates.  
;       Map-specific error-checking is done at this time.  All of the 
;       equations were obtained from "Representations of Celestial 
;       Coordinates in FITS" and cases needing special attention are handled 
;       appropriately (see the comments with individual map projections for 
;       more information on special cases).     WCS_ROTATE is then called to 
;       convert the "native" coordinates to "standard" coordinates by rotating
;       the coordinate system.  This rotation is governed by the keywords 
;       CRVAL, and LONGPOLE.  The transformation is a straightforward 
;       application of euler angles.  Finally, longitude values are converted 
;       into the range from 0 to 360 degrees.
;
; COMMON BLOCKS:
;       none
; PROCEDURES CALLED:
;       WCS_ROTATE
;
; COPYRIGHT NOTICE:
;
;       Copyright 1991, The Regents of the University of California. This
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
; 1.1    8/31/93
; 1.2    9/12/93   W. Landsman Vectorized CRXY, CRVAL, CTYPE
; 1.3    29/12/93  I. Freedman Eliminated LU decomposition
; 1.4    22/09/94  W. Landsman If scalar input, then scalar output
; 1.5    02/03/05  W. Landsman Change variable name BETA for V4.0 compatibility
; 1.6    06/07/05  W. Landsman Change loop index from integer to long
;       Converted to IDL V5.0   W. Landsman   September 1997
; 1.7    02/18/99  W. Landsman Fixed implementation of ARC algorithm
;-

PRO wcsxy2sph, x, y, longitude, latitude, map_type, ctype=ctype, $
              face=face,projp1=projp1,projp2=projp2,$
              crval=crval,crxy = crxy,longpole=longpole

; Define angle constants 

 pi = !DPI
 radeg = 57.295779513082323d0
 pi2 = pi/2.0d
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','GLS',$
            'PAR','AIT','MOL','CSC','QSC','TSC','HCT']
 origin = [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,  0, 0, 0, $
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

; check to see that enough parameters (at least 4) were sent
 if ( N_params() lt 4 ) then begin
    print,'Syntax - WCSXY2SPH, x, y, longitude, latitude,[ map_type,  '
    print,'             CTYPE= , FACE=, PROJP1= , PROJP2= , CRVAL= , CRXY= , '
    print,'             LONGPOLE = ]'
    return
 endif else if (n_params() eq 5) then begin
    if keyword_set(ctype) then message,$
  'Use either the MAP_TYPE positional parameter or set the projection type' + $
    'with CTYPE, but not both.'

; set projection_type string using map_type parameter (a number) 
  if (n_elements(map_type) ne 0) then begin 
    map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
               'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','GLS',$
               'PAR','AIT','MOL','CSC','QSC','TSC','HCT']
    projection_type = map_types[map_type]
  endif else message,'MAP_TYPE must be >= 0 and <= 25, it was set to '+map_type

endif else if (n_params() eq 4) then begin

; check to see that CTYPE is set correctly 

  if N_elements( ctype ) GE 1 then begin
        ctype1 = strtrim(ctype[0],2)     
        if strlen(ctype1) LT 8 then message,'ERROR - ' + strupcase(ctype1) + $
               ' is not a valid spherical projection type.'  
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

; GENERAL ERROR CHECKING

; find the number of elements in each of the data arrays
 n_x = n_elements(x)
 n_y = n_elements(y)
 sz_x = size(x)
 sz_y = size(y)

; convert all scalar data values into arrays with one element each
 if (n_x eq 1) then x=dblarr(1)+x
 if (n_y eq 1) then y=dblarr(1)+y

; check to see that the data arrays have the same size
 if (n_x ne n_y) then $
        message,'The arrays X and Y must have the same number of elements.'

; this sets the default map projection type for the cases when map_type or
; projection_type is set to 'DEF' or if projection_type is not set at this
; point.  As suggested in 'Representations of Celestial Coordinates in FITS'
; the default type is set to CAR (Cartesian) the simplest of all projections.
if ((n_elements(projection_type) eq 0) or (projection_type eq 'DEF')) then $
   projection_type='CAR'
 
; Check to make sure all the correct parameters and keywords are set for
; spherical projections.
 if ((N_elements(ctype) EQ 3) or keyword_set(face) or  $
    (projection_type eq 'CSC') or $
    (projection_type eq 'QSC') or (projection_type eq 'TSC')) then begin

  if not(keyword_set(face)) then noface = 1 else noface = 0

endif

; check to see if the x and y offsets are set properly.  If not, break out
; of program.  If so, apply offsets.  If the x and y offsets are not set,
; then assume they are zero.

 if ( (keyword_set(crxy)) and N_elements(crxy) NE 2) then $
     message,'Offset keyword CRXY must contain 2 elements'

 if keyword_set(crxy) then begin
        xx = double(x - crxy[0] )
        yy = double(y - crxy[1] )
 endif else begin
        xx = double(x)
        yy = double(y)
 endelse

 if  ( N_elements(crval) eq 1 ) then $
        message,'CRVAL keyword must be a 2 element vector'

; BRANCH BY MAP PROJECTION TYPE
case strupcase(projection_type) of
  'AZP':begin
    if (n_elements(projp1) eq 0) then message,$
      'AZP map projection requires the keyword PROJP1 to be set'
    if (projp1 lt 0) then message,$
      'AZP map projection requires the keyword PROJP1 >= 0'
    rho = sqrt(xx^2 + yy^2)/(radeg*(projp1 + 1.d0))
    theta = atan(1.d0,rho) - asin(rho*projp1/sqrt(rho^2 + 1.d0))
    phi = atan(xx,-yy)
  end  

  'TAN':begin
    theta = atan(radeg/sqrt(xx^2 + yy^2))
    phi = atan(xx,-yy)
  end

  'SIN':begin
    theta = acos(sqrt(xx^2 + yy^2)/radeg)
    phi = atan(xx,-yy)
  end

  'STG':begin
    theta = pi2 - 2*atan(sqrt(xx^2 + yy^2)/(2.d0*radeg))
    phi = atan(xx,-yy)
  end

  'ARC':begin
    theta = pi2 - sqrt(xx^2 + yy^2)/radeg
    phi = atan(xx,-yy)
  end

  'ZPN':  message,'ZPN is not implemented due to general uselessness.'

  'ZEA':begin
    theta = pi2 - 2.d0*asin(sqrt(xx^2 + yy^2)/(2.d0*radeg))
    phi = atan(xx,-yy)
  end

  'AIR':begin
    if not(keyword_set(projp1)) then begin
      message,/informational,$
          'PROJP1 not set, using default of PROJP1 = 90 for AIR map projection'
      projp1 = 9.d1
    endif

; Numerically solve the equation for xi, by iterating the equation for xi.
; The default initial value for xi is 30 degrees, but for some values of 
; x and y, this causes an imaginary angle to result for the next iteration of
; xi.  Unfortunately, this causes the value of xi to converge to an incorrect
; value, so the initial xi is adjusted to avoid this problem.
    xi = pi/6.d0
    theta_b = projp1/radeg
    if (theta_b ne pi2) then $
      a = alog(cos((pi2-theta_b)/2.d0))/tan((pi2-theta_b)/2.d0) $
    else a = 0.d0

    repeat begin
      bad=where(abs(exp(-(sqrt(xx^2 + yy^2)/radeg + a*tan(xi))*tan(xi))) gt 1)
      if (bad[0] ne -1) then xi[bad] = xi[bad]/2.d0
    endrep until (bad[0] eq -1)

    tolerance = 1.d-12
    repeat begin

      xi_old = xi
      xi = acos(exp(-(sqrt(xx^2 + yy^2)/radeg + a*tan(xi))*tan(xi)))

    endrep until (max(abs(xi_old - xi)) lt tolerance)

    theta = pi2 - 2.d0*xi
    phi = atan(xx,-yy)
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

    eta = yy/((projp1 + projp2)*radeg)
    theta = atan(eta,1) + asin(eta*projp1/sqrt(eta^2 + 1.d0))
    phi = xx/(projp2*radeg)
  end
  
  'CAR':begin
    phi = xx/radeg
    theta = yy/radeg
  end
  
  'HCT':begin
    phi = xx/radeg

    w_np = where(yy ge 45, n_np)
    w_eq = where((yy lt 45) and (yy gt -45), n_eq)
    w_sp = where(yy le -45, n_sp)
    theta = dblarr(n_elements(yy)) 
    
    if n_np gt 0 then theta[w_np] =  asin(1-(yy/45-2)^2/3.d)
    if n_eq gt 0 then theta[w_eq] =  asin((yy/45)*2./3.d)
    if n_sp gt 0 then theta[w_sp] = -asin(1-(yy/45+2)^2/3.d)
  end
  
  'MER':begin
    phi = xx/radeg
    theta = 2*atan(exp(yy/radeg)) - pi2
  end
  
  'CEA':begin
    if not(keyword_set(projp1)) then message,$
      'CEA map projection requires that PROJP1 keyword be set.'
    if ((projp1 le 0) or (projp1 gt 1)) then message,$
      'CEA map projection requires 0 < PROJP1 <= 1'
    phi = xx/radeg
    theta = asin(yy*projp1/radeg)
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

    theta_a = (projp2 + projp1)/(2.d0*radeg)
    alpha = (projp2 - projp1)/(2.d0*radeg)
    y_0 = radeg*2*cos(alpha)/sin(2*theta_a)
    phi = atan(xx,y_0-yy)/sin(theta_a)
    theta = theta_a + atan(1.d0/tan(theta_a) - sqrt(xx^2 + (y_0-yy)^2)/$
          (radeg*cos(alpha)))
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

; use general set of equations for projp1 not = projp2
    if (projp1 ne projp2) then begin
      theta_a = (projp2 + projp1)/(2.d0*radeg)
      alpha = (projp2 - projp1)/(2.d0*radeg)
      y_0 = radeg*(theta_a - pi2 + alpha/(tan(alpha)*tan(theta_a)))
      phi = alpha*atan(xx,y_0 - yy)/(sin(theta_a)*sin(alpha))
      theta = theta_a + alpha/(tan(alpha)*tan(theta_a)) - $
              sqrt(xx^2 + (y_0 - yy)^2)/radeg

; use special set of equations for projp1 = projp2
    endif else begin
      theta_1 = projp1/radeg
      y_0 = radeg*(theta_1 - pi2 + 1.d0/tan(theta_1))
      phi = atan(xx,y_0 - yy)/sin(theta_1)
      theta = theta_1 + 1.d0/tan(theta_1) - sqrt(xx^2 + (y_0 - yy)^2)/radeg
    endelse
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
    s_1 = sin(projp1/radeg)
    s_2 = sin(projp2/radeg)
    gamma = s_1 + s_2
    y_0 = radeg*2.d0*sqrt(1.d0 + s_1*s_2 - gamma)/gamma
    phi = 2*atan(xx,y_0 - yy)/gamma
    theta = asin((1.d0+s_1*s_2-(xx^2+(y_0-yy)^2)*(gamma/(2.d0*radeg))^2)/gamma)
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
    theta_1 = projp1/radeg
    theta_2 = projp2/radeg

; calculate value of c in simpler fashion if projp1 = projp2
    if (projp1 eq projp2) then c = sin(theta_1) else $
    c = alog(cos(theta_2)/cos(theta_1))/alog(tan((pi2-theta_2)/2.d0)/$
    tan((pi2-theta_1)/2.d0))

    alpha = radeg*cos(theta_1)/(c*(tan((pi2-theta_1)/2.d0))^c)
    phi = atan(xx,-yy)/c
    theta = pi2 - 2*atan((sqrt(xx^2 + yy^2)/alpha)^(1.d0/c))
  end
 
  'BON':begin
    if (n_elements(projp1) eq 0) then message,$
      'BON map projection requires that PROJP1 keyword be set.'
    if ((projp1 lt -90) or (projp1 gt 90)) then message,$
      'PROJP1 must satisfy -90 <= PROJP1 <= 90 for BON map projection'
    if (projp1 eq 0) then message,$
      'PROJP1 = 0 for BON map projection is better done with GLS map projection'
    theta_1 = projp1/radeg
    y_0 = 1.d0/tan(theta_1) + theta_1
    s = theta_1/abs(theta_1)
    theta = y_0 - s*sqrt(xx^2 + (y_0*radeg - yy)^2)/radeg 
    phi = s*(y_0 - theta)*atan(s*xx/(y_0*radeg - theta),$
                               (y_0*radeg - yy)/(y_0*radeg - theta))/cos(theta)
  end
  
  'PCO':begin
; Determine where y = 0 and assign theta to 0 for these points.  The reason
; for doing this separately is that the intial condition for theta in the
; numerical solution is sign(y)*45 which only works for y not = 0.
    bad = where(yy eq 0)
    good = where(yy ne 0)
    theta = double(xx - xx)
    if (bad[0] ne -1) then theta[bad] = 0.d0

; Find theta numerically.
    tolerance = 1.d-11
    tolerance_2 = 1.d-11
    if (good[0] ne -1) then begin
      theta_p = double(xx - xx)
      theta_p[good] = pi2*yy[good]/abs(yy[good])
      theta_n = double(xx - xx)
      f_p = double(xx - xx)
      f_p[good] = xx[good]^2 - 2.d0*radeg*(yy[good] - radeg*theta_p[good])/$
                  tan(theta_p[good]) + (yy[good] - radeg*theta_p[good])^2
      f_n = double(xx - xx) - 999.d0
      lambda = double(xx - xx)
      f = double(xx - xx)
      repeat begin
        case_1 = where((yy ne 0.d0) and (f_n lt (-1.d2)))
        case_2 = where((yy ne 0.d0) and (f_n ge (-1.d2)))
        if (case_1[0] ne -1) then lambda[case_1] = 0.5d0
        if (case_2[0] ne -1) then $
          lambda[case_2] = f_p[case_2]/(f_p[case_2] - f_n[case_2])
        lambda[good] = 1.d-1 > (9.d-1 < lambda[good])
        theta[good] = (1.d0 - lambda[good])*theta_p[good] + $
                      lambda[good]*theta_n[good]
        f[good] = xx[good]^2 - 2.d0*radeg*(yy[good] - radeg*theta[good])/$
                  tan(theta[good]) + (yy[good] - radeg*theta[good])^2
        neg = where((yy ne 0.d0) and (f lt 0.d0))
        pos = where((yy ne 0.d0) and (f gt 0.d0))
        if (neg[0] ne -1) then begin
          f_n[neg] = f[neg]
          theta_n[neg] = theta[neg]
        end
        if (pos[0] ne -1) then begin
          f_p[pos] = f[pos]
          theta_p[pos] = theta[pos]
        end
      endrep until ((max(abs(theta_p - theta_n)) lt tolerance) or $
                    (max(abs(f)) lt tolerance_2))
    endif

; Determine phi differently depending on whether theta = 0 or not.
    bad = where(theta eq 0.d0)
    good = where(theta ne 0.d0)
    phi = double(x - x)
    if (bad[0] ne -1) then phi[bad] = xx[bad]/radeg
    phi[good] = atan(xx[good]/radeg*tan(theta[good]),$
       1.d0 - (yy[good]/radeg - theta[good])*tan(theta[good]))/sin(theta[good])
  end
  
  'GLS':begin
    phi = xx/(radeg*cos(yy/radeg))
    theta = yy/radeg
  end
  
  'PAR':begin
    theta = 3.d0*asin(yy/pi)
    phi = xx/(1.d0 - 4.d0*(yy/pi)^2)
  end
  
  'AIT':begin
  z = sqrt(1.d0 - (xx/(4.d0*radeg))^2 - (yy/(2.d0*radeg))^2)
  phi = 2.d0*atan(z*xx/(2.d0*radeg),2.d0*z^2 - 1.d0)
  temp = where(phi gt pi)
  if (temp[0] ne -1) then phi[temp] = phi[temp] - 2*pi
  theta = asin(yy*z/radeg)
  end
  
  'MOL':begin
    phi = pi*xx/(radeg*2.d0*sqrt(2.d0 - (yy/radeg)^2))
    theta = asin(2.d0*asin(yy/(sqrt(2.d0)*radeg))/pi + $
                 yy*sqrt(2.d0 - (yy/radeg)^2)/1.8d2)
  end
  
  'CSC':begin
    xx = xx/4.5d1
    yy = yy/4.5d1

;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif

; Define array of numerical constants used in determining alpha and beta1.
    p = dblarr(7,7)
    p[0,0] = -0.27292696d0
    p[1,0] = -0.07629969d0
    p[0,1] = -0.02819452d0
    p[2,0] = -0.22797056d0
    p[1,1] = -0.01471565d0
    p[0,2] = 0.27058160d0
    p[3,0] = 0.54852384d0
    p[2,1] = 0.48051509d0
    p[1,2] = -0.56800938d0
    p[0,3] = -0.60441560d0
    p[4,0] = -0.62930065d0
    p[3,1] = -1.74114454d0
    p[2,2] = 0.30803317d0
    p[1,3] = 1.50880086d0
    p[0,4] = 0.93412077d0
    p[5,0] = 0.25795794d0
    p[4,1] = 1.71547508d0
    p[3,2] = 0.98938102d0
    p[2,3] = -0.93678576d0
    p[1,4] = -1.41601920d0
    p[0,5] = -0.63915306d0
    p[6,0] = 0.02584375d0
    p[5,1] = -0.53022337d0
    p[4,2] = -0.83180469d0
    p[3,3] = 0.08693841d0
    p[2,4] = 0.33887446d0
    p[1,5] = 0.52032238d0
    p[0,6] = 0.14381585d0

; Calculate alpha and beta1 using numerical constants
    sum = double(x - x)
    for j = 0,6 do for i = 0,6 - j do sum = sum + p[i,j]*xx^(2*i)*yy^(2*j)
    alpha = xx + xx*(1 - xx^2)*sum

    sum = double(x - x)
    for j = 0,6 do for i = 0,6 - j do sum = sum + p[i,j]*yy^(2*i)*xx^(2*j)
    beta1 = yy + yy*(1 - yy^2)*sum

; Calculate theta and phi from alpha and beta1; the method depends on which
; face the point lies on
    phi = double(x - x)
    theta = double(x - x)
    for i = 0l, n_x - 1 do begin
      case face[i] of
        0:begin
          if (beta1[i] eq 0.d0) then begin
            if (alpha[i] eq 0.d0) then begin
              theta[i] = pi2
; uh-oh lost information if this happens
              phi[i] = 0.d0
            endif else begin
              phi[i] = alpha[i]/abs(alpha[i])*pi2
              theta[i] = atan(abs(1.d0/alpha[i]))
            endelse
          endif else begin
            phi[i] = atan(alpha[i],-beta1[i])
            theta[i] = atan(-cos(phi[i])/beta1[i])
          endelse
; ensure that the latitudes are positive
          theta[i] = abs(theta[i])
        end
        1:begin
          phi[i] = atan(alpha[i])
          theta[i] = atan(beta1[i]*cos(phi[i]))
        end
        2:begin
          if (alpha[i] eq 0.d0) then phi[i] = pi2 else $
            phi[i] = atan(-1.d0/alpha[i])
          if (phi[i] lt 0.d0) then phi[i] = phi[i] + pi
          theta[i] = atan(beta1[i]*sin(phi[i]))
        end
        3:begin
          phi[i] = atan(alpha[i])
          if (phi[i] gt 0.d0) then phi[i] = phi[i] - pi else $
          if (phi[i] lt 0.d0) then phi[i] = phi[i] + pi
          theta[i] = atan(-beta1[i]*cos(phi[i]))
        end
        4:begin
          if (alpha[i] eq 0.d0) then phi[i] = -pi2 else $
            phi[i] = atan(-1.d0/alpha[i])
          if (phi[i] gt 0.d0) then phi[i] = phi[i] - pi
          theta[i] = atan(-beta1[i]*sin(phi[i]))
        end
        5:begin
          if (beta1[i] eq 0.d0) then begin
            if (alpha[i] eq 0.d0) then begin
              theta[i] = -pi2
; uh-oh lost information if this happens
              phi[i] = 0.d0
            endif else begin
              phi[i] = -alpha[i]/abs(alpha[i])*pi2 
              theta[i] = -atan(abs(1.d0/alpha[i]))
            endelse
          endif else begin
            phi[i] = atan(alpha[i],beta1[i])
            theta[i] = atan(-cos(phi[i])/beta1[i])
          endelse
; ensure that the latitudes are negative
          theta[i] = -abs(theta[i])
        end

      endcase
    endfor
  end
  
  'QSC':begin

    xx=xx/45.0d0
    yy=yy/45.0d0
;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif


; First determine the quadrant in which each points lies.  Calculate the
; ratio (alpha/beta1) for each point depending on the quadrant.  Finally,
; use this information and the face on which the point lies to calculate
; phi and theta.
    theta = double(x - x)
    phi = double(x - x)
    rho = double(x - x)
    ratio = double(x - x)
    larger = double(x - x)
    smaller = double(x - x)

    temp = where(abs(yy) ge abs(xx), Ntemp)
    if Ntemp GT 0 then larger[temp] = yy[temp]
    temp = where(abs(xx) gt abs(yy), Ntemp )
    if Ntemp GT 0 then larger[temp] = xx[temp]
    temp = where(abs(yy) lt abs(xx), Ntemp )
    if Ntemp GT 0 then smaller[temp] = yy[temp]
    temp = where(abs(xx) le abs(yy), Ntemp)
    if Ntemp GT 0 then smaller[temp] = xx[temp]

    temp = where(larger ne 0.d0, Ntemp)
    if Ntemp GT 0 then ratio[temp] = sin(pi/1.2d1*smaller[temp]/larger[temp])/$
                      (cos(pi/1.2d1*smaller[temp]/larger[temp]) - sqrt(0.5d0))

    temp = where(larger eq 0.d0, Ntemp)
    if Ntemp GT 0 then ratio[temp] = 1.d0
    rho = 1.d0 - (larger)^2*(1.d0 - 1.d0/sqrt(2.d0 + ratio^2))

    temp = where((abs(xx) gt abs(yy)) and (ratio ne 0.d0), Ntemp)
    if Ntemp GT 0 then ratio[temp] = 1.d0/ratio[temp]

    temp = where((abs(xx) gt abs(yy)) and (ratio eq 0.d0), Ntemp)
; use a kludge to produce the correct value for 1/0 without generating an error 
    if Ntemp GT 0 then ratio[temp] = tan(pi2)

    for i = 0l, n_x-1 do begin
      case face[i] of
        0:begin
          if (xx[i] ne 0.d0) then phi[i] = atan(-ratio[i]) else $
          if (yy[i] le 0.d0) then phi[i] = 0.d0 else $
          if (yy[i] gt 0.d0) then phi[i] = pi

          if (yy[i] ne 0.d0) then theta[i] = asin(rho[i]) else $
          if (xx[i] le 0.d0) then theta[i] = -pi2 else $
          if (xx[i] gt 0.d0) then theta[i] = pi2

          if (yy[i] gt 0.d0) then begin
            if (xx[i] lt 0.d0) then phi[i] = phi[i] - pi $
            else if (xx[i] gt 0.d0) then phi[i] = phi[i] + pi
          endif
        end
        1:begin
          if (xx[i] ne 0.d0) then begin
            if (yy[i] ne 0.d0) then $
             phi[i] = xx[i]/abs(xx[i])*acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else phi[i] = xx[i]/abs(xx[i])*acos(rho[i]) 
          endif else phi[i] = 0.d0
          if (yy[i] ne 0.d0) then theta[i] = yy[i]/abs(yy[i])*acos(rho[i]/$
                                            cos(phi[i])) else theta[i] = 0.d0
        end
        2:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
               phi[i] = pi - asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else if (xx[i] lt 0.d0) then $
               phi[i] = asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else phi[i] = pi2
            theta[i] = yy[i]/abs(yy[i])*acos(rho[i]/abs(sin(phi[i])))
          endif else begin 
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = pi - asin(rho[i]) $
            else if (xx[i] lt 0.d0) then phi[i] = asin(rho[i]) $
            else phi[i] = pi2
          endelse
        end
        3:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
              phi[i] = acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                            (ratio[i]^2 + rho[i]^2))) - pi $
            else if (xx[i] lt 0.d0) then $
              phi[i] = -acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                       (ratio[i]^2 + rho[i]^2))) + pi $
            else phi[i] = pi
            theta[i] = yy[i]/abs(yy[i])*acos(-rho[i]/cos(phi[i])) 
          endif else begin
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = acos(rho[i]) - pi $
            else if (xx[i] lt 0.d0) then phi[i] = -acos(rho[i]) + pi $
            else phi[i] = pi
          endelse
        end
        4:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
               phi[i] = -asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else if (xx[i] lt 0.d0) then $
               phi[i] = asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) - pi $
            else phi[i] = -pi2
            theta[i] = yy[i]/abs(yy[i])*acos(-rho[i]/sin(phi[i]))
          endif else begin
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = -asin(rho[i]) $
            else if (xx[i] lt 0.d0) then phi[i] = asin(rho[i]) - pi $
            else phi[i] = -pi2
          endelse
        end
        5:begin
          if (xx[i] ne 0.d0) then phi[i] = atan(ratio[i]) $
          else if (yy[i] le 0.d0) then phi[i] = pi $
          else if (yy[i] gt 0.d0) then phi[i] = 0.d0

          if (yy[i] ne 0.d0) then theta[i] = asin(-rho[i]) $
          else if (xx[i] le 0.d0) then theta[i] = -pi2 $
          else if (xx[i] gt 0.d0) then theta[i] = pi2

          if (yy[i] lt 0.d0) then begin
            if (xx[i] lt 0.d0) then phi[i] = phi[i] - pi $
            else if (xx[i] gt 0.d0) then phi[i] = phi[i] + pi
          endif
        end
      endcase
    endfor
  end
  
  'TSC':begin

    xx=xx/45.0d0
    yy=yy/45.0d0
;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif

    rho = sin(atan(1.0d0/sqrt(xx^2 + yy^2)))
    phi = double(x - x)
    theta = double(x - x)
    for i = 0l, n_x - 1 do begin
      case face[i] of
        0:begin
          phi[i] = atan(xx[i],-yy[i])
          theta[i] = asin(rho[i])
        end
        1:begin
          if (xx[i] ne 0.d0) then begin
            if (xx[i] ge 0.d0) then $
             phi[i] = atan(sqrt((1.d0/rho[i]^2- 1.d0)/(1 + (yy[i]/xx[i])^2))) $
            else phi[i] =atan(-sqrt((1.d0/rho[i]^2 - 1.d0)/$
                               (1 + (yy[i]/xx[i])^2)))
            theta[i] = atan(yy[i]/xx[i]*sin(phi[i]))
          endif else begin
            phi[i] = 0.d0
            if (yy[i] ge 0.d0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        2:begin
; The point theta = 0, phi = Pi/2 lies in this region, allowing 
; rho = Cos[theta]*Sin[phi] to be 1, causing an infinite quantity in the
; equation for phi
          if (rho[i] eq 1.d0) then begin
            phi[i] = pi2
            theta[i] = 0.d0
          endif else if (xx[i] gt 1.d-14) then begin
           phi[i] = atan(-sqrt((1.d0 + (yy[i]/xx[i])^2)/$
                                (1.d0/rho[i]^2 - 1.d0)))+pi
            theta[i] = atan(-yy[i]/xx[i]*cos(phi[i]))
          endif else if (xx[i] lt -1.d-14) then begin
            phi[i]=atan(sqrt((1.d0+(yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))
            theta[i] = atan(-yy[i]/xx[i]*cos(phi[i]))
          endif else begin
             phi[i] = pi2
            if (yy[i] ge 0) then theta[i] = acos(rho[i]/sin(phi[i])) $
            else theta[i] = -acos(rho[i]/sin(phi[i]))
          endelse
        end
        3:begin
          if (abs(xx[i]) ge 1.d-5) then begin
            if (xx[i] gt 0.d0) then $
           phi[i] = atan(sqrt((1.d0/rho[i]^2 - 1.d0)/$
                          (1 + (yy[i]/xx[i])^2)))-pi $
        else phi[i] = atan(-sqrt((1.d0/rho[i]^2 - 1.d0)/$
                            (1 + (yy[i]/xx[i])^2)))+pi
            theta[i] = atan(-yy[i]/xx[i]*sin(phi[i]))
          endif else begin
            if (xx[i] ge 0.d0) then phi[i] = -pi $
            else phi[i] = pi
            if (yy[i] ge 0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        4:begin
          if (rho[i] eq 1.d0) then begin
            phi[i] = -pi2
            theta[i] = atan(yy[i]/xx[i])
          endif else if (xx[i] gt 1.d-14) then begin
           phi[i]=atan(-sqrt((1.d0 + (yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))
            theta[i] = atan(yy[i]/xx[i]*cos(phi[i]))
          endif else if (xx[i] lt -1.d-14) then begin
            phi[i]=atan(sqrt((1.d0+(yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))-pi
            theta[i] = atan(yy[i]/xx[i]*cos(phi[i]))
          endif else begin
             phi[i] = 1.5d0*!pi
            if (yy[i] ge 0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        5:begin
          phi[i] = atan(xx[i],yy[i])
          theta[i] = asin(-rho[i])
        end

      endcase
    endfor

  end
  else:message,strupcase(projection_type) + $
               ' is not a valid projection type.  Reset CTYPE1 and CTYPE2'

endcase

; Convert from "native" coordinate system to "standard" coordinate system
; if the CRVAL keyword is set.  Otherwise, assume the map projection is 
; complete 

 phi = phi*radeg
 theta = theta*radeg
 if ( N_elements(crval) GE 2 ) then begin

  if (n_elements(longpole) eq 0) then longpole = 1.8d2

  if N_elements(map_type) EQ 0 then $
           map_type = where(projection_type EQ map_types)
   wcs_rotate, longitude, latitude, phi, theta, crval, longpole=longpole, $
           origin = origin[map_type[0]], /REVERSE

 endif else begin    ;no rotation from standard to native coordinates

  latitude = theta
  longitude = phi

endelse

; CONVERT LONGITUDE FROM -180 TO 180 TO 0 TO 360

 temp = where(longitude lt 0.d0, Nneg)
 if (Nneg GT 0) then longitude[temp] = longitude[temp] + 3.6d2
 temp = where(longitude ge 3.6d2-1.d-2, Nneg)
 if (Nneg GT 0) then longitude[temp] = longitude[temp] - 3.6d2

; If input params were scalars, then convert all params back to scalar

 if sz_x[0] EQ 0 then begin
        x = x[0]  & y = y[0] 
        longitude = longitude[0] & latitude = latitude[0]
 endif

 return
 end
