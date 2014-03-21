FUNCTION read_galfit_sersic, im_file
;read a GALFIT output image header and extract Sersic parameters
;main object must be second component
;sky must be first component
;returns an array with:
;x,y,mag,re,n,q,pa,sky,xerr,yerr,magerr,reerr,nerr,qerr,paerr
;if input file does not exist return value is -1

  IF file_test(im_file) THEN BEGIN
    hd = headfits(im_file, exten = 2)
    mag0 = sxpar(hd, '2_MAG')
    mag = float(strmid(mag0, 0, strpos(mag0, '+/-')))
    magerr = float(strmid(mag0, strpos(mag0, '+/-')+3, strlen(mag0)))
    re0 = sxpar(hd, '2_RE')
    re = float(strmid(re0, 0, strpos(re0, '+/-')))
    reerr = float(strmid(re0, strpos(re0, '+/-')+3, strlen(re0)))
    n0 = sxpar(hd, '2_N')
    n = float(strmid(n0, 0, strpos(n0, '+/-')))
    nerr = float(strmid(n0, strpos(n0, '+/-')+3, strlen(n0)))
    q0 = sxpar(hd, '2_AR')
    q = float(strmid(q0, 0, strpos(q0, '+/-')))
    qerr = float(strmid(q0, strpos(q0, '+/-')+3, strlen(q0)))
    pa0 = sxpar(hd, '2_PA')
    pa = float(strmid(pa0, 0, strpos(pa0, '+/-')))
    paerr = float(strmid(pa0, strpos(pa0, '+/-')+3, strlen(pa0)))
    x0 = sxpar(hd, '2_XC')
    x = float(strmid(x0, 0, strpos(x0, '+/-')))
    xerr = float(strmid(x0, strpos(x0, '+/-')+3, strlen(x0)))
    y0 = sxpar(hd, '2_YC')
    y = float(strmid(y0, 0, strpos(y0, '+/-')))
    yerr = float(strmid(y0, strpos(y0, '+/-')+3, strlen(y0)))
    s0 = sxpar(hd, '1_SKY')
    s = float(strmid(s0, 0, strpos(s0, '+/-')))

    return, [x, y, mag, re, n, q, pa, s, $
             xerr, yerr, magerr, reerr, nerr, qerr, paerr]
  ENDIF ELSE return, -1
END
