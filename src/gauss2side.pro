PRO curvefit_gauss2side, x, r, f, pder
;x: x-values
;r: the fit parameters [normalisation, centre, width-left, width-right]
;f: y-values
  !except = 0
  f = fltarr(n_elements(x))
  idx = where(x LE r[1], ct)
  IF ct GT 0 THEN f[idx] = r[0]*exp(-0.5*((x[idx]-r[1])/float(r[2]))^2)
  idx = where(x GT r[1], ct)
  IF ct GT 0 THEN f[idx] = r[0]*exp(-0.5*((x[idx]-r[1])/float(r[3]))^2)
  !except = 1
END

FUNCTION gauss2side, x, p
;function to calculate a gaussian with width1 to the left and width2
;to the right of the maximum
;the order of the prarameters is: peak,centre,width1,width2
  !except = 0
  y = p[0]*exp(-0.5*((x-p[1])/float(p[2]))^2)
  idx = where(x GT p[1], ct)
  IF ct GT 0 THEN y[idx] = p[0]*exp(-0.5*((x[idx]-p[1])/float(p[3]))^2)
  !except = 1
  return, y
END

;    yfit = curvefit(x, y, noweight, par, sigma, $
;                    FUNCTION_name = 'curvefit_gauss2side', /noderivative, $
;                    status = status, iter = iter)
