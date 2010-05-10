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
