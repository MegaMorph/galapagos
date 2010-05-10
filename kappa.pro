FUNCTION kappa_funct, kappa
  COMMON sersickappa, sersickapp
  sersn = double(sersickapp)
  f = GAMMA(2*sersn)-2*IGAMMA(2*sersn, kappa)*Gamma(2*sersn)
  return, f
END

FUNCTION kappa, n
;calculate the kappa in the sersic profile as a function of n. n=1 has
;kappa=1.678, n=4 has kappa=7.67
  COMMON sersickappa, sersickapp
  size = n_elements(n)
  k = fltarr(size)
  FOR i = 0ul, size-1 DO BEGIN
    sersickapp = double(n[i] > 0.01 < 10)
    k[i] = zbrent(0., 20., Func_name = 'kappa_funct', MAX_ITERATIONS = 50)
  ENDFOR
  IF size EQ 1 THEN k = k[0]
  return, k
END
