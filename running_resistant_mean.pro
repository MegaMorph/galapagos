PRO running_resistant_mean, x, y, xbin, ox, oy, od, osig, binsize = binsize, $
  sigcut = sigcut, bin_values = bin_values, xrange = xrange
;pro to calculate a running resistant_mean for a given x,y plot
;x,y: input x,y data pairs
;xbin: bin separation and binsize
;binsize=binsize: if set a binsize other than xbin can be defined,
;e.g. one may want a mean every 0.5, but have a bin width of 1, thus
;creating some overlap
;ox,oy: output data pairs. oy is the actual mean
;od: error of the mean value
;osig: scatter of the distribution (dispersion)
;sigcut: sigma-clipping level for the resistant_mean
;bin_values: calculate mean values at bin_values
  IF NOT keyword_set(sigcut) THEN sigcut = 3

  min = min(x)
  max = max(x)
  IF keyword_set(xrange) THEN BEGIN
    min = xrange[0]
    max = xrange[1]
  ENDIF 
  step = xbin
  IF keyword_set(binsize) THEN xbin = binsize
  n = fix((max-min)/float(step))+1
  ox = findgen(n)*step+min+0.5*step
  IF keyword_set(bin_values) THEN ox = bin_values
  oy = ox*0
  od = oy
  osig = oy
  ok = intarr(n)+1
  FOR i = 0, n-1 DO BEGIN
    idx = where(x GE ox[i]-xbin AND x LT ox[i]+xbin AND finite(y) EQ 1, ct)
    IF ct GT 2 THEN BEGIN; was 5
      resistant_mean, y[idx], sigcut, m, s, nr
      oy[i] = m
      od[i] = s
      osig[i] = s*sqrt(n_elements(idx)-1-nr)
    ENDIF ELSE ok[i] = 0
  ENDFOR
  i = where(ok EQ 1, ct)
  IF ct GT 0 THEN BEGIN
    ox = ox[i]
    oy = oy[i]
    od = od[i]
    osig = osig[i]
  ENDIF ELSE BEGIN
    ox = 0
    oy = 0
    od = 0
    osig = 0
  ENDELSE
END
