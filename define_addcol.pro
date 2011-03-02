PRO define_addcol, addcol, nband, pre, cheb

;IF nband EQ 1 THEN BEGIN
;    addcol = [['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0'], $
;              ['NDOF_GALFIT','0'], ['NFREE_GALFIT','0'], $
;              ['NFIX_GALFIT','0'], ['CHI2NU_GALFIT','0'], $
;              ['ORG_IMAGE', '" "'], ['FILE_GALFIT', '" "'], $
;              ['X_GALFIT', '0.'], ['XERR_GALFIT', '0.'], $
;              ['Y_GALFIT', '0.'], ['YERR_GALFIT', '0.'], $
;              ['MAG_GALFIT', '0.'], ['MAGERR_GALFIT', '0.'], $
;              ['RE_GALFIT', '0.'], ['REERR_GALFIT', '0.'], $
;              ['N_GALFIT', '0.'], ['NERR_GALFIT', '0.'], $
;              ['Q_GALFIT', '0.'], ['QERR_GALFIT', '0.'], $
;              ['PA_GALFIT', '0.'], ['PAERR_GALFIT', '0.'], $
;              ['SKY_GALFIT', '0.'], ['PSF_GALFIT', '" "']]
;ENDIF
;IF nband GT 1 THEN BEGIN
;;    hlpcheb0 = '['
;;    for q=0,cheb[0] do hlpcheb0 = hlpcheb0+'0.,'
;;    hlpcheb0 = strmid(hlpcheb0,0,strpos(hlpcheb0,',',/reverse_search))+']'
;;    hlpcheb1 = '['
;;    for q=0,cheb[1] do hlpcheb1 = hlpcheb1+'0.,'
;;    hlpcheb1 = strmid(hlpcheb1,0,strpos(hlpcheb1,',',/reverse_search))+']'
;;    hlpcheb2 = '['
;;    for q=0,cheb[2] do hlpcheb2 = hlpcheb2+'0.,'
;;    hlpcheb2 = strmid(hlpcheb2,0,strpos(hlpcheb2,',',/reverse_search))+']'
;;    hlpcheb3 = '['
;;    for q=0,cheb[3] do hlpcheb3 = hlpcheb3+'0.,'
;;    hlpcheb3 = strmid(hlpcheb3,0,strpos(hlpcheb3,',',/reverse_search))+']'
;;    hlpcheb4 = '['
;;    for q=0,cheb[4] do hlpcheb4 = hlpcheb4+'0.,'
;;    hlpcheb4 = strmid(hlpcheb4,0,strpos(hlpcheb4,',',/reverse_search))+']'
;;    hlpcheb5 = '['
;;    for q=0,cheb[5] do hlpcheb5 = hlpcheb5+'0.,'
;;    hlpcheb5 = strmid(hlpcheb5,0,strpos(hlpcheb5,',',/reverse_search))+']'
;;    hlpcheb6 = '['
;;    for q=0,cheb[6] do hlpcheb6 = hlpcheb6+'0.,'
;;    hlpcheb6 = strmid(hlpcheb6,0,strpos(hlpcheb6,',',/reverse_search))+']'
;;    bandstring = '['
;;    for q=1,nband do bandstring = bandstring+'" ",'
;;    bandstring = strmid(bandstring,0,strpos(bandstring,',',/reverse_search))+']'
;;    bandfloat = '['
;;    for q=1,nband do bandfloat = bandfloat+'0.,'
;;    bandfloat = strmid(bandfloat,0,strpos(bandfloat,',',/reverse_search))+']'

; version as it should be!
;    hlpcheb0 = 'fltarr('+strtrim(cheb[0]+1,2)+')'
;    hlpcheb1 = 'fltarr('+strtrim(cheb[1]+1,2)+')'
;    hlpcheb2 = 'fltarr('+strtrim(cheb[2]+1,2)+')'
;    hlpcheb3 = 'fltarr('+strtrim(cheb[3]+1,2)+')'
;    hlpcheb4 = 'fltarr('+strtrim(cheb[4]+1,2)+')'
;    hlpcheb5 = 'fltarr('+strtrim(cheb[5]+1,2)+')'
;    hlpcheb6 = 'fltarr('+strtrim(cheb[6]+1,2)+')'
; version as it is
    hlpcheb0 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb1 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb2 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb3 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb4 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb5 = 'fltarr('+strtrim(nband,2)+')'
    hlpcheb6 = 'fltarr('+strtrim(nband,2)+')'
    bandstring = 'strarr('+strtrim(nband,2)+')'
    bandfloat = 'fltarr('+strtrim(nband,2)+')'
    addcol = [['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0'], $
              ['NDOF_GALFIT','0'], ['NFREE_GALFIT','0'], $
              ['NFIX_GALFIT','0'], ['CHI2NU_GALFIT','0'], $
              ['ORG_IMAGE', '" "'], ['FILE_GALFIT', '" "'], $
              ['X_GALFIT', '0.'], ['XERR_GALFIT', '0.'], $
              ['X_GALFIT_CHEB', hlpcheb0], ['XERR_GALFIT_CHEB', hlpcheb0], $
              ['Y_GALFIT', '0.'], ['YERR_GALFIT', '0.'], $
              ['Y_GALFIT_CHEB', hlpcheb1], ['YERR_GALFIT_CHEB', hlpcheb1], $
              ['MAG_GALFIT', '0.'], ['MAGERR_GALFIT', '0.'], $
              ['MAG_GALFIT_CHEB', hlpcheb2], ['MAGERR_GALFIT_CHEB', hlpcheb2], $
              ['RE_GALFIT', '0.'], ['REERR_GALFIT', '0.'], $
              ['RE_GALFIT_CHEB', hlpcheb3], ['REERR_GALFIT_CHEB', hlpcheb3], $
              ['N_GALFIT', '0.'], ['NERR_GALFIT', '0.'], $
              ['N_GALFIT_CHEB', hlpcheb4], ['NERR_GALFIT_CHEB', hlpcheb4], $
              ['Q_GALFIT', '0.'], ['QERR_GALFIT', '0.'], $
              ['Q_GALFIT_CHEB', hlpcheb5], ['QERR_GALFIT_CHEB', hlpcheb5], $
              ['PA_GALFIT', '0.'], ['PAERR_GALFIT', '0.'], $
              ['PA_GALFIT_CHEB', hlpcheb6], ['PAERR_GALFIT_CHEB', hlpcheb6], $
              ['SKY_GALFIT', '0.'], ['PSF_GALFIT', '" "'], $
              ['ORG_IMAGE_BAND', bandstring], $
              ['X_GALFIT_BAND', bandfloat], ['XERR_GALFIT_BAND', bandfloat], $
              ['Y_GALFIT_BAND', bandfloat], ['YERR_GALFIT_BAND', bandfloat], $
              ['MAG_GALFIT_BAND', bandfloat], ['MAGERR_GALFIT_BAND', bandfloat], $
              ['RE_GALFIT_BAND', bandfloat], ['REERR_GALFIT_BAND', bandfloat], $
              ['N_GALFIT_BAND', bandfloat], ['NERR_GALFIT_BAND', bandfloat], $
              ['Q_GALFIT_BAND', bandfloat], ['QERR_GALFIT_BAND', bandfloat], $
              ['PA_GALFIT_BAND', bandfloat], ['PAERR_GALFIT_BAND', bandfloat], $
              ['SKY_GALFIT_BAND', bandfloat], ['PSF_GALFIT_BAND', bandstring]]
;ENDIF
END
