PRO define_addcol, addcol, nband, bd_fit = bd_fit, read_bd = read_bd
  
; version as it should be!
;    hlpcheb0 = 'fltarr('+strtrim(cheb[0]+1,2)+')'
;    hlpcheb1 = 'fltarr('+strtrim(cheb[1]+1,2)+')'
;    hlpcheb2 = 'fltarr('+strtrim(cheb[2]+1,2)+')'
;    hlpcheb3 = 'fltarr('+strtrim(cheb[3]+1,2)+')'
;    hlpcheb4 = 'fltarr('+strtrim(cheb[4]+1,2)+')'
;    hlpcheb5 = 'fltarr('+strtrim(cheb[5]+1,2)+')'
;    hlpcheb6 = 'fltarr('+strtrim(cheb[6]+1,2)+')'
; version as it is
; help arrays. Always store nband components for chebychev parameters
  hlpcheb0 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb1 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb2 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb3 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb4 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb5 = 'fltarr('+strtrim(nband,2)+')'
  hlpcheb6 = 'fltarr('+strtrim(nband,2)+')'
  bandstring = 'strarr('+strtrim(nband,2)+')'
  bandfloat = 'fltarr('+strtrim(nband,2)+')'
  addcol = [['FLAG_GALFIT', '0'], ['NITER_GALFIT', '0'], $
            ['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0.'], $
            ['GALFIT_VERSION', '" "'], $
            ['FIRSTCON_GALFIT','0'], ['LASTCON_GALFIT','0'], $
            ['NDOF_GALFIT','0l'], ['NFREE_GALFIT','0l'], $
            ['NFIX_GALFIT','0l'], $
            ['CHI2NU_GALFIT','0.'], ['INITFILE', '" "'], $
            ['CONSTRNT', '" "'], ['LOGFILE', '" "'], $
            ['FITSECT', '" "'], ['CONVBOX', '" "'], $
            ['CPUTIME_SETUP_GALFIT', '0.'], ['CPUTIME_FIT_GALFIT', '0.'], $
            ['CPUTIME_TOTAL_GALFIT', '0.'], $
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
            ['SKY_GALFIT_BAND', bandfloat], ['SKY_GALA_BAND', bandfloat], $
            ['SKY_SIG_BAND', bandfloat], ['SKY_RAD_BAND', bandfloat], $
            ['SKY_FLAG_BAND', bandfloat], ['SKY_GALFIT_CHEB', bandfloat], $
            ['NGOOD_GALFIT_BAND', bandfloat], ['NMASK_GALFIT_BAND', bandfloat], $
            ['PSF_GALFIT_BAND', bandstring], $
            ['X_GALFIT_DEG','0'], ['Y_GALFIT_DEG','0'], $
            ['MAG_GALFIT_DEG','0'],['RE_GALFIT_DEG','0'], $
            ['N_GALFIT_DEG','0'],['Q_GALFIT_DEG','0'], $
            ['PA_GALFIT_DEG','0'], $
            ['NDOF_GALFIT_PRIME','0l'], ['CHISQ_GALFIT_PRIME','0.'], $
            ['CHI2NU_GALFIT_PRIME','0.']]
;            ['NGOOD_GALFIT_BAND_PRIME', bandfloat], ['NMASK_GALFIT_BAND_PRIME', bandfloat]
  if keyword_set(bd_fit) or keyword_set(read_bd) then addcol=[[addcol], $
                                                              ['FLAG_GALFIT_BD', '0'], ['NITER_GALFIT_BD', '0'], $
                                                              ['NEIGH_GALFIT_BD', '0'], ['CHISQ_GALFIT_BD','0.'], $
                                                              ['GALFIT_VERSION_BD', '" "'], $
                                                              ['FIRSTCON_GALFIT_BD','0'], ['LASTCON_GALFIT_BD','0'], $
                                                              ['NDOF_GALFIT_BD','0l'], ['NFREE_GALFIT_BD','0l'], $
                                                              ['NFIX_GALFIT_BD','0l'], $
                                                              ['CHI2NU_GALFIT_BD','0.'], ['INITFILE_BD', '" "'], $
                                                              ['CONSTRNT_BD', '" "'], ['LOGFILE_BD', '" "'], $
                                                              ['FILE_GALFIT_BD', '" "'], ['CPUTIME_SETUP_GALFIT_BD', '0.'], $
                                                              ['CPUTIME_FIT_GALFIT_BD', '0.'], ['CPUTIME_TOTAL_GALFIT_BD', '0.'], $
                                                              ['X_GALFIT_D', '0.'], ['XERR_GALFIT_D', '0.'], $
                                                              ['X_GALFIT_B', '0.'], ['XERR_GALFIT_B', '0.'], $
                                                              ['X_GALFIT_CHEB_D', hlpcheb0], ['XERR_GALFIT_CHEB_D', hlpcheb0], $
                                                              ['X_GALFIT_CHEB_B', hlpcheb0], ['XERR_GALFIT_CHEB_B', hlpcheb0], $
                                                              ['Y_GALFIT_D', '0.'], ['YERR_GALFIT_D', '0.'], $
                                                              ['Y_GALFIT_B', '0.'], ['YERR_GALFIT_B', '0.'], $
                                                              ['Y_GALFIT_CHEB_D', hlpcheb1], ['YERR_GALFIT_CHEB_D', hlpcheb1], $
                                                              ['Y_GALFIT_CHEB_B', hlpcheb1], ['YERR_GALFIT_CHEB_B', hlpcheb1], $
                                                              ['MAG_GALFIT_D', '0.'], ['MAGERR_GALFIT_D', '0.'], $
                                                              ['MAG_GALFIT_B', '0.'], ['MAGERR_GALFIT_B', '0.'], $
                                                              ['MAG_GALFIT_CHEB_D', hlpcheb2], ['MAGERR_GALFIT_CHEB_D', hlpcheb2], $
                                                              ['MAG_GALFIT_CHEB_B', hlpcheb2], ['MAGERR_GALFIT_CHEB_B', hlpcheb2], $
                                                              ['RE_GALFIT_D', '0.'], ['REERR_GALFIT_D', '0.'], $
                                                              ['RE_GALFIT_B', '0.'], ['REERR_GALFIT_B', '0.'], $
                                                              ['RE_GALFIT_CHEB_D', hlpcheb3], ['REERR_GALFIT_CHEB_D', hlpcheb3], $
                                                              ['RE_GALFIT_CHEB_B', hlpcheb3], ['REERR_GALFIT_CHEB_B', hlpcheb3], $
                                                              ['N_GALFIT_D', '0.'], ['NERR_GALFIT_D', '0.'], $
                                                              ['N_GALFIT_B', '0.'], ['NERR_GALFIT_B', '0.'], $
                                                              ['N_GALFIT_CHEB_D', hlpcheb4], ['NERR_GALFIT_CHEB_D', hlpcheb4], $
                                                              ['N_GALFIT_CHEB_B', hlpcheb4], ['NERR_GALFIT_CHEB_B', hlpcheb4], $
                                                              ['Q_GALFIT_D', '0.'], ['QERR_GALFIT_D', '0.'], $
                                                              ['Q_GALFIT_B', '0.'], ['QERR_GALFIT_B', '0.'], $
                                                              ['Q_GALFIT_CHEB_D', hlpcheb5], ['QERR_GALFIT_CHEB_D', hlpcheb5], $
                                                              ['Q_GALFIT_CHEB_B', hlpcheb5], ['QERR_GALFIT_CHEB_B', hlpcheb5], $
                                                              ['PA_GALFIT_D', '0.'], ['PAERR_GALFIT_D', '0.'], $
                                                              ['PA_GALFIT_B', '0.'], ['PAERR_GALFIT_B', '0.'], $
                                                              ['PA_GALFIT_CHEB_D', hlpcheb6], ['PAERR_GALFIT_CHEB_D', hlpcheb6], $
                                                              ['PA_GALFIT_CHEB_B', hlpcheb6], ['PAERR_GALFIT_CHEB_B', hlpcheb6], $
                                                              ['X_GALFIT_BAND_D', bandfloat], ['XERR_GALFIT_BAND_D', bandfloat], $
                                                              ['X_GALFIT_BAND_B', bandfloat], ['XERR_GALFIT_BAND_B', bandfloat], $
                                                              ['Y_GALFIT_BAND_D', bandfloat], ['YERR_GALFIT_BAND_D', bandfloat], $
                                                              ['Y_GALFIT_BAND_B', bandfloat], ['YERR_GALFIT_BAND_B', bandfloat], $
                                                              ['MAG_GALFIT_BAND_D', bandfloat], ['MAGERR_GALFIT_BAND_D', bandfloat], $
                                                              ['MAG_GALFIT_BAND_B', bandfloat], ['MAGERR_GALFIT_BAND_B', bandfloat], $
                                                              ['RE_GALFIT_BAND_D', bandfloat], ['REERR_GALFIT_BAND_D', bandfloat], $
                                                              ['RE_GALFIT_BAND_B', bandfloat], ['REERR_GALFIT_BAND_B', bandfloat], $
                                                              ['N_GALFIT_BAND_D', bandfloat], ['NERR_GALFIT_BAND_D', bandfloat], $
                                                              ['N_GALFIT_BAND_B', bandfloat], ['NERR_GALFIT_BAND_B', bandfloat], $
                                                              ['Q_GALFIT_BAND_D', bandfloat], ['QERR_GALFIT_BAND_D', bandfloat], $
                                                              ['Q_GALFIT_BAND_B', bandfloat], ['QERR_GALFIT_BAND_B', bandfloat], $
                                                              ['PA_GALFIT_BAND_D', bandfloat], ['PAERR_GALFIT_BAND_D', bandfloat], $
                                                              ['PA_GALFIT_BAND_B', bandfloat], ['PAERR_GALFIT_BAND_B', bandfloat], $
                                                              ['SKY_GALFIT_BD', '0.'], ['SKY_GALFIT_BAND_BD', bandfloat], $
                                                              ['SKY_GALFIT_CHEB_BD', bandfloat], ['PSF_GALFIT_BAND_BD', bandstring], $
                                                              ['PSF_GALFIT_BD', ' '], $
                                                              ['X_GALFIT_DEG_B','0'], ['Y_GALFIT_DEG_B','0'], $
                                                              ['MAG_GALFIT_DEG_B','0'],['RE_GALFIT_DEG_B','0'], $
                                                              ['N_GALFIT_DEG_B','0'],['Q_GALFIT_DEG_B','0'], $
                                                              ['PA_GALFIT_DEG_B','0'], $
                                                              ['X_GALFIT_DEG_D','0'], ['Y_GALFIT_DEG_D','0'], $
                                                              ['MAG_GALFIT_DEG_D','0'],['RE_GALFIT_DEG_D','0'], $
                                                              ['N_GALFIT_DEG_D','0'],['Q_GALFIT_DEG_D','0'], $
                                                              ['PA_GALFIT_DEG_D','0'], $
                                                              ['NDOF_GALFIT_BD_PRIME','0l'], ['CHISQ_GALFIT_BD_PRIME','0.'], $
                                                              ['CHI2NU_GALFIT_BD_PRIME','0.']]  

END
