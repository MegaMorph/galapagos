PRO define_addcol, addcol, nband, pre, cheb

IF nband EQ 1 THEN BEGIN
    addcol = [['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0'], $
              ['NDOF_GALFIT','0'], ['NFREE_GALFIT','0'], $
              ['NFIX_GALFIT','0'], ['CHI2NU_GALFIT','0'], $
              ['ORG_IMAGE', '" "'], ['FILE_GALFIT', '" "'], $
              ['X_GALFIT', '0.'], ['XERR_GALFIT', '0.'], $
              ['Y_GALFIT', '0.'], ['YERR_GALFIT', '0.'], $
              ['MAG_GALFIT', '0.'], ['MAGERR_GALFIT', '0.'], $
              ['RE_GALFIT', '0.'], ['REERR_GALFIT', '0.'], $
              ['N_GALFIT', '0.'], ['NERR_GALFIT', '0.'], $
              ['Q_GALFIT', '0.'], ['QERR_GALFIT', '0.'], $
              ['PA_GALFIT', '0.'], ['PAERR_GALFIT', '0.'], $
              ['SKY_GALFIT', '0.'], ['PSF_GALFIT', '" "']]
ENDIF
IF nband GT 1 THEN BEGIN
    addcol = [['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0'], $
              ['NDOF_GALFIT','0'], ['NFREE_GALFIT','0'], $
              ['NFIX_GALFIT','0'], ['CHI2NU_GALFIT','0'], $
              ['ORG_IMAGE', '" "'], ['FILE_GALFIT', '" "'], $
              ['X_GALFIT', '0.'], ['XERR_GALFIT', '0.']]
    IF cheb[0] GE 1 THEN FOR c=1,cheb[0] DO addcol=[[addcol],['X_'+strtrim(c,2)+'_GALFIT', '0.'], ['XERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['Y_GALFIT', '0.'], ['YERR_GALFIT', '0.']]
    IF cheb[1] GE 1 THEN FOR c=1,cheb[1] DO addcol=[[addcol],['Y_'+strtrim(c,2)+'_GALFIT', '0.'], ['YERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['MAG_GALFIT', '0.'], ['MAGERR_GALFIT', '0.']]
    IF cheb[2] GE 1 THEN FOR c=1,cheb[2] DO addcol=[[addcol],['MAG_'+strtrim(c,2)+'_GALFIT', '0.'], ['MAGERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['RE_GALFIT', '0.'], ['REERR_GALFIT', '0.']]
    IF cheb[3] GE 1 THEN FOR c=1,cheb[3] DO addcol=[[addcol],['RE_'+strtrim(c,2)+'_GALFIT', '0.'], ['REERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['N_GALFIT', '0.'], ['NERR_GALFIT', '0.']]
    IF cheb[4] GE 1 THEN FOR c=1,cheb[4] DO addcol=[[addcol],['N_'+strtrim(c,2)+'_GALFIT', '0.'], ['NERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['Q_GALFIT', '0.'], ['QERR_GALFIT', '0.']]
    IF cheb[5] GE 1 THEN FOR c=1,cheb[5] DO addcol=[[addcol],['Q_'+strtrim(c,2)+'_GALFIT', '0.'], ['QERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['PA_GALFIT', '0.'], ['PAERR_GALFIT', '0.']]
    IF cheb[6] GE 1 THEN FOR c=1,cheb[6] DO addcol=[[addcol],['PA_'+strtrim(c,2)+'_GALFIT', '0.'], ['PAERR_'+strtrim(c,2)+'_GALFIT', '0.']]
    addcol = [[addcol],['SKY_GALFIT', '0.'], ['PSF_GALFIT', '" "']]
    FOR n=1,nband DO BEGIN
        addcol = [[addcol], $
                  ['ORG_IMAGE_'+pre[n], '" "'], $
                  ['X_GALFIT_'+pre[n], '0.'], ['XERR_GALFIT_'+pre[n], '0.'], $
                  ['Y_GALFIT_'+pre[n], '0.'], ['YERR_GALFIT_'+pre[n], '0.'], $
                  ['MAG_GALFIT_'+pre[n], '0.'], ['MAGERR_GALFIT_'+pre[n], '0.'], $
                  ['RE_GALFIT_'+pre[n], '0.'], ['REERR_GALFIT_'+pre[n], '0.'], $
                  ['N_GALFIT_'+pre[n], '0.'], ['NERR_GALFIT_'+pre[n], '0.'], $
                  ['Q_GALFIT_'+pre[n], '0.'], ['QERR_GALFIT_'+pre[n], '0.'], $
                  ['PA_GALFIT_'+pre[n], '0.'], ['PAERR_GALFIT_'+pre[n], '0.'], $
                  ['SKY_GALFIT_'+pre[n], '0.'], ['PSF_GALFIT_'+pre[n], '" "']]
    ENDFOR
ENDIF
END
