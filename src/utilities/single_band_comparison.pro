; requires rename_tags.pro and add_tags.pro to run

PRO single_comparison_all, mwl_startfile, galfit=galfit, bd=bd, ps=ps, original_fit=original_fit
; this routine carries out single_band fits, either for nonparametric
; or without. This is the faster routine and should be preferred.

; .run single_band_comparison.pro
; single_comparison_all, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS', galfit='~/megamorph/galfit/exec/galfitm-1.1.12-osx'
; single_comparison_all, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SE', galfit='~/megamorph/galfit/exec/galfitm-1.1.12-osx',/bd
  spawn, 'pwd', infolder
  IF strpos(mwl_startfile,'/') NE -1 THEN BEGIN
     workfolder = strmid(mwl_startfile,0,strpos(mwl_startfile,'/',/REVERSE_SEARCH))
     CD, workfolder
     mwl_startfile = strmid(mwl_startfile,strpos(mwl_startfile,'/',/REVERSE_SEARCH)+1)
  ENDIF

; create all single_band fits and run galfit
  single_band_comparison, mwl_startfile, galfit=galfit

; run mwl fit if it does not exist!
  IF keyword_set(original_fit) THEN BEGIN
     print, 'carrying out the original fit'
     spawn, galfit+' '+mwl_startfile
     spawn, 'rm fit.log'
  ENDIF
  
; create eps figure with results
  plot_single_mwl_comparison, mwl_startfile, bd=bd, /ps

  CD, infolder
END

PRO single_comparison_all_np, mwl_startfile, galfit=galfit, bd=bd, original_fit=original_fit
; Warning, this routine carries out ALL fits, whether they exist or
; not. This will waste some CPU time and will take a while, so make sure this is feasible,
; otherwise just run the bits/bands that you'll need to run!

; .run single_band_comparison.pro
; single_comparison_all_np, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS', galfit='~/megamorph/galfit/exec/galfitm-1.1.12-osx, bd=bd

; create all single_band fits and run galfit
  single_band_comparison, mwl_startfile, galfit=galfit, bd=bd

; creare all NP setups and run fits
  single_band_comparison, mwl_startfile, galfit=galfit, bd=bd, /np

; run original mwl fit
  IF keyword_set(original_fit) THEN BEGIN
     print, 'carrying out the original fit'
     spawn, galfit+' '+mwl_startfile
     spawn, 'rm fit.log'
  ENDIF

; create eps figure with results
  plot_single_mwl_comparison, mwl_startfile, bd=bd, /np, /ps

END

PRO single_band_comparison, mwl_startfile, band=band, galfit=galfit, np=np
; .run single_band_comparison.pro
; single_band_comparison, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS', band='u', galfit='~/megamorph/galfit/exec/galfitm-1.1.12-osx'
  spawn, 'pwd', infolder

  IF strpos(mwl_startfile,'/') NE -1 THEN BEGIN
     workfolder = strmid(mwl_startfile,0,strpos(mwl_startfile,'/',/REVERSE_SEARCH))
     CD, workfolder
     mwl_startfile = strmid(mwl_startfile,strpos(mwl_startfile,'/',/REVERSE_SEARCH)+1)
  ENDIF

; count number of bands first and get band names
  openr, filer, mwl_startfile, /get_lun
  line = ''
  REPEAT BEGIN
     readf, filer, line
  ENDREP UNTIL strpos(strtrim(line, 2), 'A) ') EQ 0
; jump to line A1)
  readf, filer, line
  content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
  bandnames = strsplit(content,',',/extract)
  nband = n_elements(bandnames)
  print, 'determined '+strtrim(nband,2)+' bands in the input file'
  close, filer
  
  IF keyword_set(band) THEN band_todo=band ELSE band_todo = bandnames

  FOR i=0,n_elements(band_todo)-1 DO BEGIN
     band_ind = where(bandnames EQ band_todo[i])
     newfile = mwl_startfile+'_'+bandnames[band_ind]
     IF keyword_set(np) THEN newfile = newfile+'_np'
     print, 'setting up band '+strtrim(band_todo[i],2)+' (band '+strtrim(band_ind,2)+')'
     create_single_band_setup, mwl_startfile, newfile, band_ind, strtrim(band_todo[i],2), np=np
     IF keyword_set(galfit) THEN BEGIN
        print, 'carrying out the fit'
        spawn, galfit+' '+newfile
        spawn, 'rm fit.log'
     ENDIF
  ENDFOR

; do nonparametric mwl fit, too
  IF keyword_set(np) AND NOT keyword_set(band) THEN BEGIN
     newfile = mwl_startfile+'_np'
     print, 'setting up band multi-band non-parametric fit'
     create_nonparam_mwl_setup, mwl_startfile, newfile
     IF keyword_set(galfit) THEN BEGIN
        print, 'carrying out the fit'
        spawn, galfit+' '+newfile
        spawn, 'rm fit.log'
     ENDIF
  ENDIF

  CD, infolder
END

PRO create_single_band_setup, mwl_startfile, newfile, band_index, bandname, np=np
; .run single_band_comparison.pro
; create_single_band_setup, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS', 0, ['u','g','r','i','z']
  
; go through setup file one line at a time and, where needed, cut down
; to one band value
  openr, filer, mwl_startfile, /get_lun
  line = ''

  openw, filew, newfile, /get_lun

  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
     readf, filer, line
; split up into parts
     content_numbers = ' '
     content_descriptor = ' '
     start = strtrim(strmid(line,0,strpos(line,')')+2),2)
     content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
     comment = strtrim(strmid(line,strpos(line,'#')),2)

; if line is normal setup line
     IF strpos(content,',') EQ -1 AND strtrim(line,2) NE '# INITIAL FITTING PARAMETERS' THEN BEGIN
; find output file name and change it
        IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
           post = ''
           IF keyword_set(np) THEN post = '_np'
           outfile_name = strmid(content,0,strpos(content,'.fits'))+'_'+bandname+post+'.fits'
           printf, filew, start+' '+outfile_name+'            '+comment
        ENDIF ELSE printf, filew, line
;        print, 'normal setup line'

     ENDIF
; if line is mwl setup line
     IF strpos(content,',') NE -1 AND strpos(content,'band') EQ -1  THEN BEGIN
;        print, line
        content_elements = strsplit(content,',',/extract)
        printf, filew, start+' '+content_elements[band_index]+'            '+comment
;        print, 'mwl setup line'
     ENDIF

; if line is mwl parameter line
     IF strpos(content,',') NE -1 AND strpos(content,'band') NE -1  THEN BEGIN
;        print, line
        content_numbers = strtrim(strmid(content,0,strpos(content,' ')),2)
        content_elements = strsplit(content_numbers,',',/extract)
        content_desc = strtrim(strmid(content,strpos(content,' ')),2)
        content_desc_fit = strtrim(fix(strmid(content_desc,0, strpos(content_desc,' ')))<1,2)
        printf, filew, start+' '+content_elements[band_index]+'     '+content_desc_fit+' band      '+comment
;        print, 'parameter line'
     ENDIF

; if line is commented line after initiation block
     IF strtrim(line,2) EQ '# INITIAL FITTING PARAMETERS' THEN BEGIN
        IF keyword_set(np) THEN printf, filew, 'U) 1                                         # Turn on nonparam with default options'
        printf, filew, line
     ENDIF        

;print, ' '
  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew
END

PRO create_nonparam_mwl_setup, mwl_startfile, newfile
; .run single_band_comparison.pro
; create_nonparam_mwl_setup, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS', '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS_np'
  openr, filer, mwl_startfile, /get_lun
  line = ''

  openw, filew, newfile, /get_lun

  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
     readf, filer, line
; split up into parts
     content_numbers = ' '
     content_descriptor = ' '
     start = strtrim(strmid(line,0,strpos(line,')')+2),2)
     content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
     comment = strtrim(strmid(line,strpos(line,'#')),2)
     
; if line is normal setup line
     IF strtrim(line,2) EQ '# INITIAL FITTING PARAMETERS' THEN BEGIN
        printf, filew, 'U) 1                                         # Turn on nonparam with default options'
     ENDIF ELSE BEGIN
        IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
           post = '_np'
           outfile_name = strmid(content,0,strpos(content,'.fits'))+post+'.fits'
           printf, filew, start+' '+outfile_name+'            '+comment
        ENDIF ELSE printf, filew, line
        
     ENDELSE
     
;print, ' '
  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew
END


PRO plot_single_mwl_comparison, mwl_startfile, bd=bd, np=np, ps=ps
; /np ADDS values from non-parametric fit
; /bd indicates Bulge/Disk fit
; .run single_band_comparison.pro
; plot_single_mwl_comparison, '/Users/haeussler/Documents/Presentations/MegaMorph_Demo_2/startfile_mwl_SS'

; count number of bands first and get band names
  openr, filer, mwl_startfile, /get_lun
  line = ''
  REPEAT BEGIN
     readf, filer, line
  ENDREP UNTIL strpos(strtrim(line, 2), 'A) ') EQ 0
; jump to line A1)
  readf, filer, line
  content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
  bandnames = strsplit(content,',',/extract)
  nband = n_elements(bandnames)
; jump to line A2)
  readf, filer, line
  content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
  wavelengths = strsplit(content,',',/extract)
  print, 'determined '+strtrim(nband,2)+' bands in the input file at wavelengths '+strtrim(content,2)
; get initial filename
  readf, filer, line
  content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
  init_filename = strmid(content,0,strpos(content,'.fits')+5)
  close, filer
  FREE_LUN, filer
  
; create structure for results
  res = create_struct('wavelength', float(wavelengths), $
                      'x', fltarr(nband)-999., 'xerr', fltarr(nband)-999., $
                      'y', fltarr(nband)-999., 'yerr', fltarr(nband)-999., $
                      'mag', fltarr(nband)-999., 'magerr', fltarr(nband)-999., $
                      're', fltarr(nband)-999., 'reerr', fltarr(nband)-999., $
                      'n', fltarr(nband)-999., 'nerr' , fltarr(nband)-999., $
                      'q', fltarr(nband)-999., 'qerr', fltarr(nband)-999., $
                      'pa', fltarr(nband)-999., 'paerr', fltarr(nband)-999., $
                      'sky', fltarr(nband)-999., $
                      'x_single', fltarr(nband)-999., 'xerr_single', fltarr(nband)-999., $
                      'y_single', fltarr(nband)-999., 'yerr_single', fltarr(nband)-999., $
                      'mag_single', fltarr(nband)-999., 'magerr_single', fltarr(nband)-999., $
                      're_single', fltarr(nband)-999., 'reerr_single', fltarr(nband)-999., $
                      'n_single', fltarr(nband)-999., 'nerr_single' , fltarr(nband)-999., $
                      'q_single', fltarr(nband)-999., 'qerr_single', fltarr(nband)-999., $
                      'pa_single', fltarr(nband)-999., 'paerr_single', fltarr(nband)-999., $
                      'sky_single', fltarr(nband)-999.)
  IF keyword_set(np) THEN BEGIN
     tags = ['x_np', 'xerr_np', 'y_np', 'yerr_np', 'mag_np', $
             'magerr_np', 're_np', 'reerr_np', 'n_np', 'nerr_np', $
             'q_np', 'qerr_np', 'pa_np', 'paerr_np', $
             'x_single_np', 'xerr_single_np', 'y_single_np', 'yerr_single_np', 'mag_single_np', $
             'magerr_single_np', 're_single_np', 'reerr_single_np', 'n_single_np', 'nerr_single_np', $
             'q_single_np', 'qerr_single_np', 'pa_single_np', 'paerr_single_np']
     values = ['fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.']
     add_tags, res, tags, values, res2
     res = res2
     delvarx, res2
  ENDIF

  IF keyword_set(bd) THEN BEGIN
     tags = ['x_d', 'xerr_d', 'y_d', 'yerr_d', 'mag_d', $
             'magerr_d', 're_d', 'reerr_d', 'n_d', 'nerr_d', $
             'q_d', 'qerr_d', 'pa_d', 'paerr_d', $
             'x_d_single', 'xerr_d_single', 'y_d_single', 'yerr_d_single', 'mag_d_single', $
             'magerr_d_single', 're_d_single', 'reerr_d_single', 'n_d_single', 'nerr_d_single', $
             'q_d_single', 'qerr_d_single', 'pa_d_single', 'paerr_d_single']
     values = ['fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
               'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.']
     add_tags, res, tags, values, res2
     res = res2
     delvarx, res2
     IF keyword_set(np) THEN BEGIN
        tags = ['x_d_np', 'xerr_d_np', 'y_d_np', 'yerr_d_np', 'mag_d_np', $
                'magerr_d_np', 're_d_np', 'reerr_d_np', 'n_d_np', 'nerr_d_np', $
                'q_d_np', 'qerr_d_np', 'pa_d_np', 'paerr_d_np', $
                'x_d_single_np', 'xerr_d_single_np', 'y_d_single_np', 'yerr_d_single_np', 'mag_d_single_np', $
                'magerr_d_single_np', 're_d_single_np', 'reerr_d_single_np', 'n_d_single_np', 'nerr_d_single_np', $
                'q_d_single_np', 'qerr_d_single_np', 'pa_d_single_np', 'paerr_d_single_np']
        values = ['fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
                  'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
                  'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
                  'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
                  'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', $
                  'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.', 'fltarr('+strtrim(nband,2)+')-999.']
        add_tags, res, tags, values, res2
        res = res2
        delvarx, res2
  ENDIF
  ENDIF
 

; read mwl fits and put in structure
  mwl_res = mrdfits(init_filename,'FINAL_BAND', /silent)
  res.x   = mwl_res.comp2_xc  & res.xerr   = mwl_res.comp2_xc_err
  res.y   = mwl_res.comp2_yc  & res.yerr   = mwl_res.comp2_yc_err
  res.mag = mwl_res.comp2_mag & res.magerr = mwl_res.comp2_mag_err
  res.re  = mwl_res.comp2_re  & res.reerr  = mwl_res.comp2_re_err
  res.n   = mwl_res.comp2_n   & res.nerr   = mwl_res.comp2_n_err
  res.q   = mwl_res.comp2_ar  & res.qerr   = mwl_res.comp2_ar_err
  res.pa  = mwl_res.comp2_pa  & res.paerr  = mwl_res.comp2_pa_err
  res.sky = mwl_res.comp1_sky
  
  IF keyword_set(np) THEN BEGIN
     mwl_res_np = mrdfits(strmid(init_filename,0,strpos(init_filename,'.fits'))+'_np.fits','FINAL_BAND', /silent)
     res.x_np   = mwl_res_np.comp2_xc  & res.xerr_np   = mwl_res_np.comp2_xc_err
     res.y_np   = mwl_res_np.comp2_yc  & res.yerr_np   = mwl_res_np.comp2_yc_err
     res.mag_np = mwl_res_np.comp2_mag & res.magerr_np = mwl_res_np.comp2_mag_err
     res.re_np  = mwl_res_np.comp2_re  & res.reerr_np  = mwl_res_np.comp2_re_err
     res.n_np   = mwl_res_np.comp2_n   & res.nerr_np   = mwl_res_np.comp2_n_err
     res.q_np   = mwl_res_np.comp2_ar  & res.qerr_np   = mwl_res_np.comp2_ar_err
     res.pa_np  = mwl_res_np.comp2_pa  & res.paerr_np  = mwl_res_np.comp2_pa_err
  ENDIF

  IF keyword_set(bd) THEN BEGIN
     res.x_d   = mwl_res.comp3_xc  & res.xerr_d   = mwl_res.comp3_xc_err
     res.y_d   = mwl_res.comp3_yc  & res.yerr_d   = mwl_res.comp3_yc_err
     res.mag_d = mwl_res.comp3_mag & res.magerr_d = mwl_res.comp3_mag_err
     res.re_d  = mwl_res.comp3_re  & res.reerr_d  = mwl_res.comp3_re_err
     res.n_d   = mwl_res.comp3_n   & res.nerr_d   = mwl_res.comp3_n_err
     res.q_d   = mwl_res.comp3_ar  & res.qerr_d   = mwl_res.comp3_ar_err
     res.pa_d  = mwl_res.comp3_pa  & res.paerr_d  = mwl_res.comp3_pa_err
     IF keyword_set(np) THEN BEGIN
        res.x_d_np   = mwl_res_np.comp3_xc  & res.xerr_d_np   = mwl_res_np.comp3_xc_err
        res.y_d_np   = mwl_res_np.comp3_yc  & res.yerr_d_np   = mwl_res_np.comp3_yc_err
        res.mag_d_np = mwl_res_np.comp3_mag & res.magerr_d_np = mwl_res_np.comp3_mag_err
        res.re_d_np  = mwl_res_np.comp3_re  & res.reerr_d_np  = mwl_res_np.comp3_re_err
        res.n_d_np   = mwl_res_np.comp3_n   & res.nerr_d_np   = mwl_res_np.comp3_n_err
        res.q_d_np   = mwl_res_np.comp3_ar  & res.qerr_d_np   = mwl_res_np.comp3_ar_err
        res.pa_d_np  = mwl_res_np.comp3_pa  & res.paerr_d_np  = mwl_res_np.comp3_pa_err
     ENDIF
  ENDIF

; read all single_band fits and put in structure
  FOR i=0,nband-1 DO BEGIN
     IF file_test(strmid(init_filename,0,strpos(init_filename,'.fits'))+'_'+bandnames[i]+'.fits') THEN BEGIN
        single_res = mrdfits(strmid(init_filename,0,strpos(init_filename,'.fits'))+'_'+bandnames[i]+'.fits','FINAL_BAND', /silent)
        res.x_single[i]   = single_res.comp2_xc  & res.xerr_single[i]   = single_res.comp2_xc_err
        res.y_single[i]   = single_res.comp2_yc  & res.yerr_single[i]   = single_res.comp2_yc_err
        res.mag_single[i] = single_res.comp2_mag & res.magerr_single[i] = single_res.comp2_mag_err
        res.re_single[i]  = single_res.comp2_re  & res.reerr_single[i]  = single_res.comp2_re_err
        res.n_single[i]   = single_res.comp2_n   & res.nerr_single[i]   = single_res.comp2_n_err
        res.q_single[i]   = single_res.comp2_ar  & res.qerr_single[i]   = single_res.comp2_ar_err
        res.pa_single[i]  = single_res.comp2_pa  & res.paerr_single[i]  = single_res.comp2_pa_err
        res.sky_single[i] = single_res.comp1_sky
        
        IF keyword_set(np) THEN BEGIN
           single_res_np = mrdfits(strmid(init_filename,0,strpos(init_filename,'.fits'))+'_'+bandnames[i]+'_np.fits','FINAL_BAND', /silent)
           res.x_single_np[i]   = single_res_np.comp2_xc  & res.xerr_single_np[i]   = single_res_np.comp2_xc_err
           res.y_single_np[i]   = single_res_np.comp2_yc  & res.yerr_single_np[i]   = single_res_np.comp2_yc_err
           res.mag_single_np[i] = single_res_np.comp2_mag & res.magerr_single_np[i] = single_res_np.comp2_mag_err
           res.re_single_np[i]  = single_res_np.comp2_re  & res.reerr_single_np[i]  = single_res_np.comp2_re_err
           res.n_single_np[i]   = single_res_np.comp2_n   & res.nerr_single_np[i]   = single_res_np.comp2_n_err
           res.q_single_np[i]   = single_res_np.comp2_ar  & res.qerr_single_np[i]   = single_res_np.comp2_ar_err
           res.pa_single_np[i]  = single_res_np.comp2_pa  & res.paerr_single_np[i]  = single_res_np.comp2_pa_err
        ENDIF
        
        IF keyword_set(bd) THEN BEGIN
           res.x_d_single[i]   = single_res.comp3_xc  & res.xerr_d_single[i]   = single_res.comp3_xc_err
           res.y_d_single[i]   = single_res.comp3_yc  & res.yerr_d_single[i]   = single_res.comp3_yc_err
           res.mag_d_single[i] = single_res.comp3_mag & res.magerr_d_single[i] = single_res.comp3_mag_err
           res.re_d_single[i]  = single_res.comp3_re  & res.reerr_d_single[i]  = single_res.comp3_re_err
           res.n_d_single[i]   = single_res.comp3_n   & res.nerr_d_single[i]   = single_res.comp3_n_err
           res.q_d_single[i]   = single_res.comp3_ar  & res.qerr_d_single[i]   = single_res.comp3_ar_err
           res.pa_d_single[i]  = single_res.comp3_pa  & res.paerr_d_single[i]  = single_res.comp3_pa_err
           IF keyword_set(np) THEN BEGIN
              res.x_d_single_np[i]   = single_res.comp3_xc  & res.xerr_d_single_np[i]   = single_res.comp3_xc_err
              res.y_d_single_np[i]   = single_res.comp3_yc  & res.yerr_d_single_np[i]   = single_res.comp3_yc_err
              res.mag_d_single_np[i] = single_res.comp3_mag & res.magerr_d_single_np[i] = single_res.comp3_mag_err
              res.re_d_single_np[i]  = single_res.comp3_re  & res.reerr_d_single_np[i]  = single_res.comp3_re_err
              res.n_d_single_np[i]   = single_res.comp3_n   & res.nerr_d_single_np[i]   = single_res.comp3_n_err
              res.q_d_single_np[i]   = single_res.comp3_ar  & res.qerr_d_single_np[i]   = single_res.comp3_ar_err
              res.pa_d_single_np[i]  = single_res.comp3_pa  & res.paerr_d_single_np[i]  = single_res.comp3_pa_err
           ENDIF
        ENDIF
     ENDIF
  ENDFOR 

; SS plots
  IF NOT keyword_set(bd) THEN BEGIN
     tablename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_SS_comparison_table.fits'
     IF keyword_set(np) THEN tablename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_SS_comparison_table_np.fits'
     mwrfits, res, tablename, /silent, /create

     IF keyword_set(ps) THEN BEGIN
        SET_PLOT, 'PS'
        filename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_SS_comparison.eps'
        IF keyword_set(np) THEN filename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_SS_comparison_np.eps'
        device, file = filename, /encapsulated, bits_per_pixel = 4, /color, xsize = 15., ysize = 30.
     ENDIF
     erase
     !y.margin=[5.0,1.0]
     !x.margin=[7.0,1.0]
     multiplot, [1,4], /init
     multiplot
     wlrange = [min(res.wavelength)-0.1*(max(res.wavelength)-min(res.wavelength)),max(res.wavelength)+0.1*(max(res.wavelength)-min(res.wavelength))]
     
     factm = 2
     fact = 2
     sok = where(res.mag_single ne -999.)
;     magrange = [res.mag-factm*res.magerr,res.mag+factm*res.magerr,res.mag_single-factm*res.magerr_single,res.mag_single+factm*res.magerr_single]
     magrange = [min(res.mag)-2, max(res.mag)+2, min(res.mag_single[sok])-2, max(res.mag_single[sok])+2]
;     rerange = [res.re-fact*res.reerr,res.re+fact*res.reerr,res.re_single-fact*res.reerr_single,res.re_single+fact*res.reerr_single]
     rerange = [min(res.re)*0.95, max(res.re)*1.05, min(res.re_single[sok])*0.95, max(res.re_single[sok])*1.05]
;     nrange = [res.n-fact*res.nerr,res.n+fact*res.nerr,res.n_single-fact*res.nerr_single,res.n_single+fact*res.nerr_single]
     nrange = [min(res.n)*0.8, max(res.n)*1.2, min(res.n_single[sok])*0.8, max(res.n_single[sok])*1.2]
;     qrange = [res.q-fact*res.qerr,res.q+fact*res.qerr,res.q_single-fact*res.qerr_single,res.q_single+fact*res.qerr_single]
     qrange = [min(res.q)*0.95, max(res.q)*1.05<1., min(res.q_single[sok])*0.95, max(res.q_single[sok])*1.05<1.]
;     parange = [res.pa-fact*res.paerr,res.pa+fact*res.paerr,res.pa_single-fact*res.paerr_single,res.pa_single+fact*res.paerr_single]
     parange = [min(res.pa)*0.95, max(res.pa)*1.05, min(res.pa_single[sok])*0.95, max(res.pa_single[sok])*1.05]
     IF keyword_set(np) THEN BEGIN
;        magrange = [res.mag-factm*res.magerr,res.mag+factm*res.magerr,res.mag_single-factm*res.magerr_single,res.mag_single+factm*res.magerr_single,res.mag_np-factm*res.magerr_np,res.mag_np+factm*res.magerr_np,res.mag_single_np-factm*res.magerr_single_np,res.mag_single_np+factm*res.magerr_single_np]
        magrange = [res.mag-factm*res.magerr,res.mag+factm*res.magerr,res.mag_single-factm*res.magerr_single,res.mag_single+factm*res.magerr_single,res.mag_np-factm*res.magerr_np,res.mag_np+factm*res.magerr_np,res.mag_single_np-factm*res.magerr_single_np,res.mag_single_np+factm*res.magerr_single_np]
;        rerange = [res.re-fact*res.reerr,res.re+fact*res.reerr,res.re_single-fact*res.reerr_single,res.re_single+fact*res.reerr_single,res.re_np-fact*res.reerr_np,res.re_np+fact*res.reerr_np,res.re_single_np-fact*res.reerr_single_np,res.re_single_np+fact*res.reerr_single_np]
        rerange = [res.re-fact*res.reerr,res.re+fact*res.reerr,res.re_single-fact*res.reerr_single,res.re_single+fact*res.reerr_single,res.re_np-fact*res.reerr_np,res.re_np+fact*res.reerr_np,res.re_single_np-fact*res.reerr_single_np,res.re_single_np+fact*res.reerr_single_np]
;        nrange = [res.n-fact*res.nerr,res.n+fact*res.nerr,res.n_single-fact*res.nerr_single,res.n_single+fact*res.nerr_single,res.n_np-fact*res.nerr_np,res.n+fact*res.nerr_np,res.n_single_np-fact*res.nerr_single_np,res.n_single_np+fact*res.nerr_single_np]
        nrange = [res.n-fact*res.nerr,res.n+fact*res.nerr,res.n_single-fact*res.nerr_single,res.n_single+fact*res.nerr_single,res.n_np-fact*res.nerr_np,res.n+fact*res.nerr_np,res.n_single_np-fact*res.nerr_single_np,res.n_single_np+fact*res.nerr_single_np]
;        qrange = [res.q-fact*res.qerr,res.q+fact*res.qerr,res.q_single-fact*res.qerr_single,res.q_single+fact*res.qerr_single,res.q_np-fact*res.qerr_np,res.q+fact*res.qerr_np,res.q_single_np-fact*res.qerr_single_np,res.q_single_np+fact*res.qerr_single_np]
        qrange = [res.q-fact*res.qerr,res.q+fact*res.qerr,res.q_single-fact*res.qerr_single,res.q_single+fact*res.qerr_single,res.q_np-fact*res.qerr_np,res.q+fact*res.qerr_np,res.q_single_np-fact*res.qerr_single_np,res.q_single_np+fact*res.qerr_single_np]
;        parange = [res.pa-fact*res.paerr,res.pa+fact*res.paerr,res.pa_single-fact*res.paerr_single,res.pa_single+fact*res.paerr_single,res.pa_np-fact*res.paerr_np,res.pa_np+fact*res.paerr_np,res.pa_single_np-fact*res.paerr_single_np,res.pa_single_np+fact*res.paerr_single_np]
        parange = [res.pa-fact*res.paerr,res.pa+fact*res.paerr,res.pa_single-fact*res.paerr_single,res.pa_single+fact*res.paerr_single,res.pa_np-fact*res.paerr_np,res.pa_np+fact*res.paerr_np,res.pa_single_np-fact*res.paerr_single_np,res.pa_single_np+fact*res.paerr_single_np]
     ENDIF

     plot, res.wavelength[sok], res.mag_single[sok], psym=1,/nodata, yrange=[max(magrange),min(magrange)],xrange=wlrange,xstyle=1
;     oplot, res.wavelength[sok], res.mag_single[sok], col=135, linestyle=2
     oplot, res.wavelength[sok], res.mag_single[sok], psym=4, col=135
     errplot, res.wavelength[sok], res.mag_single[sok]-factm*res.magerr_single[sok], res.mag_single[sok]+factm*res.magerr_single[sok], col=135
;     oplot, res.wavelength, res.mag, col=235
     oplot, res.wavelength, res.mag, psym=1, col=235
     errplot, res.wavelength, res.mag-factm*res.magerr, res.mag+factm*res.magerr, col=235
     legend, ['single band +-'+strtrim(factm,2)+'sigma','multi band +-'+strtrim(factm,2)+'sigma'], textcolors=[135,235], charsize=1., /bottom, /right
     IF keyword_set(np) THEN BEGIN
;        oplot, res.wavelength[sok], res.mag_single_np[sok], col=135, linestyle=1
        oplot, res.wavelength[sok], res.mag_single_np[sok], psym=4, col=135
        errplot, res.wavelength[sok], res.mag_single_np[sok]-factm*res.magerr_single_np[sok], res.mag_single_np[sok]+factm*res.magerr_single_np[sok], col=135
;        oplot, res.wavelength, res.mag_np, col=235, linestyle=1
        oplot, res.wavelength, res.mag_np, psym=1, col=235
        errplot, res.wavelength, res.mag_np-factm*res.magerr_np, res.mag_np+factm*res.magerr_np, col=235
     ENDIF
     multiplot

     plot, res.wavelength[sok], res.re_single[sok], psym=1,/nodata, yrange=[min(rerange),max(rerange)],xrange=wlrange,xstyle=1,/ylog
;     oplot, res.wavelength[sok], res.re_single[sok], col=135, linestyle=2
     oplot, res.wavelength[sok], res.re_single[sok], psym=4, col=135
     errplot, res.wavelength[sok], res.re_single[sok]-fact*res.reerr_single[sok], res.re_single[sok]+fact*res.reerr_single[sok], col=135
;     oplot, res.wavelength, res.re, col=235
     oplot, res.wavelength, res.re, psym=1, col=235
     errplot, res.wavelength, res.re-fact*res.reerr, res.re+fact*res.reerr, col=235
     legend, ['single band +-'+strtrim(fact,2)+'sigma','multi band +-'+strtrim(fact,2)+'sigma'], textcolors=[135,235], charsize=1., /bottom, /right
     IF keyword_set(np) THEN BEGIN
;        oplot, res.wavelength, res.re_single_np, col=135, linestyle=1
        oplot, res.wavelength, res.re_single_np, psym=4, col=135
        errplot, res.wavelength, res.re_single_np-fact*res.reerr_single_np, res.re_single_np+fact*res.reerr_single_np, col=135
;        oplot, res.wavelength, res.re_np, col=235, linestyle=1
        oplot, res.wavelength, res.re_np, psym=1, col=235
        errplot, res.wavelength, res.re_np-fact*res.reerr_np, res.re_np+fact*res.reerr_np, col=235
     ENDIF
     multiplot
     plot, res.wavelength[sok], res.n_single[sok], psym=1,/nodata, yrange=[min(nrange),max(nrange)],xrange=wlrange,xstyle=1
;     oplot, res.wavelength[sok], res.n_single[sok], col=135, linestyle=2
     oplot, res.wavelength[sok], res.n_single[sok], psym=4, col=135
     errplot, res.wavelength[sok], res.n_single[sok]-fact*res.nerr_single[sok], res.n_single[sok]+fact*res.nerr_single[sok], col=135
;     oplot, res.wavelength, res.n, col=235
     oplot, res.wavelength, res.n, psym=1, col=235
     errplot, res.wavelength, res.n-fact*res.nerr, res.n+fact*res.nerr, col=235
     IF keyword_set(np) THEN BEGIN
;        oplot, res.wavelength, res.n_single_np, col=135, linestyle=1
        oplot, res.wavelength, res.n_single_np, psym=4, col=135
        errplot, res.wavelength, res.n_single_np-fact*res.nerr_single_np, res.n_single_np+fact*res.nerr_single_np, col=135
;        oplot, res.wavelength, res.n_np, col=235, linestyle=1
        oplot, res.wavelength, res.n_np, psym=1, col=235
        errplot, res.wavelength, res.n_np-fact*res.nerr_np, res.n_np+fact*res.nerr_np, col=235
        legend, ['with nonparametric'], psym=0, linestyle=1, charsize=1., /bottom, /right
     ENDIF
     multiplot
 
     plot, res.wavelength[sok], res.q_single[sok], psym=1,/nodata, yrange=[min(qrange),max(qrange)],xrange=wlrange,xstyle=1
;     oplot, res.wavelength[sok], res.q_single[sok], col=135, linestyle=2
     oplot, res.wavelength[sok], res.q_single[sok], psym=4, col=135
     errplot, res.wavelength[sok], res.q_single[sok]-fact*res.qerr_single[sok], res.q_single[sok]+fact*res.qerr_single[sok], col=135
;     oplot, res.wavelength, res.q, col=235
     oplot, res.wavelength, res.q, psym=1, col=235
     errplot, res.wavelength, res.q-fact*res.qerr, res.q+fact*res.qerr, col=235
     IF keyword_set(np) THEN BEGIN
;        oplot, res.wavelength, res.q_single_np, col=135, linestyle=1
        oplot, res.wavelength, res.q_single_np, psym=4, col=135
        errplot, res.wavelength, res.q_single_np-fact*res.qerr_single_np, res.q_single_np+fact*res.qerr_single_np, col=135
;        oplot, res.wavelength, res.q_np, col=235, linestyle=1
        oplot, res.wavelength, res.q_np, psym=1, col=235
        errplot, res.wavelength, res.q_np-fact*res.qerr_np, res.q_np+fact*res.qerr_np, col=235
     ENDIF

     xyouts, 0.03, 0.86, 'mag', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.64, 're', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.42, 'n', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.20, 'q', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.43, 0.02, 'wavelength', normal = 1,/data, charsize=1.3

     multiplot, /reset

     IF keyword_set(ps) THEN BEGIN
        device,/close
        SET_PLOT, 'X'
     ENDIF
  ENDIF ELSE BEGIN
     
; B/D plots
; write out table
     res2 = rename_tags(res, ['x','xerr','y','yerr','mag','magerr','re','reerr','n','nerr','q','qerr','pa','paerr','x_single','xerr_single','y_single','yerr_single','mag_single','magerr_single','re_single','reerr_single','n_single','nerr_single','q_single','qerr_single','pa_single','paerr_single'], $
                        ['x_b','xerr_b','y_b','yerr_b','mag_b','magerr_b','re_b','reerr_b','n_b','nerr_b','q_b','qerr_b','pa_b','paerr_b','x_b_single','xerr_b_single','y_b_single','yerr_b_single','mag_b_single','magerr_b_single','re_b_single','reerr_b_single','n_b_single','nerr_b_single','q_b_single','qerr_b_single','pa_b_single','paerr_b_single'])
     res = res2
     delvarx, res2
     IF keyword_set(np) THEN BEGIN
        res2 = rename_tags(res, ['x_np','xerr_np','y_np','yerr_np','mag_np','magerr_np','re_np','reerr_np','n_np','nerr_np','q_np','qerr_np','pa_np','paerr_np','x_single_np','xerr_single_np','y_single_np','yerr_single_np','mag_single_np','magerr_single_np','re_single_np','reerr_single_np','n_single_np','nerr_single_np','q_single_np','qerr_single_np','pa_single_np','paerr_single_np'], $
                           ['x_b_np','xerr_b_np','y_b_np','yerr_b_np','mag_b_np','magerr_b_np','re_b_np','reerr_b_np','n_b_np','nerr_b_np','q_b_np','qerr_b_np','pa_b_np','paerr_b_np','x_b_single_np','xerr_b_single_np','y_b_single_np','yerr_b_single_np','mag_b_single_np','magerr_b_single_np','re_b_single_np','reerr_b_single_np','n_b_single_np','nerr_b_single_np','q_b_single_np','qerr_b_single_np','pa_b_single_np','paerr_b_single_np'])
        res = res2
        delvarx, res2
     ENDIF
     tablename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_BD_comparison_table.fits'
     IF keyword_set(np) THEN tablename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_BD_comparison_table_np.fits'
     mwrfits, res, tablename, /silent, /create
     
    IF keyword_set(ps) THEN BEGIN
        SET_PLOT, 'PS'
        filename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_BD_comparison.eps'
        IF keyword_set(np) THEN filename = strmid(init_filename,0,strpos(init_filename,'.fits'))+'_BD_comparison_np.eps'
        device, file = filename, /encapsulated, bits_per_pixel = 4, /color, xsize = 15., ysize = 30.
     ENDIF
     erase
     !y.margin=[5.0,1.0]
     !x.margin=[7.0,1.0]
     multiplot, [1,4], /init
     multiplot

     factm = 20
     fact = 10
     minmag = min([res.mag_b-factm*res.magerr_b-res.mag_b[2],res.mag_b_single-factm*res.magerr_b_single-res.mag_b_single[2],res.mag_d-factm*res.magerr_d-res.mag_d[2],res.mag_d_single-factm*res.magerr_d_single-res.mag_d_single[2]])
     maxmag = max([res.mag_b+factm*res.magerr_b-res.mag_b[2],res.mag_b_single+factm*res.magerr_b_single-res.mag_b_single[2],res.mag_d+factm*res.magerr_d-res.mag_d[2],res.mag_d_single+factm*res.magerr_d_single-res.mag_d_single[2]])
     minre = min([res.re_b-fact*res.reerr_b,res.re_b_single-fact*res.reerr_b_single,res.re_d-fact*res.reerr_d,res.re_d_single-fact*res.reerr_d_single])
     maxre = max([res.re_b+fact*res.reerr_b,res.re_b_single+fact*res.reerr_b_single,res.re_d+fact*res.reerr_d,res.re_d_single+fact*res.reerr_d_single])
     minn = min([res.n_b-fact*res.nerr_b,res.n_b_single-fact*res.nerr_b_single,res.n_d-fact*res.nerr_d,res.n_d_single-fact*res.nerr_d_single])
     maxn = max([res.n_b+fact*res.nerr_b,res.n_b_single+fact*res.nerr_b_single,res.n_d+fact*res.nerr_d,res.n_d_single+fact*res.nerr_d_single])
     minq = min([res.q_b-fact*res.qerr_b,res.q_b_single-fact*res.qerr_b_single,res.q_d-fact*res.qerr_d,res.q_d_single-fact*res.qerr_d_single])
     maxq = max([res.q_b+fact*res.qerr_b,res.q_b_single+fact*res.qerr_b_single,res.q_d+fact*res.qerr_d,res.q_d_single+fact*res.qerr_d_single])
     minpa = min([res.pa_b-fact*res.paerr_b,res.pa_b_single-fact*res.paerr_b_single,res.pa_d-fact*res.paerr_d,res.pa_d_single-fact*res.paerr_d_single])
     maxpa = max([res.pa_b+fact*res.paerr_b,res.pa_b_single+fact*res.paerr_b_single,res.pa_d+fact*res.paerr_d,res.pa_d_single+fact*res.paerr_d_single])

     IF keyword_set(np) THEN BEGIN
        minmag = min([res.mag_b-factm*res.magerr_b-res.mag_b[2],res.mag_b_single-factm*res.magerr_b_single-res.mag_b_single[2],res.mag_d-factm*res.magerr_d-res.mag_d[2],res.mag_d_single-factm*res.magerr_d_single-res.mag_d_single[2],res.mag_b_np-factm*res.magerr_b_np-res.mag_b_np[2],res.mag_b_single_np-factm*res.magerr_b_single_np-res.mag_b_single_np[2],res.mag_d_np-factm*res.magerr_d_np-res.mag_d_np[2],res.mag_d_single_np-factm*res.magerr_d_single_np-res.mag_d_single_np[2]])
        maxmag = max([res.mag_b+factm*res.magerr_b-res.mag_b[2],res.mag_b_single+factm*res.magerr_b_single-res.mag_b_single[2],res.mag_d+factm*res.magerr_d-res.mag_d[2],res.mag_d_single+factm*res.magerr_d_single-res.mag_d_single[2],res.mag_b_np+factm*res.magerr_b_np-res.mag_b_np[2],res.mag_b_single_np+factm*res.magerr_b_single_np-res.mag_b_single_np[2],res.mag_d_np+factm*res.magerr_d_np-res.mag_d_np[2],res.mag_d_single_np+factm*res.magerr_d_single_np-res.mag_d_single_np[2]])
        minre = min([res.re_b-fact*res.reerr_b,res.re_b_single-fact*res.reerr_b_single,res.re_d-fact*res.reerr_d,res.re_d_single-fact*res.reerr_d_single,res.re_b_np-fact*res.reerr_b_np,res.re_b_single_np-fact*res.reerr_b_single_np,res.re_d_np-fact*res.reerr_d_np,res.re_d_single-fact*res.reerr_d_single])
        maxre = max([res.re_b+fact*res.reerr_b,res.re_b_single+fact*res.reerr_b_single,res.re_d+fact*res.reerr_d,res.re_d_single+fact*res.reerr_d_single,res.re_b_np+fact*res.reerr_b_np,res.re_b_single_np+fact*res.reerr_b_single_np,res.re_d_np+fact*res.reerr_d_np,res.re_d_single_np+fact*res.reerr_d_single_np])
        minn = min([res.n_b-fact*res.nerr_b,res.n_b_single-fact*res.nerr_b_single,res.n_d-fact*res.nerr_d,res.n_d_single-fact*res.nerr_d_single,res.n_b_np-fact*res.nerr_b_np,res.n_b_single_np-fact*res.nerr_b_single_np,res.n_d_np-fact*res.nerr_d_np,res.n_d_single_np-fact*res.nerr_d_single_np])
        maxn = max([res.n_b+fact*res.nerr_b,res.n_b_single+fact*res.nerr_b_single,res.n_d+fact*res.nerr_d,res.n_d_single+fact*res.nerr_d_single,res.n_b_np+fact*res.nerr_b_np,res.n_b_single_np+fact*res.nerr_b_single_np,res.n_d_np+fact*res.nerr_d_np,res.n_d_single_np+fact*res.nerr_d_single_np])
        minq = min([res.q_b-fact*res.qerr_b,res.q_b_single-fact*res.qerr_b_single,res.q_d-fact*res.qerr_d,res.q_d_single-fact*res.qerr_d_single,res.q_b_np-fact*res.qerr_b_np,res.q_b_single_np-fact*res.qerr_b_single_np,res.q_d_np-fact*res.qerr_d_np,res.q_d_single_np-fact*res.qerr_d_single_np])
        maxq = max([res.q_b+fact*res.qerr_b,res.q_b_single+fact*res.qerr_b_single,res.q_d+fact*res.qerr_d,res.q_d_single+fact*res.qerr_d_single,res.q_b_np+fact*res.qerr_b_np,res.q_b_single_np+fact*res.qerr_b_single_np,res.q_d_np+fact*res.qerr_d_np,res.q_d_single_np+fact*res.qerr_d_single_np])
        minpa = min([res.pa_b-fact*res.paerr_b,res.pa_b_single-fact*res.paerr_b_single,res.pa_d-fact*res.paerr_d,res.pa_d_single-fact*res.paerr_d_single,res.pa_b_np-fact*res.paerr_b_np,res.pa_b_single_np-fact*res.paerr_b_single_np,res.pa_d_np-fact*res.paerr_d_np,res.pa_d_single_np-fact*res.paerr_d_single_np])
        maxpa = max([res.pa_b+fact*res.paerr_b,res.pa_b_single+fact*res.paerr_b_single,res.pa_d+fact*res.paerr_d,res.pa_d_single+fact*res.paerr_d_single,res.pa_b_np+fact*res.paerr_b_np,res.pa_b_single_np+fact*res.paerr_b_single_np,res.pa_d_np+fact*res.paerr_d_np,res.pa_d_single_np+fact*res.paerr_d_single_np])
     ENDIF

     plot, res.wavelength, res.mag_b_single-res.mag_b_single[2], psym=1,/nodata, yrange=[maxmag,minmag]
     oplot, res.wavelength, res.mag_b_single-res.mag_b_single[2], col=200, linestyle=2
     oplot, res.wavelength, res.mag_b_single-res.mag_b_single[2], psym=1, col=200
     errplot, res.wavelength, res.mag_b_single-factm*res.magerr_b_single-res.mag_b_single[2], res.mag_b_single+factm*res.magerr_b_single-res.mag_b_single[2], col=200
     oplot, res.wavelength, res.mag_d_single-res.mag_d_single[2], col=95, linestyle=2
     oplot, res.wavelength, res.mag_d_single-res.mag_d_single[2], psym=1, col=95
     errplot, res.wavelength, res.mag_d_single-factm*res.magerr_d_single-res.mag_d_single[2], res.mag_d_single+factm*res.magerr_d_single-res.mag_d_single[2], col=95
     oplot, res.wavelength, res.mag_b-res.mag_b[2], col=235
     oplot, res.wavelength, res.mag_b-res.mag_b[2], psym=1, col=235
     errplot, res.wavelength, res.mag_b-factm*res.magerr_b-res.mag_b[2], res.mag_b+factm*res.magerr_b-res.mag_b[2], col=235
     oplot, res.wavelength, res.mag_d-res.mag_d[2], col=135
     oplot, res.wavelength, res.mag_d-res.mag_d[2], psym=1, col=135
     errplot, res.wavelength, res.mag_d-factm*res.magerr_d-res.mag_d[2], res.mag_d+factm*res.magerr_d-res.mag_d[2], col=135
     legend, ['single band bulge +-'+strtrim(factm,2)+'sigma', 'mwl bulge +-'+strtrim(factm,2)+'sigma', 'single band disk +-'+strtrim(factm,2)+'sigma', 'mwl disk +-'+strtrim(factm,2)+'sigma'],textcolors=[200,235,95,135], /bottom, /right, charsize=1.
     IF keyword_set(np) THEN BEGIN
        oplot, res.wavelength, res.mag_b_single_np-res.mag_b_single_np[2], col=200, linestyle=1
        oplot, res.wavelength, res.mag_b_single_np-res.mag_b_single_np[2], psym=1, col=200
        errplot, res.wavelength, res.mag_b_single_np-factm*res.magerr_b_single_np-res.mag_b_single_np[2], res.mag_b_single_np+factm*res.magerr_b_single_np-res.mag_b_single_np[2], col=200
        oplot, res.wavelength, res.mag_d_single_np-res.mag_d_single_np[2], col=95, linestyle=1
        oplot, res.wavelength, res.mag_d_single_np-res.mag_d_single_np[2], psym=1, col=95
        errplot, res.wavelength, res.mag_d_single_np-factm*res.magerr_d_single_np-res.mag_d_single_np[2], res.mag_d_single_np+factm*res.magerr_d_single_np-res.mag_d_single_np[2], col=95
        oplot, res.wavelength, res.mag_b_np-res.mag_b_np[2], col=235, linestyle=1
        oplot, res.wavelength, res.mag_b_np-res.mag_b_np[2], psym=1, col=235
        errplot, res.wavelength, res.mag_b_np-factm*res.magerr_b_np-res.mag_b_np[2], res.mag_b_np+factm*res.magerr_b_np-res.mag_b_np[2], col=235
        oplot, res.wavelength, res.mag_d_np-res.mag_d_np[2], col=135, linestyle=1
        oplot, res.wavelength, res.mag_d_np-res.mag_d_np[2], psym=1, col=135
        errplot, res.wavelength, res.mag_d_np-factm*res.magerr_d_np-res.mag_d_np[2], res.mag_d_np+factm*res.magerr_d_np-res.mag_d_np[2], col=135
     ENDIF
     multiplot

     plot, res.wavelength, res.re_b_single, psym=1,/nodata, yrange=[minre,maxre]
     oplot, res.wavelength, res.re_b_single, col=200, linestyle=2
     oplot, res.wavelength, res.re_b_single, psym=1, col=200
     errplot, res.wavelength, res.re_b_single-fact*res.reerr_b_single, res.re_b_single+fact*res.reerr_b_single, col=200
     oplot, res.wavelength, res.re_d_single, col=95, linestyle=2
     oplot, res.wavelength, res.re_d_single, psym=1, col=95
     errplot, res.wavelength, res.re_d_single-fact*res.reerr_d_single, res.re_d_single+fact*res.reerr_d_single, col=95
     oplot, res.wavelength, res.re_b, col=235
     oplot, res.wavelength, res.re_b, psym=1, col=235
     errplot, res.wavelength, res.re_b-fact*res.reerr_b, res.re_b+fact*res.reerr_b, col=235
     oplot, res.wavelength, res.re_d, col=135
     oplot, res.wavelength, res.re_d, psym=1, col=135
     errplot, res.wavelength, res.re_d-fact*res.reerr_d, res.re_d+fact*res.reerr_d, col=135
     legend, ['single band bulge +-'+strtrim(fact,2)+'sigma', 'mwl bulge +-'+strtrim(fact,2)+'sigma', 'single band disk +-'+strtrim(fact,2)+'sigma', 'mwl disk +-'+strtrim(fact,2)+'sigma'],textcolors=[200,235,95,135], /right, charsize=1.
     IF keyword_set(np) THEN BEGIN
        oplot, res.wavelength, res.re_b_single_np, col=200, linestyle=1
        oplot, res.wavelength, res.re_b_single_np, psym=1, col=200
        errplot, res.wavelength, res.re_b_single_np-fact*res.reerr_b_single_np, res.re_b_single_np+fact*res.reerr_b_single_np, col=200
        oplot, res.wavelength, res.re_d_single_np, col=95, linestyle=1
        oplot, res.wavelength, res.re_d_single_np, psym=1, col=95
        errplot, res.wavelength, res.re_d_single_np-fact*res.reerr_d_single_np, res.re_d_single_np+fact*res.reerr_d_single_np, col=95
        oplot, res.wavelength, res.re_b_np, col=235, linestyle=1
        oplot, res.wavelength, res.re_b_np, psym=1, col=235
        errplot, res.wavelength, res.re_b_np-fact*res.reerr_b_np, res.re_b_np+fact*res.reerr_b_np, col=235
        oplot, res.wavelength, res.re_d_np, col=135, linestyle=1
        oplot, res.wavelength, res.re_d_np, psym=1, col=135
        errplot, res.wavelength, res.re_d_np-fact*res.reerr_d_np, res.re_d_np+fact*res.reerr_d_np, col=135
    ENDIF
     multiplot

     plot, res.wavelength, res.n_b_single, psym=1,/nodata, yrange=[minn<0.5,maxn]
     oplot, res.wavelength, res.n_b_single, col=200, linestyle=2
     oplot, res.wavelength, res.n_b_single, psym=1, col=200
     errplot, res.wavelength, res.n_b_single-fact*res.nerr_b_single, res.n_b_single+fact*res.nerr_b_single, col=200
     oplot, res.wavelength, res.n_d_single, col=95, linestyle=2
     oplot, res.wavelength, res.n_d_single, psym=1, col=95
     errplot, res.wavelength, res.n_d_single-fact*res.nerr_d_single, res.n_d_single+fact*res.nerr_d_single, col=95
     oplot, res.wavelength, res.n_b, col=235
     oplot, res.wavelength, res.n_b, psym=1, col=235
     errplot, res.wavelength, res.n_b-fact*res.nerr_b, res.n_b+fact*res.nerr_b, col=235
     oplot, res.wavelength, res.n_d, col=135
     oplot, res.wavelength, res.n_d, psym=1, col=135
     errplot, res.wavelength, res.n_d-fact*res.nerr_d, res.n_d+fact*res.nerr_d, col=135
     IF keyword_set(np) THEN BEGIN
        oplot, res.wavelength, res.n_b_single_np, col=200, linestyle=1
        oplot, res.wavelength, res.n_b_single_np, psym=1, col=200
        errplot, res.wavelength, res.n_b_single_np-fact*res.nerr_b_single_np, res.n_b_single_np+fact*res.nerr_b_single_np, col=200
        oplot, res.wavelength, res.n_d_single_np, col=95, linestyle=1
        oplot, res.wavelength, res.n_d_single_np, psym=1, col=95
        errplot, res.wavelength, res.n_d_single_np-fact*res.nerr_d_single_np, res.n_d_single_np+fact*res.nerr_d_single_np, col=95
        oplot, res.wavelength, res.n_b_np, col=235, linestyle=1
        oplot, res.wavelength, res.n_b_np, psym=1, col=235
        errplot, res.wavelength, res.n_b_np-fact*res.nerr_b_np, res.n_b_np+fact*res.nerr_b_np, col=235
        oplot, res.wavelength, res.n_d_np, col=135, linestyle=1
        oplot, res.wavelength, res.n_d_np, psym=1, col=135
        errplot, res.wavelength, res.n_d_np-fact*res.nerr_d_np, res.n_d_np+fact*res.nerr_d_np, col=135
        legend, ['with nonparametric'], psym=0, linestyle=1, charsize=1., /bottom, /right
     ENDIF
     multiplot

     plot, res.wavelength, res.q_b_single, psym=1,/nodata, yrange=[minq,maxq]
     oplot, res.wavelength, res.q_b_single, col=200, linestyle=2
     oplot, res.wavelength, res.q_b_single, psym=1, col=200
     errplot, res.wavelength, res.q_b_single-fact*res.qerr_b_single, res.q_b_single+fact*res.qerr_b_single, col=200
     oplot, res.wavelength, res.q_d_single, col=95, linestyle=2
     oplot, res.wavelength, res.q_d_single, psym=1, col=95
     errplot, res.wavelength, res.q_d_single-fact*res.qerr_d_single, res.q_d_single+fact*res.qerr_d_single, col=95
     oplot, res.wavelength, res.q_b, col=235
     oplot, res.wavelength, res.q_b, psym=1, col=235
     errplot, res.wavelength, res.q_b-fact*res.qerr_b, res.q_b+fact*res.qerr_b, col=235
     oplot, res.wavelength, res.q_d, col=135
     oplot, res.wavelength, res.q_d, psym=1, col=135
     errplot, res.wavelength, res.q_d-fact*res.qerr_d, res.q_d+fact*res.qerr_d, col=135
     IF keyword_set(np) THEN BEGIN
        oplot, res.wavelength, res.q_b_single_np, col=200, linestyle=1
        oplot, res.wavelength, res.q_b_single_np, psym=1, col=200
        errplot, res.wavelength, res.q_b_single_np-fact*res.qerr_b_single_np, res.q_b_single_np+fact*res.qerr_b_single_np, col=200
        oplot, res.wavelength, res.q_d_single_np, col=95, linestyle=1
        oplot, res.wavelength, res.q_d_single_np, psym=1, col=95
        errplot, res.wavelength, res.q_d_single_np-fact*res.qerr_d_single_np, res.q_d_single_np+fact*res.qerr_d_single_np, col=95
        oplot, res.wavelength, res.q_b_np, col=235, linestyle=1
        oplot, res.wavelength, res.q_b_np, psym=1, col=235
        errplot, res.wavelength, res.q_b_np-fact*res.qerr_b_np, res.q_b_np+fact*res.qerr_b_np, col=235
        oplot, res.wavelength, res.q_d_np, col=135, linestyle=1
        oplot, res.wavelength, res.q_d_np, psym=1, col=135
     ENDIF
     
     xyouts, 0.03, 0.86, 'mag', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.64, 're', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.42, 'n', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.03, 0.20, 'q', normal = 1,/data, charsize=1.3, orientat = 90
     xyouts, 0.43, 0.02, 'wavelength', normal = 1,/data, charsize=1.3

     multiplot, /reset

     IF keyword_set(ps) THEN BEGIN
        device,/close
        SET_PLOT, 'X'
     ENDIF
     
  ENDELSE
  
END

