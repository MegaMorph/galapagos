PRO create_output_from_obj, obj, layers, gal_exe
; .run create_output_from_obj.pro
; create_output_from_obj, '/Users/haeussler/Documents/Dropbox/Arianna_Fits/414.556/414.556_obj_adapt','blank,input,model,residual,psf','~/megamorph/galfit/exec/galfitm-1.2.0-osx'
  spawn, 'pwd', infolder
  IF strpos(obj,'/') NE -1 THEN BEGIN
     workfolder = strmid(obj,0,strpos(obj,'/',/REVERSE_SEARCH))
     CD, workfolder
     obj = strmid(obj,strpos(obj,'/',/REVERSE_SEARCH)+1)
  ENDIF

; go through setup file one line at a time and change line with output file format to include all information
  openr, filer, obj, /get_lun
  line = ''

  newfile = obj+'_output'
  openw, filew, newfile, /get_lun

  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
     readf, filer, line
     
; change format of output file
     IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
        printf, filew, start+' '+layers+'  '+comment
     ENDIF ELSE printf, filew, line
     
  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew

; run fit with free parameters
  spawn, gal_exe+' '+newfile
  spawn, 'rm fit.log '+newfile

  CD, infolder
END
