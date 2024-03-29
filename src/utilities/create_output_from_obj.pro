PRO create_output_from_obj, obj, layers, gal_exe
; this script does REPEAT the fit with new output options, e.g. can
; NOT be used to simply create the residuals
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
; split up into parts
     content_numbers = ' '
     content_descriptor = ' '
     start = strtrim(strmid(line,0,strpos(line,')')+2),2)
     content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
     comment = strtrim(strmid(line,strpos(line,'#')),2)

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
