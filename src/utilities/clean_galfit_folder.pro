pro clean_galfit_folder, path, timestamp=timestamp
; .run clean_galfit_folder
; clean_galfit_folder, '/usersVol1/haeussler/vgala'
CD, path

spawn, 'ls */galfit/*sav', list

FOR n=0,n_elements(list)-1 DO BEGIN
    pre = ''
    pre = strmid(list[n],0,strpos(list[n],'_gf.sav'))
    spawn, 'rm '+pre+'_obj'
    spawn, 'rm '+list[n]
ENDFOR

IF keyword_set(timestamp) DO BEGIN
    spawn, 'ls */galfit/*obj', list2
    
    FOR n=0,n_elements(list2)-1 DO BEGIN
        pre = ''
        pre = strmid(list2[n],0,strpos(list[n],'obj'))
        stop
        spawn, 'rm '+pre+'_obj'
        spawn, 'rm '+list[n]
    ENDFOR

ENDIF

END

