pro clean_galfit_folder, path
; .run clean_galfit_folder
; clean_galfit_folder, '/usersVol1/haeussler/vgala'
CD, path

spawn, 'ls */galfit/*sav', list

for n=0,n_elements(list)-1 do begin
    pre = ''
    pre = strmid(list[n],0,strpos(list[n],'_gf.sav'))
    spawn, 'rm '+pre+'_obj'
    spawn, 'rm '+list[n]
endfor

end
