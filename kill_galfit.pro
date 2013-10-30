
pro kill_galfit, task, time_limit, mac=mac
; .run kill_galfit.pro
; kill_galfit, 'galfitm-0.1.2.1', 180

spawn, 'whoami', id
spawn, 'ps -u '+id+' | grep '+task, ps
ps=strtrim(ps,2)

if not keyword_set(mac) then begin
   for i=0,n_elements(ps)-1 do begin
      rest=' '
      pid = strtrim(strmid(ps[i], 0, strpos(ps[i],' ')),2)
      rest = strtrim(strmid(ps[i], strpos(ps[i],' ')),2)
      rest = strtrim(strmid(rest, strpos(rest,' ')),2)

      h = fix(strmid(rest, 0, strpos(rest,':')))
      rest = strtrim(strmid(rest, strpos(rest,':')+1),2)
      min = fix(strmid(rest, 0, strpos(rest,':')))
   endfor

   if h*60+min gt time_limit then print, 'process '+pid+' killed, running too long'
   if h*60+min gt time_limit then spawn, 'kill -9 '+pid
   
endif

if keyword_set(mac) then begin
   if n_elements(ps) gt 1 then begin
      for i=0,n_elements(ps)-2 do begin
         rest=' '
         pshere = strtrim(strmid(ps[i], strpos(ps[i],' ')),2)
         pid = strtrim(strmid(pshere, 0, strpos(pshere,' ',2)),2)
         rest = strtrim(strmid(pshere, strpos(pshere,' ')),2)
         rest = strtrim(strmid(rest, strpos(rest,' ')),2)
         
         min = fix(strmid(rest, 0, strpos(rest,':')))
         
         if min gt time_limit then print, 'process '+pid+' killed, running too long'
         if min gt time_limit then spawn, 'kill -9 '+pid
         
      endfor
   endif
endif
   
end
