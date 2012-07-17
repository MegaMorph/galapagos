pro kill_galfit, task, time_limit
; .run kill_galfit.pro
; kill_galfit, 'galfitm-0.1.2.1', 180

spawn, 'ps -e | grep '+task, ps

for i=0,n_elements(ps)-1 do begin
rest=' '
pid = strtrim(strmid(ps[i], 0, strpos(ps[i],' ')),2)
rest = strtrim(strmid(ps[i], strpos(ps[i],' ')),2)
rest = strtrim(strmid(rest, strpos(rest,' ')),2)

h = fix(strmid(rest, 0, strpos(rest,':')))
rest = strtrim(strmid(rest, strpos(rest,':')+1),2)
min = fix(strmid(rest, 0, strpos(rest,':')))

if h*60+min gt time_limit then print, pid+' killed, running too long'
if h*60+min gt time_limit then spawn, 'kill -9 '+pid

endfor
end
