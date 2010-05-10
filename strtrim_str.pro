function strtrim_str,stringin,string2,flag,TRIM=trim
;remove leading/trailing/both ends string from other string
;       [0,     1,       2]
;TRIM removes whitespace from both ends
string1=stringin
if keyword_set(trim) then string1=strtrim(string1,2)

if flag eq 1 or flag eq 2 then begin
	repeat begin
		length=strlen(string1)
		pos=strpos(string1,string2,/reverse_search)
		if pos eq length-1 then $
		string1=strmid(string1,0,length-1)
	endrep until length eq strlen(string1)
endif

if flag eq 0 or flag eq 2 then begin
	repeat begin
		length=strlen(string1)
		pos=strpos(string1,string2)
		if pos eq 0 then $
		string1=strmid(string1,1,length)
	endrep until length eq strlen(string1)
endif

return,string1
end
