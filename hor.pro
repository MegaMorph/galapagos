;-------------------------------------------------------------
;+
; NAME:
;       HOR
; PURPOSE:
;       Plot a horizontal line on a graph at specified y value.
; CATEGORY:
; CALLING SEQUENCE:
;       hor, y
; INPUTS:
;       y = Y value of horizontal line.  Scalar or array.    in
; KEYWORD PARAMETERS:
;       Keywords:
;         /DEVICE means work in device coordinates.
;         /NORMALIZED means work in normalized coordinates.
;           Default is data coordinates.
;         LINESTYLE=s.  Linestyle (def=!p.linestyle).
;         COLOR=c.      Line color (def=!p.color).
;         THICKNESS=t   Line thickness (def=!p.thick).
;         FILL=clr        Optional color to fill between line pairs.
;           Fills between lines 0 and 1, 2 and 3, and so on.
;         POINTER=pt      Draw arrowhead pointers at left and right
;           instead of lines.  Arrowhead dimensions may be given as
;           fraction of screen or plot window size, the value of
;           pt is height, or [height, width].  For /pointer the
;           default used is [.03,.03].
;         /LEFT  used with POINTER to plot left pointers only.
;         /RIGHT used with POINTER to plot right pointers only.
;         /OUT   Keep pointers outside axes (Data coord only).
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
;       Notes: see ver.
; MODIFICATION HISTORY:
;       R. Sterner, 2 Aug, 1989.
;       R. Sterner, 21 May, 1992 --- fixed for log X axes.
;       R. Sterner,  3 Nov, 1992 --- Added /device.
;       R. Sterner, 20 Jun, 1993 --- Added /normalized.
;       R. Sterner,  1994 Feb  2 --- Added THICK.
;       R. Sterner, 1994 Jun 3 --- Added FILL.
;       R. Sterner, 1994 Jun 16 --- Added POINTER, /TOP, /BOTTOM.
;       R. Sterner, 1997 Jul 11 --- Added /OUT.
;
; Copyright (C) 1989, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro hor, y, help=hlp, device=device, linestyle=ls, color=clr, $
	  thickness=thk, normalized=norm, fill=fill, pointer=pt, $
	  left=left, right=right, out=out
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Plot a horizontal line on a graph at specified y value.'
	  print,' hor, y'
	  print,'   y = Y value of horizontal line.  Scalar or array.    in'
	  print,' Keywords:'
          print,'   /DEVICE means work in device coordinates.'
          print,'   /NORMALIZED means work in normalized coordinates.'
	  print,'     Default is data coordinates.'
	  print,'   LINESTYLE=s.  Linestyle (def=!p.linestyle).'
	  print,'   COLOR=c.      Line color (def=!p.color).'
	  print,'   THICKNESS=t   Line thickness (def=!p.thick).'
          print,'   FILL=clr        Optional color to fill between line pairs.'
          print,'     Fills between lines 0 and 1, 2 and 3, and so on.'
          print,'   POINTER=pt      Draw arrowhead pointers at left and right'
          print,'     instead of lines.  Arrowhead dimensions may be given as'
          print,'     fraction of screen or plot window size, the value of'
          print,'     pt is height, or [height, width].  For /pointer the'
          print,'     default used is [.03,.03].'
	  print,'   /LEFT  used with POINTER to plot left pointers only.'
	  print,'   /RIGHT used with POINTER to plot right pointers only.'
	  print,'   /OUT   Keep pointers outside axes (Data coord only).'
	  print,' Notes: see ver.'
	  return
	end
 
	yy = y
	n = n_elements(yy)
	if n_elements(ls) eq 0 then ls = !p.linestyle
	if n_elements(clr) eq 0 then clr = !p.color
	if n_elements(thk) eq 0 then thk = !p.thick
        ;------  Handle pointers  -----------
        if n_elements(pt) eq 0 then pt = 0
        pflag = 0
        if pt(0) gt 0 then begin
          if pt(0) eq 1 then pt=.03
          ht = pt(0)
          wd = pt(n_elements(pt)-1)
          if n_elements(pt) eq 1 then wd = ht/2.
          pflag = 1
        endif
	lflag=0
	rflag=0
	if keyword_set(left) then lflag=1
	if keyword_set(right) then rflag=1
	if (lflag+rflag) eq 0 then begin
	  lflag=1
	  rflag=1
	endif
 
        ;--------  Device  ------------
        if keyword_set(device) then begin
          xx = [0,!d.x_size-1]
          for i = 0, n-1 do begin
            ;--------  Filled line pairs  -----------
            if n_elements(fill) ne 0 then begin
              if (i mod 2) eq 0 then begin
                x1 = xx(0) & x2 = xx(1)
                y1 = yy(i) & y2 = yy((i+1)<(n-1))
                polyfill, /dev, [x1,x2,x2,x1],[y1,y1,y2,y2],color=fill
              endif
            ;---------  Single lines  ---------------
            endif else if pflag eq 0 then begin
              plots, /device,xx,[0,0]+yy(i),linestyle=ls,color=clr,thick=thk
            ;---------  Pointers  -------------------
            endif else begin
              dx = round((!d.x_size-1)*ht)
              x1 = [0,dx,0]
              x2 = !d.x_size-1 - [0,dx,0]
              dy = round((!d.y_size-1)*wd/2.)
              dy = [-dy,0,dy]+yy(i)
              if lflag then polyfill,/dev,x1,dy,col=clr
              if rflag then polyfill,/dev,x2,dy,col=clr
            endelse
          endfor
	;---------  Normalized  ----------
        endif else if keyword_set(norm) then begin
          xx = [0,1]
          for i = 0, n-1 do begin
            ;--------  Filled line pairs  -----------
            if n_elements(fill) ne 0 then begin
              if (i mod 2) eq 0 then begin
                x1 = xx(0) & x2 = xx(1)
                y1 = yy(i) & y2 = yy((i+1)<(n-1))
                polyfill, /norm, [x1,x2,x2,x1],[y1,y1,y2,y2],color=fill
              endif
            ;---------  Single lines  ---------------
            endif else if pflag eq 0 then begin
              plots, /norm,xx,[0,0]+yy(i),linestyle=ls,color=clr,thick=thk
            ;---------  Pointers  -------------------
            endif else begin
              dx = ht
              x1 = [0,dx,0]
              x2 = [1,1-dx,1]
              dy = wd/2.
              dy = [-dy,0,dy]+yy(i)
              if lflag then polyfill,/norm,x1,dy,col=clr
              if rflag then polyfill,/norm,x2,dy,col=clr
            endelse
          endfor
	;----------  Data  -------------
        endif else begin
	  xx = [!x.range, !x.crange]
	  for i = 0, n-1 do begin
	    ;------  Linear X axis  ------------
	    if !x.type eq 0 then begin
              ;--------  Filled line pairs  -----------
              if n_elements(fill) ne 0 then begin
                if (i mod 2) eq 0 then begin
                  x1 = min(xx)  &  x2 = max(xx)
                  y1 = yy(i) & y2 = yy((i+1)<(n-1))
                  polyfill, [x1,x2,x2,x1],[y1,y1,y2,y2],color=fill,noclip=0
                endif
              ;---------  Single lines  ---------------
              endif else if pflag eq 0 then begin
	        oplot,[min(xx),max(xx)],[1.,1.]*yy(i),linestyle=ls,$
		  color=clr, thick=thk
              ;---------  Pointers  -------------------
              endif else begin
                dx = (!x.crange(1)-!x.crange(0))*ht
                x1 = [0,dx,0]+!x.crange(0)
                x2 = [0,-dx,0]+!x.crange(1)
	        if keyword_set(out) then begin
	  	  x1 = x1-dx
		  x2 = x2+dx
	        endif
                dy = (!y.crange(1)-!y.crange(0))*wd/2.
                if !y.type eq 0 then dy=[-dy,0,dy]+yy(i) else $
                  dy=10^([-dy,0,dy]+alog10(yy(i)))
                if lflag then polyfill,x1,dy,col=clr
                if rflag then polyfill,x2,dy,col=clr
              endelse
	    ;------  Log X axis  ------------
	    endif else begin
              ;--------  Filled line pairs  -----------
              if n_elements(fill) ne 0 then begin
                if (i mod 2) eq 0 then begin
                  y1 = yy(i) & y2 = yy((i+1)<(n-1))
                  x1 = min(xx)  &  x2 = max(xx)
                  polyfill, 10^[x1,x2,x2,x1],[y1,y1,y2,y2],color=fill,noclip=0
                endif
              ;---------  Single lines  ---------------
              endif else if pflag eq 0 then begin
	        oplot,10^[min(xx),max(xx)],[1.,1.]*yy(i),linestyle=ls,$
		  color=clr, thick=thk
              ;---------  Pointers  -------------------
              endif else begin
                dy = (!y.crange(1)-!y.crange(0))*wd/2.
                if !y.type eq 0 then dy=[-dy,0,dy]+yy(i) else $
                  dy=10^([-dy,0,dy]+alog10(yy(i)))
                dx = (!x.crange(1)-!x.crange(0))*ht
                x1 = 10^([0,dx,0]+!x.crange(0))
                x2 = 10^([0,-dx,0]+!x.crange(1))
	        if keyword_set(out) then begin
                  x1 = 10^([0,dx,0]-dx+!x.crange(0))
                  x2 = 10^([0,-dx,0]+dx+!x.crange(1))
	        endif
                if lflag then polyfill,x1,dy,col=clr
                if rflag then polyfill,x2,dy,col=clr
              endelse
	    endelse  ; !x.type.
	  endfor
	endelse
 
	return
	end
