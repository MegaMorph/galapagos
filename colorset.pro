PRO COLORSET, RETAIN=RETAIN, DECOMPOSED=DECOMPOSED, PSEUDOCOLOR=PSEUDOCOLOR, $
  QUIET=QUIET

;+
; NAME:
;    COLORSET
;
; PURPOSE:
;    Select true color (24 bit) if available, or pseudo color (8 bit) visual
;    consistently on X, Windows, and Macintosh.
;
; CATEGORY:
;    Startup utilities.
;
; CALLING SEQUENCE:
;    COLORSET
;
; INPUTS:
;    None
;
; OPTIONAL INPUTS:
;    None
;	
; KEYWORD PARAMETERS:
;    RETAIN        Specifies the default method used
;                  for backing store when creating new windows.
;                  0 => No backing store
;                  1 => Server or window system performs backing store
;                  2 => Make IDL perform backing store (DEFAULT)
;    DECOMPOSED    Specifies the the way in which graphics
;                  color index values are interpreted when using displays with
;                  decomposed color (TrueColor or DirectColor visuals).
;                  0 = > Color indices given by single 8 bit values (DEFAULT)
;                  1 = > Color indices given by three 8 bit values
;    PSEUDOCOLOR   If set, try selecting PseudoColor (8-bit) mode
;                  (default is to try selecting TrueColor (24-bit) mode).
;    QUIET         If set, no color information is printed
;                  (default is to print the color table size, and number of colors).
;              
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    This routine changes the IDL visual for the rest of the IDL session.
;
; RESTRICTIONS:
;    Only affects X, WIN, and MAC displays.    
;    Only has an effect if run before any windows have been
;    created, and if no DEVICE commands have been executed.
;
; EXAMPLE:
;
; ;Execute the following command immediately after IDL startup.
;
; colorset
;
; MODIFICATION HISTORY:
;    Written by: Liam.Gumley@ssec.wisc.edu
;    $Id: colorset.pro,v 1.5 1999/07/16 18:38:51 gumley Exp $
;-

rcs_id = "$Id: colorset.pro,v 1.5 1999/07/16 18:38:51 gumley Exp $"

;- Check keyword values

if n_elements(retain) ne 1 then retain = 2
retain = (0 > retain) < 2

if n_elements(decomposed) ne 1 then decomposed = 0
decomposed = (0 > decomposed) < 1

;- Check for supported display

if (!d.name ne 'X') and (!d.name ne 'WIN') and (!d.name ne 'MAC') then begin
  message, 'This routine is only supported on X, WIN, and MAC displays', /continue
  return
endif

;- Check for open graphics window

if !d.window ge 0 then begin
  message, 'Window already created in this session - COLORSET may have no effect.', /continue
  message, 'To ensure COLORSET works, call it before any windows are created.', /continue
endif

;- Select color mode on X and MAC (visual cannot be changed on WIN)

if (!d.name eq 'X') or (!d.name eq 'MAC') then begin
  if keyword_set(pseudocolor) then begin
    device, pseudo_color=8
  endif else begin
    device, true_color=24
  endelse
endif

;- Select decomposition and retain mode

device, decomposed=decomposed, retain=retain
  
;- Create a window to lock in the visual type for this IDL session

current_window = !d.window
window, /free, /pixmap
wdelete, !d.window
if current_window gt 0 then wset, current_window

;- Report what happened

if not keyword_set(quiet) then begin
  device, get_visual_name=mode
  print, 'Display mode    : ', mode
  print, 'Color table size: ', strcompress( !d.table_size, /remove_all )
  print, 'Number of colors: ', strcompress( !d.n_colors, /remove_all )
  print, ''
endif
  
END    
