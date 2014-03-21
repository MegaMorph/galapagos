; $Id: //depot/idl/IDL_63_RELEASE/idldir/lib/filepath.pro#1 $
;
; Copyright (c) 1989-2006, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

FUNCTION FILEPATH, FILENAME, ROOT_DIR=root_dir, SUBDIRECTORY=subdir, $
	TERMINAL = TERMINAL, TMP = TMP
;+
; NAME:
;	FILEPATH
;
; PURPOSE:
;	Given the name of a file in the IDL distribution,
;	FILEPATH returns the fully-qualified path to use in
;	opening the file. Operating system dependencies
;	are taken into consideration. This routine is used by RSI to
;	make the User Library portable.
;
; CATEGORY:
;	File Management.
;
; CALLING SEQUENCE:
;	Result = FILEPATH('filename' [, SUBDIRECTORY = subdir])
;
; INPUTS:
;    filename:	The lowercase name of the file to be opened. No device
;		or directory information should be included.
;
; KEYWORDS:
;    ROOT_DIR: The name of the directory from which the resulting path
;	should be based. If not present, the value of !DIR is used.
;	This keyword is ignored if TERMINAL or TMP are specified.
;
;    SUBDIRECTORY:	The name of the subdirectory in which the file
;		should be found. If this keyword is omitted, the main
;		directory is used.  This variable can be either a scalar
;		string or a string array with the name of each level of
;		subdirectory depth represented as an element of the array.
;
;    TERMINAL:	Return the filename of the user's terminal.
;
;    TMP:	The file is a scratch file.  Return a path to the
;		proper place for temporary files under the current operating
;		system.
;
; OUTPUTS:
;	The fully-qualified file path is returned.  If one of the subdirectory
;	keywords is not specified, the file is assumed to exist in the
;	main distribution directory.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	ROOT_DIR, TERMINAL, and TMP are mutually exclusive. Only one of
;	these should be used in a single call to FILEPATH. SUBDIRECTORY
;	does not make sense with TERMINAL or TMP.
;
; EXAMPLE:
;	To get a path to the file DETERM in the "userlib" subdirectory to the
;	IDL "lib" subdirectory, enter:
;
;		path = FILEPATH("determ", SUBDIRECTORY = ["lib", "userlib"])
;
;	The variable "path" contains a string that is the fully-qualified file
;	path for the file DETERM.
;
; MODIFICATION HISTORY:
;	December, 1989, AB, RSI (Formalized from original by DMS)
;	October, 1990, SG, RSI (added support for MSDOS)
;	February, 1991, SMR, RSI (added string array support for multi-level
;	    			  directories)
;	21 April 1993, AB, Added ROOT_DIR keyword.
;       14 July  1994, KDB, RSI - Corrected logic error in VMS section
;           of the ROOT_DIR keyword. Any sub-directory specification was
;           being ignored when using ROOT_DIR.
;	March, 1995, DJE, Add a ':' if root_dir is specified on the Mac.
;	29 July 1995, Robert.M.Candey.1@gsfc.nasa.gov, Changed VMS case for
;	    no specified path to not append '.][000000]'
;	April, 1996, DJE, Remove call to STRLOWCASE(SUBDIR).
;	August, 1996, AJH, used environment variables to define TMP on Win32
;	12 January 1998, AB, General cleanup and added 2 improvements for VMS
;           supplied by Paul Hick (pphick@ucsd.edu): (1) Add a colon to the
;           end of ROOT_DIR if it doesn't end in a ':' or ']' to allow
;           root_dir to be a logical name without the trailing ':', and
;           (2) Remove instances of '.][' that result when using rooted
;           logical names for ROOT_DIR. These changes make it easier to use
;           the same FILEPATH call across VMS and other operating systems.
;	28 January 1999, AB, use new behavior of GETTMP('IDL_TMPDIR') to obtain
;	    the correct TMP directory. This means that internal IDL and PRO
;	    code will all treat temporary files the same way.
;-


ON_ERROR,2		; Return to caller if an error occurs

do_tmp = KEYWORD_SET(TMP)		;get temporary path if existing
path = ''

IF (KEYWORD_SET(TERMINAL)) THEN BEGIN
  if ((fstat(0)).isagui) then begin
    MESSAGE, 'No terminal device available with IDLde (GUI) interface'
  endif else if (!version.os eq 'vms') then begin
    path = 'SYS$OUTPUT:'
  endif else begin	; Must be Unix. Mac and Windows are always GUI
    path = '/dev/tty'
  endelse

  return, path
ENDIF

IF (do_tmp) THEN BEGIN
  root_dir = GETENV('IDL_TMPDIR')
ENDIF ELSE BEGIN
  IF (not KEYWORD_SET(ROOT_DIR)) THEN root_dir = !DIR
  sep = PATH_SEP()
  if (!VERSION.OS eq 'vms') then  BEGIN
        ; Add a trailing ':' if root_dir does not end in ':' or ']'
        lastchar = strmid(root_dir, strlen(root_dir)-1, 1)
        IF lastchar NE "]" AND lastchar NE ":" THEN root_dir = root_dir+":"
        sep = "."
        ENDIF
  IF (KEYWORD_SET(SUBDIR)) THEN BEGIN
    ;if the SUBDIR keyword is set then concatenate the directories using
    ; the proper separator character for the current OS.
    FOR i = 0, N_ELEMENTS(SUBDIR) - 1 DO BEGIN
	path = path + SUBDIR[i]
	IF(i NE N_ELEMENTS(SUBDIR) - 1) THEN path = path + sep
    ENDFOR
    if !VERSION.OS EQ 'MacOS' THEN path = path + sep
  ENDIF
ENDELSE


CASE !VERSION.OS OF
  'vms': BEGIN
      IF (NOT do_tmp) THEN BEGIN
	IF (path EQ '') THEN begin
	  case 1 of
	    strmid(root_dir, strlen(root_dir)-2, 2) eq ".]": $
	       root_dir = strmid(root_dir,0,strlen(root_dir)-2) +']' ; remove .
	    strmid(root_dir, strlen(root_dir)-1, 1) eq "]": ; nothing
	  else: BEGIN
	      ; If root_dir is a rooted logical and there is no explicit
	      ; subdir part, we need to fill in [000000]. However, anything
              ; else should just be glued together as is.
	      scr = root_dir
              len = strlen(scr)
              if (strmid(scr, len-1, 1) eq ':') then scr =strmid(scr, 0, len-1)
	      scr = getenv(scr)
	      if (strmid(scr, $
                         strlen(scr)-2, 2) eq '.]') then $
                path = '[000000]' ; assume implicit '.]' or device in root_dir
              END
	  endcase
	ENDIF ELSE BEGIN ; path is filled
	  path = '[' + path + ']'
          ; check for a ".]" at the end of our root directory
          IF(( strmid(root_dir, strlen(root_dir)-2, 2) ne ".]") and    $
             ( strmid(root_dir, strlen(root_dir)-1, 1) eq "]") )then   $
             root_dir = strmid(root_dir,0,strlen(root_dir)-1) +'.]'
	ENDELSE
      ENDIF
    END
  'Win32': BEGIN
      if (STRMID(root_dir,STRLEN(root_dir)-1,1) ne '\') then $
          path = '\' + path
      IF ((path ne '') and (path NE '\')) THEN path = path + '\'
    END
  'MacOS': BEGIN
      ; make sure the root dir ends with a separator
      IF (STRMID(root_dir, STRLEN(root_dir) - 1, 1) NE ':') THEN $
	root_dir = root_dir + ':'
    END
  ELSE: BEGIN
      len=strlen(root_dir)
      if ((len gt 0) and (strmid(root_dir, len-1, 1) ne '/')) then $
	  path = '/' + path
      IF (path NE '') and (path NE '/') THEN path = path + '/'
    END
ENDCASE

path = root_dir + path

if (!version.os eq 'vms') then begin
    chars = strpos(path,'.][')
    ; if root_dir is something like DISKA:[IDL.] and path is something like
    ; [DATA.TMP], an invalid VMS path will be returned: DISKA:[IDL.][DATA.TMP]
    ; The solution is to remove the ']['. This happens if root_dir is from
    ; translating a rooted logical name.
    IF chars NE -1 THEN $
        path = strmid(path,0,chars+1)+strmid(path,chars+3,strlen(path)-chars-3)
endif

RETURN, path + filename

END
