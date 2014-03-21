; $Id: //depot/idl/IDL_63_RELEASE/idldir/lib/path_sep.pro#1 $
;
; Copyright (c) 2001-2006, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

;+
; NAME:
;	PATH_SEP
;
; PURPOSE:
;	Return the proper file path segment separator character for the
;	current operating system. This is the character used by
;	the host operating system for delimiting subdirectory names
;	in a path specification. Use of this function instead
;	of hardwiring separators makes code more portable.
;
; CATEGORY:
;	File Management.
;
; CALLING SEQUENCE:
;	Result = PATH_SEP()
;
; INPUTS:
;	None
;
; KEYWORDS:
;    SEARCH_PATH
;	If set, PATH_SEP returns the character used to separate entries
;	in a search path.
;
;    PARENT_DIRECTORY
;	If set, PATH_SEP returns the standard directory notation used
;	by the host operating system to indicate the parent of a
;	directory.
;
; OUTPUTS:
;	The path separator character is returned as a scalar string.
;
; COMMON BLOCKS:
;	None.
;
; MODIFICATION HISTORY:
;	4 April 2001, AB
;-

function path_sep, SEARCH_PATH=searchsep, PARENT_DIRECTORY=pdir

  idx = (where([ 'MacOS', 'Windows', 'unix' ] eq !version.os_family))[0]

  if (keyword_set(searchsep)) then begin
	if keyword_set(pdir) then $
		message, 'Conflicting keywords specified: SEARCH_PATH and PARENT_DIRECTORY. Returning SEARCH_PATH.', /info 
	return, ([ ',', ';', ':' ])[idx] 
  endif
  if (keyword_set(pdir)) then return, ([ ':', '..', '..' ])[idx]
  return, ([ ':', '\',  '/' ])[idx]
end
