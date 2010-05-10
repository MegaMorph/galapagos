        FUNCTION FXPOSIT, XFILE, EXT_NO, readonly=readonly, COMPRESS=COMPRESS, $
                 SILENT = Silent, EXTNUM = extnum, ERRMSG= ERRMSG
;+
; NAME:
;     FXPOSIT
; PURPOSE:
;     Return the unit number of a FITS file positioned at specified extension
; EXPLANATION:
;     The FITS file will be ready to be read at the beginning of the 
;     specified extension.    Exither an extension number or extension name
;     can be specified.   Called by headfits.pro, mrdfits.pro, readfits.pro
;
; CALLING SEQUENCE:
;     unit=FXPOSIT(FILE, EXT_NO_OR_NAME, /READONLY, COMPRESS=program, /SILENT)
;
; INPUT PARAMETERS:
;     FILE    = FITS file name, scalar string
;     EXT_NO_OR_NAME  = Either the extension to be moved to (scalar 
;               nonnegative integer) or the name of the extension to read 
;               (scalar string)
;
; RETURNS:
;     Unit number of file or -1 if an error is detected.
;
; OPTIONAL INPUT KEYWORD PARAMETER:
;     /READONLY - If this keyword is set and non-zero, then OPENR rather 
;                than OPENU will be used to open the FITS file.
;     COMPRESS - If this keyword is set and non-zero, then then treat
;                the file as compressed.  If 1 assume a gzipped file.
;                and use IDLs internal decompression facility.    For Unix 
;                compressed or bzip2 compressed files spawn off a process to 
;                decompress and use its output as the FITS stream.  If the 
;                keyword is not 1, then use its value as a string giving the 
;                command needed for decompression.
;     /SILENT    If set, then suppress any messages about invalid characters
;                in the FITS file.
;
; OPTIONAL OUTPUT KEYWORD:
;      EXTNUM - Nonnegative integer give the extension number actually read
;               Useful only if the extension was specified by name.
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
; SIDE EFFECTS:
;      Opens and returns the descriptor of a file.
; PROCEDURE:
;      Open the appropriate file, or spawn a command and intercept
;      the output.
;      Call FXMOVE to get to the appropriate extension.
; PROCEDURE CALLS:
;      FXMOVE()
; MODIFICATION HISTORY:
;      Derived from William Thompson's FXFINDEND routine.
;      Modified by T.McGlynn, 5-October-1994.
;       Modified by T.McGlynn, 25-Feb-1995 to handle compressed
;          files.  Pipes cannot be accessed using FXHREAD so
;          MRD_HREAD was written.
;       W. Landsman 23-Apr-1997    Force the /bin/sh shell when uncompressing 
;       T. McGlynn  03-June-1999   Use /noshell option to get rid of processes left by spawn.
;                                  Use findfile to retain ability to use wildcards
;       W. Landsman 03-Aug-1999    Use EXPAND_TILDE under Unix to find file
;       T. McGlynn  04-Apr-2000    Put reading code into FXMOVE,
;                                  additional support for compression from D.Palmer.
;       W. Landsman/D.Zarro 04-Jul-2000    Added test for !VERSION.OS EQ 'Win32' (WinNT)
;       W. Landsman    12-Dec-2000 Added /SILENT keyword
;       W. Landsman April 2002     Use FILE_SEARCH for V5.5 or later
;       W. Landsman Feb 2004       Assume since V5.3 (OPENR,/COMPRESS available)
;       W. Landsman,W. Thompson, 2-Mar-2004, Add support for BZIP2 
;       W. Landsman                Don't leave open file if an error occurs
;       W. Landsman  Sep 2004      Treat FTZ extension as gzip compressed
;       W. Landsman  Feb 2006      Removed leading spaces (prior to V5.5)
;       W. Landsman  Nov 2006      Allow specification of extension name
;                                  Added EXTNUM, ERRMSG keywords
;-
;
        ON_ERROR,2
        compile_opt idl2  
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 2 THEN BEGIN 
            PRINT,'SYNTAX:  UNIT = FXPOSIT(FILE, EXT_NO, /Readonly,' + $
	                   '/SILENT, compress=prog)'
            RETURN,-1
        ENDIF
        PRINTERR = NOT ARG_PRESENT(ERRMSG)
	ERRMSG = ''

            FILE = FILE_SEARCH(XFILE, COUNT=COUNT) 

        IF COUNT EQ 0 THEN BEGIN
	    ERRMSG = 'Specified FITS File not found ' 
	    IF PRINTERR THEN MESSAGE,ERRMSG,/CON 
            RETURN, -1   ; Don't print anything out, just report an error
	ENDIF    
        
        FILE = FILE[0]
;
;  Check if this is a compressed file.
;
        UNIT = -1

        UCMPRS = ' '
	IF KEYWORD_SET(compress) THEN BEGIN
	    IF strcompress(string(compress),/remo) eq '1' THEN BEGIN
	        compress = 'gunzip'
	    ENDIF
	    UCMPRS = compress;
	ENDIF ELSE BEGIN
        
            LEN = STRLEN(FILE)
            IF LEN GT 3 THEN $
	        TAIL = STRLOWCASE(STRMID(FILE, LEN-3, 3))  $
	    ELSE TAIL = ' '
	    
            IF STRMID(TAIL,1,2) EQ '.z'  THEN $
                UCMPRS = 'uncompress'   $
	    ELSE IF TAIL EQ '.gz' or tail EQ 'ftz' THEN $
	        UCMPRS = 'gunzip'       $
	    ELSE IF TAIL EQ 'bz2' THEN $
	        UCMPRS = 'bunzip2'
	    
	ENDELSE
                
;  Handle compressed files.

	IF UCMPRS EQ 'gunzip' THEN BEGIN
	        
                IF KEYWORD_SET(READONLY) THEN BEGIN
                    OPENR, UNIT, FILE, /COMPRESS, /GET_LUN, ERROR = ERROR
                ENDIF ELSE BEGIN
                    OPENU, UNIT, FILE, /COMPRESS, /GET_LUN, ERROR = ERROR
                ENDELSE

	ENDIF ELSE IF UCMPRS NE ' ' THEN BEGIN
		
                IF (!VERSION.OS_FAMILY EQ 'unix') THEN BEGIN
                        SPAWN, [UCMPRS,'-c',FILE], UNIT=UNIT, /NOSHELL
                ENDIF ELSE BEGIN
                        PRINT, 'MRDFITS: Only Unix IDL supports piped spawns'
                        PRINT, '         File must be uncompressed manually'
                        RETURN, -1                      
                ENDELSE
                
        ENDIF ELSE BEGIN
;
;  Go to the start of the file.
;
                IF KEYWORD_SET(READONLY) THEN $
                      OPENR, UNIT, FILE, /GET_LUN, ERROR = ERROR $
                ELSE  OPENU, UNIT, FILE, /GET_LUN, ERROR = ERROR

                IF ERROR NE 0 THEN BEGIN
                        IF PRINTERR THEN PRINT,!ERROR_STATE.MSG ELSE $
			    ERRMSG = !ERROR_STATE.MSG 
                        RETURN,-1
                ENDIF
        ENDELSE
	
        IF SIZE(EXT_NO,/TNAME) NE 'STRING' THEN $
	      IF EXT_NO LE 0 THEN RETURN, UNIT

	STAT = FXMOVE(UNIT, EXT_NO, SILENT = Silent, EXT_NO = extnum, $
	ERRMSG=ERRMSG)
	IF STAT LT 0 THEN BEGIN
            FREE_LUN, UNIT
	    RETURN, STAT
	ENDIF ELSE RETURN, UNIT
END


