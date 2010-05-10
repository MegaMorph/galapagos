function SXPAR, hdr, name, abort, COUNT=matches, COMMENT = comments, $
                                  NoContinue = NoContinue, SILENT = silent
;+
; NAME:
;      SXPAR
; PURPOSE:
;      Obtain the value of a parameter in a FITS header
;
; CALLING SEQUENCE:
;      result = SXPAR( Hdr, Name, [ Abort, COUNT=, COMMENT =, /NoCONTINUE  ])   
;
; INPUTS:
;      Hdr =  FITS header array, (e.g. as returned by READFITS) 
;             string array, each element should have a length of 80 characters      
;
;      Name = String name of the parameter to return.   If Name is of the
;             form 'keyword*' then an array is returned containing values of
;             keywordN where N is an integer.  The value of keywordN will be
;             placed in RESULT(N-1).  The data type of RESULT will be the
;             type of the first valid match of keywordN found.
;
; OPTIONAL INPUTS:
;       ABORT - string specifying that SXPAR should do a RETALL
;               if a parameter is not found.  ABORT should contain
;               a string to be printed if the keyword parameter is not found.
;               If not supplied, SXPAR will return quietly with COUNT = 0
;               (and !ERR = -1) if a keyword is not found.
;
; OPTIONAL INPUT KEYWORDS: 
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
;       /SILENT - Set this keyword to suppress warning messages about duplicate
;                 keywords in the FITS header.
;
; OPTIONAL OUTPUT KEYWORDS:
;       COUNT - Optional keyword to return a value equal to the number of 
;               parameters found by SXPAR, integer scalar
;
;       COMMENT - Array of comments associated with the returned values
;
; OUTPUTS:
;       Function value = value of parameter in header.
;               If parameter is double precision, floating, long or string,
;               the result is of that type.  Apostrophes are stripped
;               from strings.  If the parameter is logical, 1b is
;               returned for T, and 0b is returned for F.
;               If Name was of form 'keyword*' then a vector of values
;               are returned.
;
; SIDE EFFECTS:
;       !ERR is set to -1 if parameter not found, 0 for a scalar
;       value returned.  If a vector is returned it is set to the
;       number of keyword matches found.    The use of !ERR is deprecated, and
;       instead the COUNT keyword is preferred
;
;       If a keyword (except HISTORY or COMMENT) occurs more than once in a 
;       header, a warning is given, and the *last* occurence is used.
;
; EXAMPLES:
;       Given a FITS header, h, return the values of all the NAXISi values
;       into a vector.    Then place the history records into a string vector.
;
;       IDL> naxisi = sxpar( h ,'NAXIS*')         ; Extract NAXISi value
;       IDL> history = sxpar( h, 'HISTORY' )      ; Extract HISTORY records
;
; PROCEDURE:
;       The first 8 chacters of each element of Hdr are searched for a 
;       match to Name.  The value from the last 20 characters is returned.  
;       An error occurs if there is no parameter with the given name.
;
;       If a numeric value has no decimal point it is returned as type
;       LONG.   If it contains more than 8 numerals, or contains the 
;       characters 'D' or 'E', then it is returned as type DOUBLE.  Otherwise
;       it is returned as type FLOAT.    Very large integer values, outside
;       the range of valid LONG, are returned as DOUBLE.
;
;       If the value is too long for one line, it may be continued on to the
;       the next input card, using the OGIP CONTINUE convention.  For more info,
;       http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/r13.html
;
;       Complex numbers are recognized as two numbers separated by one or more
;       space characters.
;
;       If a numeric value has no decimal point (or E or D) it is returned as
;       type LONG.  If it contains more than 8 numerals, or contains the
;       character 'D', then it is returned as type DOUBLE.  Otherwise it is
;       returned as type FLOAT.    If an integer is too large to be stored as
;       type LONG, then it is returned as DOUBLE.
;
; NOTES:
;       The functions SXPAR() and FXPAR() are nearly identical, although
;       FXPAR() has slightly more sophisticated parsing.   There is no
;       particular reason for having two nearly identical procedures, but
;       both are too widely used to drop either one.
;
; PROCEDURES CALLED:
;       GETTOK(), VALID_NUM()
; MODIFICATION HISTORY:
;       DMS, May, 1983, STPAR Written.
;       D. Lindler Jan 90 added ABORT input parameter
;       J. Isensee Jul,90 added COUNT keyword
;       W. Thompson, Feb. 1992, added support for FITS complex values.
;       W. Thompson, May 1992, corrected problem with HISTORY/COMMENT/blank
;               keywords, and complex value error correction.
;       W. Landsman, November 1994, fix case where NAME is an empty string 
;       W. Landsman, March 1995,  Added COMMENT keyword, ability to read
;               values longer than 20 character
;       W. Landsman, July 1995, Removed /NOZERO from MAKE_ARRAY call
;       T. Beck May 1998, Return logical as type BYTE
;       W. Landsman May 1998, Make sure integer values are within range of LONG
;       Converted to IDL V5.0, May 1998
;       W. Landsman Feb 1998, Recognize CONTINUE convention 
;       W. Landsman Oct 1999, Recognize numbers such as 1E-10 as floating point
;       W. Landsman Jan 2000, Only accept integer N values when name = keywordN
;       W. Landsman Dec 2001, Optional /SILENT keyword to suppress warnings
;       W. Landsman/D. Finkbeiner  Mar 2002  Make sure extracted vectors 
;             of mixed data type are returned with the highest type.
;-
;----------------------------------------------------------------------
 if N_params() LT 2 then begin
     print,'Syntax -     result =  sxpar( hdr, name, [abort])'
     print,'   Input Keywords:    /NOCONTINUE, /SILENT'
     print,'   Output Keywords:   COUNT=,  COMMENT= '
     return, -1
 endif 

 VALUE = 0
 if N_params() LE 2 then begin
      abort_return = 0
      abort = 'FITS Header'
 end else abort_return = 1
 if abort_return then On_error,1 else On_error,2

;       Check for valid header

  s = size(hdr)         ;Check header for proper attributes.
  if ( s[0] NE 1 ) or ( s[2] NE 7 ) then $
           message,'FITS Header (first parameter) must be a string array'

  nam = strtrim( strupcase(name) )      ;Copy name, make upper case     


;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.

   namelength1 = (strlen(nam) - 1 ) > 1         
   if strpos( nam, '*' ) EQ namelength1 then begin    
            nam = strmid( nam, 0, namelength1)  
            vector = 1                  ;Flag for vector output  
            name_length = strlen(nam)   ;Length of name 
            num_length = 8 - name_length        ;Max length of number portion  
            if num_length LE 0 then  $ 
                  message, 'Keyword length must be 8 characters or less'

;  Otherwise, extend NAME with blanks to eight characters.

    endif else begin  
                while strlen(nam) LT 8 do nam = nam + ' ' ;Make 8 chars long
                vector = 0      
    endelse


;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in NFOUND, and the
;  value of the number field in NUMBER.

        histnam = (nam eq 'HISTORY ') or (nam eq 'COMMENT ') or (nam eq '') 
        if N_elements(start) EQ 0 then start = -1l
        start = long(start[0])
        if (not vector) and (start GE 0) then begin
            if N_elements(precheck)  EQ 0 then precheck = 5
            if N_elements(postcheck) EQ 0 then postcheck = 20
            nheader = N_elements(hdr)
            mn = (start - precheck)  > 0
            mx = (start + postcheck) < nheader-1
            keyword = strmid(hdr[mn:mx], 0, 8)
        endif else begin
            restart:
            start   = -1l
            keyword = strmid( hdr, 0, 8)
        endelse

        if vector then begin
            nfound = where(strpos(keyword,nam) GE 0, matches)
            if ( matches gt 0 ) then begin
                numst= strmid( hdr[nfound], name_length, num_length)
                number = replicate(-1, matches)
                for i = 0, matches-1 do         $
                    if VALID_NUM( numst[i], num,/INTEGER) then number[i] = num
                igood = where(number GE 0, matches)
                if matches GT 0 then begin
                    nfound = nfound[igood]
                    number = number[igood]
                endif
            endif

;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then print an error
;  message.

        endif else begin
            nfound = where(keyword EQ nam, matches)
            if (matches EQ 0) and (start GE 0) then goto, RESTART
            if (start GE 0) then nfound = nfound + mn
            if (matches GT 1) and (not histnam) then        $
                if not keyword_set(silent) then $
                message,/informational, 'Warning - keyword ' +   $
                nam + ' located more than once in ' + abort
            if (matches GT 0) then start = nfound[matches-1]
        endelse


; Process string parameter 

 if matches GT 0 then begin
  line = hdr[nfound]
  svalue = strtrim( strmid(line,9,71),2)
  if histnam then $
        value = strtrim(strmid(line,8,71),2) else for i = 0,matches-1 do begin
      if ( strmid(svalue[i],0,1) EQ "'" ) then begin   ;Is it a string?
                  test = strmid( svalue[i],1,strlen( svalue[i] )-1)
                  next_char = 0
                  off = 0
                  value = '' 
          NEXT_APOST:
                  endap = strpos(test, "'", next_char)      ;Ending apostrophe  
                  if endap LT 0 then $ 
                            MESSAGE,'Value of '+name+' invalid in '+abort
                  value = value + strmid( test, next_char, endap-next_char )  

;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.

                 if strmid( test, endap+1, 1) EQ "'" then begin    
                    value = value + "'"
                    next_char = endap+2         
                    goto, NEXT_APOST
                 endif      

; Extract the comment, if any
                
                slash = strpos( test, "/", endap )
                if slash LT 0 then comment = '' else    $
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )

; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
; SXPAR) 4. /NOCONTINE is not set

    if not keyword_set(nocontinue) then begin
                off = off + 1
                val = strtrim(value,2)

                if (strlen(val) gt 0) and $
                  (strmid(val, strlen(val)-1, 1) EQ '&') and $
                  (strmid(hdr[nfound[i]+off],0,8) EQ 'CONTINUE') then begin
                   if (size(sxpar(hdr, 'LONGSTRN',/NoCONTINUE)))[1] EQ 7 then begin                    
                  value = strmid(val, 0, strlen(val)-1)
                  test = hdr[nfound[i]+off]
                  test = strmid(test, 8, strlen(test)-8)
                  test = strtrim(test, 2)
                  if strmid(test, 0, 1) NE "'" then message, $
                    'ERROR: Invalidly CONTINUEd string in '+ abort
                  next_char = 1
                  GOTO, NEXT_APOST
                ENDIF
               ENDIF
    ENDIF


; Process non-string value  

          endif else begin

                test = svalue[i]
                slash = strpos( test, "/" )
                if slash GT 0 then begin
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )
                        test = strmid( test, 0, slash )
                end else comment = ''

; Find the first word in TEST.  Is it a logical value ('T' or 'F')

                test2 = test
                value = gettok(test2,' ')
               if ( value EQ 'T' ) then value = 1b else $
               if ( value EQ 'F' ) then value = 0b else begin

;  Test to see if a complex number.  It's  a complex number if the value and
;  the next word, if any, are both valid values.

                if strlen(test2) EQ 0 then goto, NOT_COMPLEX
                value2 = gettok( test2, ' ') 
                if value2 EQ '' then goto, NOT_COMPLEX
                On_ioerror, NOT_COMPLEX
                value2 = float(value2)
                value = complex(value,value2)
                goto, GOT_VALUE

;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.

NOT_COMPLEX:
                On_IOerror, GOT_VALUE
                  if (strpos(value,'.') GE 0) or (strpos(value,'E') GT 0) $
                  or (strpos(value,'D') GE 0) then begin  ;Floating or double?
                      if ( strpos(value,'D') GT 0 ) or $  ;Double?
                         ( strlen(value) GE 8 ) then value = double(value) $
                                                else value = float(value)
                       endif else begin                   ;Long integer
                            lmax = 2.0d^31 - 1.0d
                            lmin = -2.0d31
                            value = double(value)
                            if (value GE lmin) and (value LE lmax) then $
                                value = long(value)
                       endelse

GOT_VALUE:
                On_IOerror, NULL
                endelse
             endelse; if c eq apost

;  Add to vector if required

         if vector then begin
               if ( i EQ 0 ) then begin
                     maxnum = max(number)
                     dtype = size(value,/type)
                     result = make_array( maxnum, TYPE = dtype )
                     comments = strarr( maxnum )
               endif 
               if size(value,/type) GT dtype then begin   ;Do we need to recast?
                    result = result + 0*value
                    dtype = size(value,/type)
               endif
               result[ number[i]-1 ] =  value
               comments[ number[i]-1 ] = comment
          endif else $
                comments = comment
  endfor

  if vector then begin
         !ERR = matches     
         return, result
  endif else !ERR = 0

endif  else  begin    
     if abort_return then message,'Keyword '+nam+' not found in '+abort
     !ERR = -1
endelse     

return, value       

END                 
