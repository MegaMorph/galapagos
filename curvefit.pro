; $Id: curvefit.pro,v 1.30 2003/05/16 17:27:06 chris Exp $
;
; Copyright (c) 1982-2003, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
;+
; NAME:
;       CURVEFIT
;
; PURPOSE:
;       Non-linear least squares fit to a function of an arbitrary
;       number of parameters.  The function may be any non-linear
;       function.  If available, partial derivatives can be calculated by
;       the user function, else this routine will estimate partial derivatives
;       with a forward difference approximation.
;
; CATEGORY:
;       E2 - Curve and Surface Fitting.
;
; CALLING SEQUENCE:
;       Result = CURVEFIT(X, Y, Weights, A, SIGMA, FUNCTION_NAME = name, $
;                         ITMAX=ITMAX, ITER=ITER, TOL=TOL, /NODERIVATIVE)
;
; INPUTS:
;       X:  A row vector of independent variables.  This routine does
;           not manipulate or use values in X, it simply passes X
;           to the user-written function.
;
;       Y:  A row vector containing the dependent variable.
;
;  Weights:  A row vector of weights, the same length as Y.
;            For no weighting,
;                 Weights(i) = 1.0.
;            For instrumental (Gaussian) weighting,
;                 Weights(i)=1.0/sigma(i)^2
;            For statistical (Poisson)  weighting,
;                 Weights(i) = 1.0/y(i), etc.
;
;            For no weighting, set Weights to an undefined variable.
;
;       A:  A vector, with as many elements as the number of terms, that
;           contains the initial estimate for each parameter.  IF A is double-
;           precision, calculations are performed in double precision,
;           otherwise they are performed in single precision. Fitted parameters
;           are returned in A.
;
; KEYWORDS:
;	FITA:   A vector, with as many elements as A, which contains a zero for
;   		each fixed parameter, and a non-zero value for elements of A to
;   		fit. If not supplied, all parameters are taken to be non-fixed.
;
;       FUNCTION_NAME:  The name of the function (actually, a procedure) to
;       fit.  IF omitted, "FUNCT" is used. The procedure must be written as
;       described under RESTRICTIONS, below.
;
;       ITMAX:  Maximum number of iterations. Default = 20.
;       ITER:   The actual number of iterations which were performed
;       TOL:    The convergence tolerance. The routine returns when the
;               relative decrease in chi-squared is less than TOL in an
;               interation. Default = 1.e-3.
;       CHI2:   The value of chi-squared on exit (obselete)
;
;       CHISQ:   The value of reduced chi-squared on exit
;       NODERIVATIVE:   IF this keyword is set THEN the user procedure will not
;               be requested to provide partial derivatives. The partial
;               derivatives will be estimated in CURVEFIT using forward
;               differences. IF analytical derivatives are available they
;               should always be used.
;
;       DOUBLE = Set this keyword to force the calculation to be done in
;                double-precision arithmetic.
;
;   STATUS: Set this keyword to a named variable in which to return
;           the status of the computation. Possible values are:
;           STATUS = 0: The computation was successful.
;           STATUS = 1: The computation failed. Chi-square was
;                       increasing without bounds.
;           STATUS = 2: The computation failed to converge in ITMAX
;                       iterations.
;
;   YERROR: The standard error between YFIT and Y.
;
; OUTPUTS:
;       Returns a vector of calculated values.
;       A:  A vector of parameters containing fit.
;
; OPTIONAL OUTPUT PARAMETERS:
;       Sigma:  A vector of standard deviations for the parameters in A.
;
;     Note: if Weights is undefined, then you are assuming that
;           your model is correct. In this case, SIGMA is multiplied
;           by SQRT(CHISQ/(N-M)), where N is the number of points
;           in X and M is the number of terms in the fitting function.
;           See section 15.2 of Numerical Recipes in C (2nd ed) for details.
;
; COMMON BLOCKS:
;       NONE.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       The function to be fit must be defined and called FUNCT,
;       unless the FUNCTION_NAME keyword is supplied.  This function,
;       (actually written as a procedure) must accept values of
;       X (the independent variable), and A (the fitted function's
;       parameter values), and return F (the function's value at
;       X), and PDER (a 2D array of partial derivatives).
;       For an example, see FUNCT in the IDL User's Libaray.
;       A call to FUNCT is entered as:
;       FUNCT, X, A, F, PDER
; where:
;       X = Variable passed into CURVEFIT.  It is the job of the user-written
;           function to interpret this variable.
;       A = Vector of NTERMS function parameters, input.
;       F = Vector of NPOINT values of function, y(i) = funct(x), output.
;       PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;               PDER(I,J) = DErivative of function at ith point with
;               respect to jth parameter.  Optional output parameter.
;               PDER should not be calculated IF the parameter is not
;               supplied in call. IF the /NODERIVATIVE keyword is set in the
;               call to CURVEFIT THEN the user routine will never need to
;               calculate PDER.
;
; PROCEDURE:
;       Copied from "CURFIT", least squares fit to a non-linear
;       function, pages 237-239, Bevington, Data Reduction and Error
;       Analysis for the Physical Sciences.  This is adapted from:
;       Marquardt, "An Algorithm for Least-Squares Estimation of Nonlinear
;       Parameters", J. Soc. Ind. Appl. Math., Vol 11, no. 2, pp. 431-441,
;       June, 1963.
;
;       "This method is the Gradient-expansion algorithm which
;       combines the best features of the gradient search with
;       the method of linearizing the fitting function."
;
;       Iterations are performed until the chi square changes by
;       only TOL or until ITMAX iterations have been performed.
;
;       The initial guess of the parameter values should be
;       as close to the actual values as possible or the solution
;       may not converge.
;
; EXAMPLE:  Fit a function of the form f(x) = a * exp(b*x) + c to
;           sample pairs contained in x and y.
;           In this example, a=a(0), b=a(1) and c=a(2).
;           The partials are easily computed symbolicaly:
;           df/da = exp(b*x), df/db = a * x * exp(b*x), and df/dc = 1.0
;
;           Here is the user-written procedure to return F(x) and
;           the partials, given x:
;
;       pro gfunct, x, a, f, pder      ; Function + partials
;         bx = exp(a(1) * x)
;         f= a(0) * bx + a(2)         ;Evaluate the function
;         IF N_PARAMS() ge 4 THEN $   ;Return partials?
;         pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(f))]]
;       end
;
;         x=findgen(10)                  ;Define indep & dep variables.
;         y=[12.0, 11.0,10.2,9.4,8.7,8.1,7.5,6.9,6.5,6.1]
;         Weights=1.0/y            ;Weights
;         a=[10.0,-0.1,2.0]        ;Initial guess
;         yfit=curvefit(x,y,Weights,a,sigma,function_name='gfunct')
;         print, 'Function parameters: ', a
;         print, yfit
;       end
;
; MODIFICATION HISTORY:
;       Written, DMS, RSI, September, 1982.
;       Does not iterate IF the first guess is good.  DMS, Oct, 1990.
;       Added CALL_PROCEDURE to make the function's name a parameter.
;              (Nov 1990)
;       12/14/92 - modified to reflect the changes in the 1991
;            edition of Bevington (eq. II-27) (jiy-suggested by CreaSo)
;       Mark Rivers, U of Chicago, Feb. 12, 1995
;           - Added following keywords: ITMAX, ITER, TOL, CHI2, NODERIVATIVE
;             These make the routine much more generally useful.
;           - Removed Oct. 1990 modification so the routine does one iteration
;             even IF first guess is good. Required to get meaningful output
;             for errors.
;           - Added forward difference derivative calculations required for
;             NODERIVATIVE keyword.
;           - Fixed a bug: PDER was passed to user's procedure on first call,
;             but was not defined. Thus, user's procedure might not calculate
;             it, but the result was THEN used.
;
;      Steve Penton, RSI, June 1996.
;            - Changed SIGMAA to SIGMA to be consistant with other fitting
;              routines.
;            - Changed CHI2 to CHISQ to be consistant with other fitting
;              routines.
;            - Changed W to Weights to be consistant with other fitting
;              routines.
;            _ Updated docs regarding weighing.
;
;      Chris Torrence, RSI, Jan,June 2000.
;         - Fixed bug: if A only had 1 term, it was passed to user procedure
;           as an array. Now ensure it is a scalar.
;         - Added more info to error messages.
;         - Added /DOUBLE keyword.
;      CT, RSI, Nov 2001: If Weights is undefined, then assume no weighting,
;           and boost the Sigma error estimates according to NR Sec 15.2
;           Added YERROR keyword.
;      CT, RSI, May 2003: Added STATUS keyword.
;	        Added FITA keyword (courtesy B. LaBonte)
;
;-
FUNCTION CURVEFIT, x, y, weightsIn, a, sigma, FUNCTION_NAME = Function_Name, $
    FITA=fita, $
    ITMAX=itmax, ITER=iter, TOL=tol, CHI2=chi2, $
    NODERIVATIVE=noderivative, CHISQ=chisq, $
    DOUBLE=double, YERROR=yerror, $
    STATUS=status

    COMPILE_OPT strictarr

       ON_ERROR,2              ;Return to caller IF error

       ;Name of function to fit

       IF n_elements(function_name) LE 0 THEN function_name = "FUNCT"

       type = size(a,/type)
    double = (N_ELEMENTS(double) LT 1) ? (type EQ 5) : KEYWORD_SET(double)

    if ARG_PRESENT(status) then $
        status = 0   ; Assume success

    CASE double OF
    0: IF (type NE 4) THEN a = float(a)  ;Make params floating
    1: IF (type NE 5) THEN a = double(a)  ;Make params floating
    ENDCASE

    IF n_elements(tol) EQ 0 THEN tol = double ? 1d-3 : 1e-3  ;Convergence tol
    IF n_elements(itmax) EQ 0 THEN itmax = 20     ;Maximum # iterations

       ; IF we will be estimating partial derivatives THEN compute machine
       ; precision

       IF keyword_set(NODERIVATIVE) THEN BEGIN
          res = machar(DOUBLE=double)
          eps = sqrt(res.eps)
       ENDIF

    ;  Handle fixed parameters
	nterms = N_ELEMENTS(a)         ; # of parameters
	if (N_ELEMENTS(fita) ne nterms) then $
	    fita = REPLICATE(1, nterms)
	iparam = WHERE(fita, nparam)
	if (nparam eq 0) then $
	    MESSAGE, 'FITA must contain at least one parameter to fit.'

       nY = n_elements(y)
       nfree = nY - nparam ; Degrees of freedom

       IF nfree LE 0 THEN MESSAGE, $
        'Number of parameters in A must be less than number of dependent values in Y.'

       IF (nterms EQ 1) THEN a = a[0]   ; Ensure a is a scalar
       flambda = double ? 0.001d : 0.001                   ;Initial lambda
       diag = LINDGEN(nparam)*(nparam + 1) ; Subscripts of diagonal elements
       sigma = double ? DBLARR(nterms) : FLTARR(nterms)

;      Define the partial derivative array
       pder = double ? dblarr(nY, nterms) : fltarr(nY, nterms)

    noWeighting = N_ELEMENTS(weightsIn) eq 0
    weights = noWeighting ? REPLICATE(1.0, nY) : weightsIn

    error_msg1 = 'Result F from "'+ $
        Function_name+'" must have same number of elements as Y.'
    error_msg2 = 'PDER from "'+ $
        Function_name+'"  must be of size N_ELEMENTS(Y) by N_ELEMENTS(A).'

       FOR iter = 1, itmax DO BEGIN      ; Iteration loop

;         Evaluate alpha and beta matricies.

          IF keyword_set(NODERIVATIVE) THEN BEGIN

;            Evaluate function and estimate partial derivatives
            CALL_PROCEDURE, Function_name, x, a, yfit
            IF (N_ELEMENTS(yfit) NE nY) THEN MESSAGE, error_msg1
             FOR term=0, nterms-1 DO BEGIN

                p = a       ; Copy current parameters

                ; Increment size for forward difference derivative
                inc = eps * abs(p[term])
                IF (inc EQ 0.) THEN inc = eps
                p[term] = p[term] + inc
                CALL_PROCEDURE, function_name, x, p, yfit1
                pder[0,term] = (yfit1-yfit)/inc

             ENDFOR
          ENDIF ELSE BEGIN

             ; The user's procedure will return partial derivatives
             call_procedure, function_name, x, a, yfit, pder
            IF (N_ELEMENTS(yfit) NE nY) THEN MESSAGE, error_msg1

          ENDELSE

          IF nterms EQ 1 THEN pder = reform(pder, n_elements(y), 1)
            IF (NOT ARRAY_EQUAL(SIZE(pder,/DIM),[nY,nterms])) THEN $
                MESSAGE, error_msg2

          beta = (nparam eq 1) ? [TOTAL((y-yfit)*Weights*pder[*,iparam])] : $
            (y-yfit)*Weights # pder[*,iparam]
          alpha = TRANSPOSE(pder[*,iparam]) # $
            (Weights # (FLTARR(nparam)+1)*pder)

          ; save current values of return parameters
          sigma1 = sqrt( 1.0 / alpha[diag] )           ; Current sigma.
          sigma[iparam]  = sigma1

          chisq1 = total(Weights*(y-yfit)^2)/nfree     ; Current chi squared.
          chisq = chisq1

          yfit1 = yfit

          done_early = chisq1 LT total(abs(y))/1d7/nfree
          IF done_early THEN GOTO, done

          c = sqrt(alpha[diag])
          c = c # c

          lambdaCount = 0

          REPEAT BEGIN

             lambdaCount = lambdaCount + 1

             ; Normalize alpha to have unit diagonal.

             array = alpha / c

             ; Augment the diagonal.

             array[diag] = array[diag]*(1.+flambda)

             ; Invert modified curvature matrix to find new parameters.

             IF n_elements(array) EQ 1 THEN array = (1.0 / array) $
             ELSE array = invert(array)

             b = a
             b[iparam] = a[iparam] + array/c # transpose(beta) ; New params
             IF (nterms EQ 1) THEN b = b[0]             ; Ensure b is a scalar

             call_procedure, function_name, x, b, yfit  ; Evaluate function
             chisq = total(Weights*(y-yfit)^2)/nfree    ; New chisq
             sigma[iparam] = SQRT(array[diag]/alpha[diag])      ; New sigma

             IF (finite(chisq) EQ 0) OR $
                  (lambdaCount GT 30 AND chisq GE chisq1) THEN BEGIN

                ; Reject changes made this iteration, use old values.

                yfit  = yfit1
                sigma[iparam] = sigma1
                chisq = chisq1

                if ARG_PRESENT(status) then begin
                    status = 1
                endif else begin
                    MESSAGE,'Failed to converge- CHISQ increasing without bound.', $
                       /INFORMATIONAL
                endelse

                GOTO, done

             ENDIF

             flambda = flambda*10.               ; Assume fit got worse

          ENDREP UNTIL chisq LE chisq1

          flambda = flambda/100.

          a=b                                    ; Save new parameter estimate.

          IF ((chisq1-chisq)/chisq1) LE tol THEN GOTO,done   ;Finished?
       ENDFOR                        ;iteration loop

        if ARG_PRESENT(status) then begin
            status = 2
        endif else begin
            iterationStr = STRTRIM(itmax,2)+' iteration' + (['','s'])[itmax NE 1]
            MESSAGE, 'Failed to converge after '+iterationStr+'.', $
                /INFORMATIONAL
        endelse

done:

    ; If no weighting, then we need to boost error estimates by sqrt(chisq).
    ; See Numerical Recipes section 15.2 for details.
    if noWeighting then begin
        sigma *= SQRT(chisq)
    endif

    ; Experimental variance estimate, unbiased
    var = (nY GT nterms) ? TOTAL((yfit-y)^2 )/nfree : (dbl ? 0d : 0.0)
    yerror = SQRT(var)

    chi2 = chisq         ; Return chi-squared (chi2 obsolete-still works)
    if done_early then $
        iter--

    return,yfit          ; return result
END
