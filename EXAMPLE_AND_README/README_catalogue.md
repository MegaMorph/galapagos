This is a brief README file that explains the meanings of the output columns (as of version 2.1.5).  
Previous versions had additional columns, but as all those values were duplicates of the ones present in this catalogue, they have now been removed.  
Parameters have also been re-ordered, in order to make the catalogue more readable and understandable.

All **\*BAND** and all **\*CHEB** parameters are ARRAYS (!) with the same dimensions as bands used during the fit.  
**BAND** values are in the same order as bands were defined in the file given in A00) in the setup file, ignoring the SEXtractor band. E.g. in a ugriz setup where th r-band is used as the 'main' band and defined in the second line in A00), the order would be rugiz.  
**CHEB** values are polynomial values and are strictly ordered with order of the polynomial. They can be used to derive th Chebishev polynomial in between the 'band' values, e.g. to read off restframe values. DANGEROUS to use in case of high degrees of freedom (as desirable e.g for magnitudes) or when values are held fixed during the fit (e.g. sky values)

Error bars are values as derived by GALFIT, e.g. might be underestimating the TRUE uncertainty of the parameters (Haeussler et al, 2007)

Other parameters are scalar parameters, unless indicated otherwise.

 ---
 ### General Parameters

| PARAMETER           | TYPE (Integer, float,..)     | EXPLANATION  |
| ------------------  |----------- |-------|
| **GALAPAGOS PARAMETERS** | | |
| GALAPAGOS*_*VERSION           | STRING | The galapagos version used to create this catalogue |
| GALA*_*ID                     | STRING | A galaxy identifier, consists of tile and SExtractor number, e.g. t15_12.101.
| | | this can be used to find files matching certain objects  |
| **SExtractor Parameters** | | |
| NUMBER                        | LONG   | SExtractor number of the object, useful for identification |
| X*_*IMAGE                     | DOUBLE | Position X on the SExtractor image |
| Y*_*IMAGE                     | DOUBLE | Position Y on the SExtractor image |
| CXX*_*IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| CYY*_*IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| CXY*_*IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| THETA*_*IMAGE                 | DOUBLE | Position Angle against the image, as given by SExtractor |
| THETA*_*WORLD                 | DOUBLE | Position Angle against the WCS, as given by SExtractor | |
| ELLIPTICITY                   | DOUBLE | Object ellipticity as estimated by SExtractor |
| KRON*_*RADIUS                 | DOUBLE | Kron radius as estimated by SExtractor, used to derive starting values for GALFIT |
| A*_*IMAGE                     | DOUBLE | A*_*IMAGE as given by SExtractor (semi major axis in combination with KRON*_*RADIUS |
| B*_*IMAGE                     | DOUBLE | B*_*IMAGE as given by SExtractor (semi minor axis in combination with KRON*_*RADIUS |
| ALPHA*_*J2000                 | DOUBLE | Object Right Ascension as given by SExtractor |
| DELTA*_*J2000                 | DOUBLE | Object Declination as given by SExtractor |
| BACKGROUND                    | DOUBLE | Local background as given by SExtractor |
| FLUX*_*BEST                   | DOUBLE | Flux*_*Best as given by SExtractor |
| FLUXERR*_*BEST                | DOUBLE | Error on FLUX*_*BEST |
| MAG*_*BEST                    | DOUBLE | MAG*_*Best as given by SExtractor |
| MAGERR*_*BEST                 | DOUBLE | Error on MAG*_*BEST |
| FLUX*_*RADIUS                 | DOUBLE | Halflight radius as given by SExtractor|
| ISOAREA*_*IMAGE               | DOUBLE | Object area as given by SExtractor |
| FWHM*_*IMAGE                  | DOUBLE | Image FWHM as given by SExtractor |
| FLAGS                         | INT    | SExtractr flags (see SExtractor manual) |
| CLASS*_*STAR                  | DOUBLE | Star/Galaxy Classifier |
| **General Galapagos Parameters** | | |
| TILE                          | STRING | Image (Sextractor name) on which the object was selected to be fit |
| ORG*_*IMAGE*_*BAND            | STRING | Array of Image names for each band on which the object was selected to be fit  |
| SKY*_*GALA*_*BAND             | FLOAT  | Sky value in each band as derived by Galapagos |
| SKY*_*SIG*_*BAND              | FLOAT  | Sky uncertainty as derived by Galapagos (not used in the code, can be used for further test/analysis |
| SKY*_*RAD*_*BAND              | FLOAT  | Radius at which the sky has been determined [pixels] |
| SKY*_*FLAG*_*BAND             | FLOAT  | Flags for sky determination:  |
| |                                      | 0 - done successfully |
| |                                      | +1 - image too small for accurate sky calculation (source is too large) |
| |                                      | +2 - secondary contributing source (not subtracted) requires radius larger than image |
| |                                      | +4 - sky did not converge, still decreasing with radius at image boundary (image too small) |
| |                                      | +8 - not enough measurements, value from SExtractor taken |
| |                                      | +16 - value from contributing source taken |
| |                                      | +32 - image is masked entirely, no sky determination was possible. Assuming 0 as sky value |


 ---
 ###Parameters for Single_Sersic Fits

| PARAMETER           | TYPE (Integer, float,..)     | EXPLANATION  |
| ------------------  |----------- |-------|
| **General Galfit Parameters** | | |
| GALFIT*_*VERSION              | STRING | GALFIT version used for fit |
| FILE*_*GALFIT                 | STRING | GALFIT output file, which includes all images, models, residuals and PSFs |
| INITFILE                      | STRING | GALFIT start file, for manual re-run and adaptation |
| CONSTRNT                      | STRING | GALFIT constraint file |
| PSF*_*GALFIT*_*BAND           | STRING | PSF images used for the fit (but copied into output file anyway) |
| FLAG*_*GALFIT                 | INT    | FLAG of GALFIT status: |
| |                                      | 0: fit has not been tried, most values == -99|
| |                                      | 1: fit has been started, but is either still active or crashed during the fit, most values == -99, but sky values etc should have been estimated, in order to be able to look for systematic faults|
| |                                      | 2: fit has been started and produced a successful fit result|
| FITSECT                       | STRING | Size of the fit section that the fit uses (in Galapagos use equivalent with postage stamp size) |
| CONVBOX                       | STRING | Size of GALFIT convolution box used for the fit |
| NGOOD*_*GALFIT*_*BAND         | FLOAT  | Number of goo pixels in each band |
| NMASK*_*GALFIT*_*BAND         | FLOAT  | Number of masked pixels in each band |
| NITER*_*GALFIT                | INT    | Number of Iterations that GALFIT needed to finish |
| NEIGH*_*GALFIT                | INT    | Number of neighbouring objects in the fit, irrespective whether free or fixed (e.g. some measure of object crowding).  |
| CHISQ*_*GALFIT                | FLOAT  | Chi^2 of the fit |
| NFREE*_*GALFIT                | LONG   | Number of free parameters during the fit |
| NFIX*_*GALFIT                 | LONG   | Number of fixed parameters during the fit (e.g. in neighbouring objects) |
| NDOF*_*GALFIT                 | LONG   | Degree of Freedom of the fit |
| CHI2NU*_*GALFIT               | FLOAT  | Reduced Chi^2 of the fit |
| FIRSTCON*_*GALFIT             | INT    | FIRST iteration which  violated a constraint |
| LASTCON*_*GALFIT              | INT    | LAST iteration that violated a constraints (e.g. dangerous if the last iteration before Galfit finished, however, depends on WHICH constraint has been violated and whether by primary or secondary objects) |
| CPUTIME*_*SETUP*_*GALFIT      | FLOAT  | CPU time spend on setting up the fit |
| CPUTIME*_*FIT*_*GALFIT        | FLOAT  | CPU time spend on the actual fit |
| CPUTIME*_*TOTAL*_*GALFIT      | FLOAT  | total CPU time used by GALFIT |
| **Galfit Fit Parameters** | | |
| SKY*_*GALFIT*_*BAND           | FLOAT  | Sky values as used by GALFIT for each band (should be identical with SKY*_*GALA*_*BAND) |
| SKY*_*GALFIT*_*CHEB           | FLOAT  | Chebishev parameters that describe the sky polynomial (dangerous to use as sjy values are held fixed during the fit, e.g. the polynomial has no real meaning in between the wavelengths of the input bands)|
| X*_*GALFIT*_*DEG              | INT    | Degree of freedom for X-position of primary source |
| |                                      | 0: fixed at input values |
| |                                      | 1: variable during fit, but with values constant with wavelength |
| |                                      | 2: variable during fit, linear with wavelength (e.g. 2 degrees of freedom in polynomial)|
| |                                      | 3: variable during fit, quadratic with wavelength (e.g. 3 degrees of freedom in polynomial)|
| |                                      | ... |
| X*_*GALFIT*_*BAND             | FLOAT  | X-Position in each band |
| XERR*_*GALFIT*_*BAND          | FLOAT  | Error on X-Position in each band |
| X*_*GALFIT*_*CHEB             | FLOAT  | Chebishev parameters for X-Position |
| XERR*_*GALFIT*_*CHEB          | FLOAT  | Errors on Chebishev parameters for X-Position |
| Y*_*GALFIT*_*DEG              | INT    | Degree of freedom for Y-position of primary source |
| Y*_*GALFIT*_*BAND             | FLOAT  | Y-Position in each band |
| YERR*_*GALFIT*_*BAND          | FLOAT  | Error on Y-Position in each band |
| Y*_*GALFIT*_*CHEB             | FLOAT  | Chebishev parameters for Y-Position |
| YERR*_*GALFIT*_*CHEB          | FLOAT  | Error on Chebishev parameters for Y-Position |
| MAG*_*GALFIT*_*DEG            | INT    | Degree of freedom for magnitude of primary source |
| MAG*_*GALFIT*_*BAND           | FLOAT  | Magnitude in each band |
| MAGERR*_*GALFIT*_*BAND        | FLOAT  | Error on Magnitude in each band |
| MAG*_*GALFIT*_*CHEB           | FLOAT  | Chebishev parameters for Magnitude |
| MAGERR*_*GALFIT*_*CHEB        | FLOAT  | Error on Chebishev parameters for  |
| RE*_*GALFIT*_*DEG             | INT    | Degree of freedom for re of primary source |
| RE*_*GALFIT*_*BAND            | FLOAT  | Re in each band |
| REERR*_*GALFIT*_*BAND         | FLOAT  | Error on re in each band |
| RE*_*GALFIT*_*CHEB            | FLOAT  | Chebishev parameters for re |
| REERR*_*GALFIT*_*CHEB         | FLOAT  | Error on Chebishev parameters for re |
| N*_*GALFIT*_*DEG              | INT    | Degree of freedom for sersic index of primary source |
| N*_*GALFIT*_*BAND             | FLOAT  | Sersic Index in each band |
| NERR*_*GALFIT*_*BAND          | FLOAT  | Error on Sersic Index in each band |
| N*_*GALFIT*_*CHEB             | FLOAT  | Chebishev parameters for Sersic Index  |
| NERR*_*GALFIT*_*CHEB          | FLOAT  | Error on Chebishev parameters for Sersic Index  |
| Q*_*GALFIT*_*DEG              | INT    | Degree of freedom for axis ratio of primary source |
| Q*_*GALFIT*_*BAND             | FLOAT  | Axis Ratio in each band |
| QERR*_*GALFIT*_*BAND          | FLOAT  | Error on Axis Ratio in each band |
| Q*_*GALFIT*_*CHEB             | FLOAT  | Chebishev parameters for Axis Ratio |
| QERR*_*GALFIT*_*CHEB          | FLOAT  | Error on Chebishev parameters for Axis Ratio |
| PA*_*GALFIT*_*DEG             | INT    | Degree of freedom for position angle of primary source |
| PA*_*GALFIT*_*BAND            | FLOAT  | Position Angle in each band |
| PAERR*_*GALFIT*_*BAND         | FLOAT  | Error on Position Angle in each band |
| PA*_*GALFIT*_*CHEB            | FLOAT  | Chebishev parameters for Position Angle |
| PAERR*_*GALFIT*_*CHEB         | FLOAT  | Error on Chebishev parameters for Position Angle |
  
 ---  
###Parameters for Bulge/Disk Decomposition Fits (only available when B/D has been switched on)
(missing parameters identical to Single-Sersic fit values)

| PARAMETER           | TYPE (Integer, float,..)     | EXPLANATION  |
| ------------------  |----------- |-------|
| **General Galfit Parameters** | | |
| GALFIT*_*VERSION*_*BD         | STRING | GALFIT version used for B/D fit |
| FILE*_*GALFIT*_*BD            | STRING | GALFIT output file for B/D fit, which includes all images, models, residuals and PSFs |
| INITFILE*_*BD                 | STRING | GALFIT start file for B/D fit, for manual re-run and adaptation |
| CONSTRNT*_*BD                 | STRING | GALFIT constraint file used in B/D fit |
| PSF*_*GALFIT*_*BAND*_*BD      | STRING | PSF images used for the B/D fit (but copied into output file anyway) |
| FLAG*_*GALFIT*_*BD            | INT    | FLAG of GALFIT status: |
| |                                      | 0: fit has not been tried, most values == -99|
| |                                      | 1: fit has been started, but is either still active or crashed during the fit, most values == -99, 
| NITER*_*GALFIT*_*BD           | INT    | Number of Iterations that GALFIT needed to finish B/D fit |
| NEIGH*_*GALFIT*_*BD           | INT    | Number of neighbouring objects in the fit in B/D fit |
| CHISQ*_*GALFIT*_*BD           | FLOAT  | Chi^2 of the B/D fit |
| NFREE*_*GALFIT*_*BD           | LONG   | Number of free parameters during the B/D fit |
| NFIX*_*GALFIT*_*BD            | LONG   | Number of fixed parameters during the B/D fit (e.g. in neighbouring objects)  |
| NDOF*_*GALFIT*_*BD            | LONG   | Degree of Freedom of the B/D fit |
| CHI2NU*_*GALFIT*_*BD          | FLOAT  | Reduced Chi^2 of the B/D fit |
| FIRSTCON*_*GALFIT*_*BD        | INT    | FIRST iteration which  violated a constraint |
| LASTCON*_*GALFIT*_*BD         | INT    | LAST iteration that violated a constraints (e.g. dangerous if the last iteration before Galfit finished, however, depends on WHICH constraint has been violated and whether by primary or secondary objects) |
| CPUTIME*_*SETUP*_*GALFIT*_*BD | FLOAT  | CPU time spend on setting up the B/D fit |
| CPUTIME*_*FIT*_*GALFIT*_*BD   | FLOAT  | CPU time spend on the actual B/D fit |
| CPUTIME*_*TOTAL*_*GALFIT*_*BD | FLOAT  | total CPU time used by GALFIT for B/D fit |
| **General Fit Parameters** | | |
| SKY*_*GALFIT*_*BAND*_*BD      | FLOAT  | Sky values as used by GALFIT for each band (should be identical with SKY*_*GALA*_*BAND) |
| SKY*_*GALFIT*_*CHEB*_*BD      | FLOAT  | Chebishev parameters that describe the sky polynomial (dangerous to use as sjy values are held fixed during the fit, e.g. the polynomial has no real meaning in between the wavelengths of the input bands) |
| **Galfit B/D Fit Parameters BULGE** | | |
| X*_*GALFIT*_*DEG*_*B          | INT    | Bulge: Degree of freedom for X-position of primary source |
| |                                      | 0: fixed at input values |
| |                                      | 1: variable during fit, but with values constant with wavelength |
| |                                      | 2: variable during fit, linear with wavelength (e.g. 2 degrees of freedom in polynomial)|
| |                                      | 3: variable during fit, quadratic with wavelength (e.g. 3 degrees of freedom in polynomial)|
| |                                      | ... |
| X*_*GALFIT*_*BAND*_*B         | FLOAT  | Bulge-X-Position in each band |
| XERR*_*GALFIT*_*BAND*_*B      | FLOAT  | Error on Bulge-X-Position in each band |
| X*_*GALFIT*_*CHEB*_*B         | FLOAT  | Chebishev parameters for Bulge-X-Position |
| XERR*_*GALFIT*_*CHEB*_*B      | FLOAT  | Errors on Chebishev parameters for Bulge-X-Position |
| Y*_*GALFIT*_*DEG*_*B          | INT    | Bulge: Degree of freedom for Y-position of primary source |
| Y*_*GALFIT*_*BAND*_*B         | FLOAT  | Bulge-Y-Position in each band |
| YERR*_*GALFIT*_*BAND*_*B      | FLOAT  | Error on Bulge-Y-Position in each band |
| Y*_*GALFIT*_*CHEB*_*B         | FLOAT  | Chebishev parameters for Bulge-Y-Position |
| YERR*_*GALFIT*_*CHEB*_*B      | FLOAT  | Error on Chebishev parameters for Bulge-Y-Position |
| MAG*_*GALFIT*_*DEG*_*B        | INT    | Bulge: Degree of freedom for magnitude of primary source |
| MAG*_*GALFIT*_*BAND*_*B       | FLOAT  | Bulge-Magnitude in each band |
| MAGERR*_*GALFIT*_*BAND*_*B    | FLOAT  | Error on Bulge-Magnitude in each band |
| MAG*_*GALFIT*_*CHEB*_*B       | FLOAT  | Chebishev parameters for Bulge-Magnitude |
| MAGERR*_*GALFIT*_*CHEB*_*B    | FLOAT  | Error on Chebishev parameters for Bulge-Magnitude |
| RE*_*GALFIT*_*DEG*_*B         | INT    | Bulge: Degree of freedom for re of primary source |
| RE*_*GALFIT*_*BAND*_*B        | FLOAT  | Bulge-Re in each band |
| REERR*_*GALFIT*_*BAND*_*B     | FLOAT  | Error on Bulge-re in each band |
| RE*_*GALFIT*_*CHEB*_*B        | FLOAT  | Chebishev parameters for Bulge-re |
| REERR*_*GALFIT*_*CHEB*_*B     | FLOAT  | Error on Chebishev parameters for Bulge-re |
| N*_*GALFIT*_*DEG*_*B          | INT    | Bulge: Degree of freedom for sersic index of primary source |
| N*_*GALFIT*_*BAND*_*B         | FLOAT  | Bulge-Sersic Index in each band |
| NERR*_*GALFIT*_*BAND*_*B      | FLOAT  | Error on Bulge-Sersic Index in each band |
| N*_*GALFIT*_*CHEB*_*B         | FLOAT  | Chebishev parameters for Bulge-Sersic Index |
| NERR*_*GALFIT*_*CHEB*_*B      | FLOAT  | Error on Chebishev parameters for Bulge-Sersic Index |
| Q*_*GALFIT*_*DEG*_*B          | INT    | Bulge: Degree of freedom for axis ratio of primary source |
| Q*_*GALFIT*_*BAND*_*B         | FLOAT  | Bulge-Axis Ratio in each band |
| QERR*_*GALFIT*_*BAND*_*B      | FLOAT  | Error on Bulge-Axis Ratio in each band |
| Q*_*GALFIT*_*CHEB*_*B         | FLOAT  | Chebishev parameters for Bulge-Axis Ratio |
| QERR*_*GALFIT*_*CHEB*_*B      | FLOAT  | Error on Chebishev parameters for Bulge-Axis Ratio |
| PA*_*GALFIT*_*DEG*_*B         | INT    | Bulge: Degree of freedom for position angle of primary source |
| PA*_*GALFIT*_*BAND*_*B        | FLOAT  | Bulge-Position Angle in each band |
| PAERR*_*GALFIT*_*BAND*_*B     | FLOAT  | Error on Bulge-Position Angle in each band |
| PA*_*GALFIT*_*CHEB*_*B        | FLOAT  | Chebishev parameters for Bulge-Position Angle |
| PAERR*_*GALFIT*_*CHEB*_*B     | FLOAT  | Error on Chebishev parameters for Bulge-Position Angle |
| **Galfit B/D Fit Parameters DISK** | | |
| X*_*GALFIT*_*BAND*_*D         | FLOAT  | Disk-X-Position in each band |
| XERR*_*GALFIT*_*BAND*_*D      | FLOAT  | Error on Disk-X-Position in each band |
| X*_*GALFIT*_*CHEB*_*D         | FLOAT  | Chebishev parameters for Disk-X-Position |
| XERR*_*GALFIT*_*CHEB*_*D      | FLOAT  | Errors on Chebishev parameters for Disk-X-Position |
| Y*_*GALFIT*_*DEG*_*D          | INT    | Disk: Degree of freedom for Y-position of primary source |
| Y*_*GALFIT*_*BAND*_*D         | FLOAT  | Disk-Y-Position in each band |
| YERR*_*GALFIT*_*BAND*_*D      | FLOAT  | Error on Disk-Y-Position in each band |
| Y*_*GALFIT*_*CHEB*_*D         | FLOAT  | Chebishev parameters for Disk-Y-Position |
| YERR*_*GALFIT*_*CHEB*_*D      | FLOAT  | Error on Chebishev parameters for Disk-Y-Position |
| MAG*_*GALFIT*_*DEG*_*D        | INT    | Disk: Degree of freedom for magnitude of primary source |
| MAG*_*GALFIT*_*BAND*_*D       | FLOAT  | Disk-Magnitude in each band |
| MAGERR*_*GALFIT*_*BAND*_*D    | FLOAT  | Error on Disk-Magnitude in each band |
| MAG*_*GALFIT*_*CHEB*_*D       | FLOAT  | Chebishev parameters for Disk-Magnitude |
| MAGERR*_*GALFIT*_*CHEB*_*D    | FLOAT  | Error on Chebishev parameters for Disk-Magnitude |
| RE*_*GALFIT*_*DEG*_*D         | INT    | Disk: Degree of freedom for re of primary source |
| RE*_*GALFIT*_*BAND*_*D        | FLOAT  | Disk-Re in each band |
| REERR*_*GALFIT*_*BAND*_*D     | FLOAT  | Error on Disk-re in each band |
| RE*_*GALFIT*_*CHEB*_*D        | FLOAT  | Chebishev parameters for Disk-re |
| REERR*_*GALFIT*_*CHEB*_*D     | FLOAT  | Error on Chebishev parameters for Disk-re |
| N*_*GALFIT*_*DEG*_*D          | INT    | Disk: Degree of freedom for sersic index of primary source |
| N*_*GALFIT*_*BAND*_*D         | FLOAT  | Disk-Sersic Index in each band |
| NERR*_*GALFIT*_*BAND*_*D      | FLOAT  | Error on Disk-Sersic Index in each band |
| N*_*GALFIT*_*CHEB*_*D         | FLOAT  | Chebishev parameters for Disk-Sersic Index |
| NERR*_*GALFIT*_*CHEB*_*D      | FLOAT  | Error on Chebishev parameters for Disk-Sersic Index |
| Q*_*GALFIT*_*DEG*_*D          | INT    | Disk: Degree of freedom for axis ratio of primary source |
| Q*_*GALFIT*_*BAND*_*D         | FLOAT  | Disk-Axis Ratio in each band |
| QERR*_*GALFIT*_*BAND*_*D      | FLOAT  | Error on Disk-Axis Ratio in each band |
| Q*_*GALFIT*_*CHEB*_*D         | FLOAT  | Chebishev parameters for Disk-Axis Ratio |
| QERR*_*GALFIT*_*CHEB*_*D      | FLOAT  | Error on Chebishev parameters for Disk-Axis Ratio |
| PA*_*GALFIT*_*DEG*_*D         | INT    | Disk: Degree of freedom for position angle of primary source |
| PA*_*GALFIT*_*BAND*_*D        | FLOAT  | Disk-Position Angle in each band |
| PAERR*_*GALFIT*_*BAND*_*D     | FLOAT  | Error on Disk-Position Angle in each band |
| PA*_*GALFIT*_*CHEB*_*D        | FLOAT  | Chebishev parameters for Disk-Position Angle |
| PAERR*_*GALFIT*_*CHEB*_*D     | FLOAT  | Error on Chebishev parameters for Disk-Position Angle |
