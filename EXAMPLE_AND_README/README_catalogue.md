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
| GALAPAGOS_VERSION           | STRING | The galapagos version used to create this catalogue |
| GALA_ID                     | STRING | A galaxy identifier, consists of tile and SExtractor number, e.g. t15_12.101.
| | | this can be used to find files matching certain objects  |
| **SExtractor Parameters** | | |
| NUMBER                        | LONG   | SExtractor number of the object, useful for identification |
| X_IMAGE                     | DOUBLE | Position X on the SExtractor image |
| Y_IMAGE                     | DOUBLE | Position Y on the SExtractor image |
| CXX_IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| CYY_IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| CXY_IMAGE                   | DOUBLE | Ellipse parameters by SExtractor, used internally |
| THETA_IMAGE                 | DOUBLE | Position Angle against the image, as given by SExtractor |
| THETA_WORLD                 | DOUBLE | Position Angle against the WCS, as given by SExtractor | |
| ELLIPTICITY                   | DOUBLE | Object ellipticity as estimated by SExtractor |
| KRON_RADIUS                 | DOUBLE | Kron radius as estimated by SExtractor, used to derive starting values for GALFIT |
| A_IMAGE                     | DOUBLE | A_IMAGE as given by SExtractor (semi major axis in combination with KRON_RADIUS |
| B_IMAGE                     | DOUBLE | B_IMAGE as given by SExtractor (semi minor axis in combination with KRON_RADIUS |
| ALPHA_J2000                 | DOUBLE | Object Right Ascension as given by SExtractor |
| DELTA_J2000                 | DOUBLE | Object Declination as given by SExtractor |
| BACKGROUND                    | DOUBLE | Local background as given by SExtractor |
| FLUX_BEST                   | DOUBLE | Flux_Best as given by SExtractor |
| FLUXERR_BEST                | DOUBLE | Error on FLUX_BEST |
| MAG_BEST                    | DOUBLE | MAG_Best as given by SExtractor |
| MAGERR_BEST                 | DOUBLE | Error on MAG_BEST |
| FLUX_RADIUS                 | DOUBLE | Halflight radius as given by SExtractor|
| ISOAREA_IMAGE               | DOUBLE | Object area as given by SExtractor |
| FWHM_IMAGE                  | DOUBLE | Image FWHM as given by SExtractor |
| FLAGS                         | INT    | SExtractr flags (see SExtractor manual) |
| CLASS_STAR                  | DOUBLE | Star/Galaxy Classifier |
| **General Galapagos Parameters** | | |
| TILE                          | STRING | Image (Sextractor name) on which the object was selected to be fit |
| ORG_IMAGE_BAND            | STRING | Array of Image names for each band on which the object was selected to be fit  |
| SKY_GALA_BAND             | FLOAT  | Sky value in each band as derived by Galapagos |
| SKY_SIG_BAND              | FLOAT  | Sky uncertainty as derived by Galapagos (not used in the code, can be used for further test/analysis |
| SKY_RAD_BAND              | FLOAT  | Radius at which the sky has been determined [pixels] |
| SKY_FLAG_BAND             | FLOAT  | Flags for sky determination:  |
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
| GALFIT_VERSION              | STRING | GALFIT version used for fit |
| FILE_GALFIT                 | STRING | GALFIT output file, which includes all images, models, residuals and PSFs |
| INITFILE                      | STRING | GALFIT start file, for manual re-run and adaptation |
| CONSTRNT                      | STRING | GALFIT constraint file |
| PSF_GALFIT_BAND           | STRING | PSF images used for the fit (but copied into output file anyway) |
| FLAG_GALFIT                 | INT    | FLAG of GALFIT status: |
| |                                      | 0: fit has not been tried, most values == -99|
| |                                      | 1: fit has been started, but is either still active or crashed during the fit, most values == -99, but sky values etc should have been estimated, in order to be able to look for systematic faults|
| |                                      | 2: fit has been started and produced a successful fit result|
| FITSECT                       | STRING | Size of the fit section that the fit uses (in Galapagos use equivalent with postage stamp size) |
| CONVBOX                       | STRING | Size of GALFIT convolution box used for the fit |
| NGOOD_GALFIT_BAND         | FLOAT  | Number of goo pixels in each band |
| NMASK_GALFIT_BAND         | FLOAT  | Number of masked pixels in each band |
| NITER_GALFIT                | INT    | Number of Iterations that GALFIT needed to finish |
| NEIGH_GALFIT                | INT    | Number of neighbouring objects in the fit, irrespective whether free or fixed (e.g. some measure of object crowding).  |
| CHISQ_GALFIT                | FLOAT  | Chi^2 of the fit |
| NFREE_GALFIT                | LONG   | Number of free parameters during the fit |
| NFIX_GALFIT                 | LONG   | Number of fixed parameters during the fit (e.g. in neighbouring objects) |
| NDOF_GALFIT                 | LONG   | Degree of Freedom of the fit |
| CHI2NU_GALFIT               | FLOAT  | Reduced Chi^2 of the fit |
| FIRSTCON_GALFIT             | INT    | FIRST iteration which  violated a constraint |
| LASTCON_GALFIT              | INT    | LAST iteration that violated a constraints (e.g. dangerous if the last iteration before Galfit finished, however, depends on WHICH constraint has been violated and whether by primary or secondary objects) |
| CPUTIME_SETUP_GALFIT      | FLOAT  | CPU time spend on setting up the fit |
| CPUTIME_FIT_GALFIT        | FLOAT  | CPU time spend on the actual fit |
| CPUTIME_TOTAL_GALFIT      | FLOAT  | total CPU time used by GALFIT |
| **Galfit Fit Parameters** | | |
| SKY_GALFIT_BAND           | FLOAT  | Sky values as used by GALFIT for each band (should be identical with SKY_GALA_BAND) |
| SKY_GALFIT_CHEB           | FLOAT  | Chebishev parameters that describe the sky polynomial (dangerous to use as sjy values are held fixed during the fit, e.g. the polynomial has no real meaning in between the wavelengths of the input bands)|
| X_GALFIT_DEG              | INT    | Degree of freedom for X-position of primary source |
| |                                      | 0: fixed at input values |
| |                                      | 1: variable during fit, but with values constant with wavelength |
| |                                      | 2: variable during fit, linear with wavelength (e.g. 2 degrees of freedom in polynomial)|
| |                                      | 3: variable during fit, quadratic with wavelength (e.g. 3 degrees of freedom in polynomial)|
| |                                      | ... |
| X_GALFIT_BAND             | FLOAT  | X-Position in each band |
| XERR_GALFIT_BAND          | FLOAT  | Error on X-Position in each band |
| X_GALFIT_CHEB             | FLOAT  | Chebishev parameters for X-Position |
| XERR_GALFIT_CHEB          | FLOAT  | Errors on Chebishev parameters for X-Position |
| Y_GALFIT_DEG              | INT    | Degree of freedom for Y-position of primary source |
| Y_GALFIT_BAND             | FLOAT  | Y-Position in each band |
| YERR_GALFIT_BAND          | FLOAT  | Error on Y-Position in each band |
| Y_GALFIT_CHEB             | FLOAT  | Chebishev parameters for Y-Position |
| YERR_GALFIT_CHEB          | FLOAT  | Error on Chebishev parameters for Y-Position |
| MAG_GALFIT_DEG            | INT    | Degree of freedom for magnitude of primary source |
| MAG_GALFIT_BAND           | FLOAT  | Magnitude in each band |
| MAGERR_GALFIT_BAND        | FLOAT  | Error on Magnitude in each band |
| MAG_GALFIT_CHEB           | FLOAT  | Chebishev parameters for Magnitude |
| MAGERR_GALFIT_CHEB        | FLOAT  | Error on Chebishev parameters for  |
| RE_GALFIT_DEG             | INT    | Degree of freedom for re of primary source |
| RE_GALFIT_BAND            | FLOAT  | Re in each band |
| REERR_GALFIT_BAND         | FLOAT  | Error on re in each band |
| RE_GALFIT_CHEB            | FLOAT  | Chebishev parameters for re |
| REERR_GALFIT_CHEB         | FLOAT  | Error on Chebishev parameters for re |
| N_GALFIT_DEG              | INT    | Degree of freedom for sersic index of primary source |
| N_GALFIT_BAND             | FLOAT  | Sersic Index in each band |
| NERR_GALFIT_BAND          | FLOAT  | Error on Sersic Index in each band |
| N_GALFIT_CHEB             | FLOAT  | Chebishev parameters for Sersic Index  |
| NERR_GALFIT_CHEB          | FLOAT  | Error on Chebishev parameters for Sersic Index  |
| Q_GALFIT_DEG              | INT    | Degree of freedom for axis ratio of primary source |
| Q_GALFIT_BAND             | FLOAT  | Axis Ratio in each band |
| QERR_GALFIT_BAND          | FLOAT  | Error on Axis Ratio in each band |
| Q_GALFIT_CHEB             | FLOAT  | Chebishev parameters for Axis Ratio |
| QERR_GALFIT_CHEB          | FLOAT  | Error on Chebishev parameters for Axis Ratio |
| PA_GALFIT_DEG             | INT    | Degree of freedom for position angle of primary source |
| PA_GALFIT_BAND            | FLOAT  | Position Angle in each band |
| PAERR_GALFIT_BAND         | FLOAT  | Error on Position Angle in each band |
| PA_GALFIT_CHEB            | FLOAT  | Chebishev parameters for Position Angle |
| PAERR_GALFIT_CHEB         | FLOAT  | Error on Chebishev parameters for Position Angle |
  
 ---  
###Parameters for Bulge/Disk Decomposition Fits (only available when B/D has been switched on)
(missing parameters identical to Single-Sersic fit values)

| PARAMETER           | TYPE (Integer, float,..)     | EXPLANATION  |
| ------------------  |----------- |-------|
| **General Galfit Parameters** | | |
| GALFIT_VERSION_BD         | STRING | GALFIT version used for B/D fit |
| FILE_GALFIT_BD            | STRING | GALFIT output file for B/D fit, which includes all images, models, residuals and PSFs |
| INITFILE_BD                 | STRING | GALFIT start file for B/D fit, for manual re-run and adaptation |
| CONSTRNT_BD                 | STRING | GALFIT constraint file used in B/D fit |
| PSF_GALFIT_BAND_BD      | STRING | PSF images used for the B/D fit (but copied into output file anyway) |
| FLAG_GALFIT_BD            | INT    | FLAG of GALFIT status: |
| |                                      | 0: fit has not been tried, most values == -99|
| |                                      | 1: fit has been started, but is either still active or crashed during the fit, most values == -99, 
| NITER_GALFIT_BD           | INT    | Number of Iterations that GALFIT needed to finish B/D fit |
| NEIGH_GALFIT_BD           | INT    | Number of neighbouring objects in the fit in B/D fit |
| CHISQ_GALFIT_BD           | FLOAT  | Chi^2 of the B/D fit |
| NFREE_GALFIT_BD           | LONG   | Number of free parameters during the B/D fit |
| NFIX_GALFIT_BD            | LONG   | Number of fixed parameters during the B/D fit (e.g. in neighbouring objects)  |
| NDOF_GALFIT_BD            | LONG   | Degree of Freedom of the B/D fit |
| CHI2NU_GALFIT_BD          | FLOAT  | Reduced Chi^2 of the B/D fit |
| FIRSTCON_GALFIT_BD        | INT    | FIRST iteration which  violated a constraint |
| LASTCON_GALFIT_BD         | INT    | LAST iteration that violated a constraints (e.g. dangerous if the last iteration before Galfit finished, however, depends on WHICH constraint has been violated and whether by primary or secondary objects) |
| CPUTIME_SETUP_GALFIT_BD | FLOAT  | CPU time spend on setting up the B/D fit |
| CPUTIME_FIT_GALFIT_BD   | FLOAT  | CPU time spend on the actual B/D fit |
| CPUTIME_TOTAL_GALFIT_BD | FLOAT  | total CPU time used by GALFIT for B/D fit |
| **General Fit Parameters** | | |
| SKY_GALFIT_BAND_BD      | FLOAT  | Sky values as used by GALFIT for each band (should be identical with SKY_GALA_BAND) |
| SKY_GALFIT_CHEB_BD      | FLOAT  | Chebishev parameters that describe the sky polynomial (dangerous to use as sjy values are held fixed during the fit, e.g. the polynomial has no real meaning in between the wavelengths of the input bands) |
| **Galfit B/D Fit Parameters BULGE** | | |
| X_GALFIT_DEG_B          | INT    | Bulge: Degree of freedom for X-position of primary source |
| |                                      | 0: fixed at input values |
| |                                      | 1: variable during fit, but with values constant with wavelength |
| |                                      | 2: variable during fit, linear with wavelength (e.g. 2 degrees of freedom in polynomial)|
| |                                      | 3: variable during fit, quadratic with wavelength (e.g. 3 degrees of freedom in polynomial)|
| |                                      | ... |
| X_GALFIT_BAND_B         | FLOAT  | Bulge-X-Position in each band |
| XERR_GALFIT_BAND_B      | FLOAT  | Error on Bulge-X-Position in each band |
| X_GALFIT_CHEB_B         | FLOAT  | Chebishev parameters for Bulge-X-Position |
| XERR_GALFIT_CHEB_B      | FLOAT  | Errors on Chebishev parameters for Bulge-X-Position |
| Y_GALFIT_DEG_B          | INT    | Bulge: Degree of freedom for Y-position of primary source |
| Y_GALFIT_BAND_B         | FLOAT  | Bulge-Y-Position in each band |
| YERR_GALFIT_BAND_B      | FLOAT  | Error on Bulge-Y-Position in each band |
| Y_GALFIT_CHEB_B         | FLOAT  | Chebishev parameters for Bulge-Y-Position |
| YERR_GALFIT_CHEB_B      | FLOAT  | Error on Chebishev parameters for Bulge-Y-Position |
| MAG_GALFIT_DEG_B        | INT    | Bulge: Degree of freedom for magnitude of primary source |
| MAG_GALFIT_BAND_B       | FLOAT  | Bulge-Magnitude in each band |
| MAGERR_GALFIT_BAND_B    | FLOAT  | Error on Bulge-Magnitude in each band |
| MAG_GALFIT_CHEB_B       | FLOAT  | Chebishev parameters for Bulge-Magnitude |
| MAGERR_GALFIT_CHEB_B    | FLOAT  | Error on Chebishev parameters for Bulge-Magnitude |
| RE_GALFIT_DEG_B         | INT    | Bulge: Degree of freedom for re of primary source |
| RE_GALFIT_BAND_B        | FLOAT  | Bulge-Re in each band |
| REERR_GALFIT_BAND_B     | FLOAT  | Error on Bulge-re in each band |
| RE_GALFIT_CHEB_B        | FLOAT  | Chebishev parameters for Bulge-re |
| REERR_GALFIT_CHEB_B     | FLOAT  | Error on Chebishev parameters for Bulge-re |
| N_GALFIT_DEG_B          | INT    | Bulge: Degree of freedom for sersic index of primary source |
| N_GALFIT_BAND_B         | FLOAT  | Bulge-Sersic Index in each band |
| NERR_GALFIT_BAND_B      | FLOAT  | Error on Bulge-Sersic Index in each band |
| N_GALFIT_CHEB_B         | FLOAT  | Chebishev parameters for Bulge-Sersic Index |
| NERR_GALFIT_CHEB_B      | FLOAT  | Error on Chebishev parameters for Bulge-Sersic Index |
| Q_GALFIT_DEG_B          | INT    | Bulge: Degree of freedom for axis ratio of primary source |
| Q_GALFIT_BAND_B         | FLOAT  | Bulge-Axis Ratio in each band |
| QERR_GALFIT_BAND_B      | FLOAT  | Error on Bulge-Axis Ratio in each band |
| Q_GALFIT_CHEB_B         | FLOAT  | Chebishev parameters for Bulge-Axis Ratio |
| QERR_GALFIT_CHEB_B      | FLOAT  | Error on Chebishev parameters for Bulge-Axis Ratio |
| PA_GALFIT_DEG_B         | INT    | Bulge: Degree of freedom for position angle of primary source |
| PA_GALFIT_BAND_B        | FLOAT  | Bulge-Position Angle in each band |
| PAERR_GALFIT_BAND_B     | FLOAT  | Error on Bulge-Position Angle in each band |
| PA_GALFIT_CHEB_B        | FLOAT  | Chebishev parameters for Bulge-Position Angle |
| PAERR_GALFIT_CHEB_B     | FLOAT  | Error on Chebishev parameters for Bulge-Position Angle |
| **Galfit B/D Fit Parameters DISK** | | |
| X_GALFIT_BAND_D         | FLOAT  | Disk-X-Position in each band |
| XERR_GALFIT_BAND_D      | FLOAT  | Error on Disk-X-Position in each band |
| X_GALFIT_CHEB_D         | FLOAT  | Chebishev parameters for Disk-X-Position |
| XERR_GALFIT_CHEB_D      | FLOAT  | Errors on Chebishev parameters for Disk-X-Position |
| Y_GALFIT_DEG_D          | INT    | Disk: Degree of freedom for Y-position of primary source |
| Y_GALFIT_BAND_D         | FLOAT  | Disk-Y-Position in each band |
| YERR_GALFIT_BAND_D      | FLOAT  | Error on Disk-Y-Position in each band |
| Y_GALFIT_CHEB_D         | FLOAT  | Chebishev parameters for Disk-Y-Position |
| YERR_GALFIT_CHEB_D      | FLOAT  | Error on Chebishev parameters for Disk-Y-Position |
| MAG_GALFIT_DEG_D        | INT    | Disk: Degree of freedom for magnitude of primary source |
| MAG_GALFIT_BAND_D       | FLOAT  | Disk-Magnitude in each band |
| MAGERR_GALFIT_BAND_D    | FLOAT  | Error on Disk-Magnitude in each band |
| MAG_GALFIT_CHEB_D       | FLOAT  | Chebishev parameters for Disk-Magnitude |
| MAGERR_GALFIT_CHEB_D    | FLOAT  | Error on Chebishev parameters for Disk-Magnitude |
| RE_GALFIT_DEG_D         | INT    | Disk: Degree of freedom for re of primary source |
| RE_GALFIT_BAND_D        | FLOAT  | Disk-Re in each band |
| REERR_GALFIT_BAND_D     | FLOAT  | Error on Disk-re in each band |
| RE_GALFIT_CHEB_D        | FLOAT  | Chebishev parameters for Disk-re |
| REERR_GALFIT_CHEB_D     | FLOAT  | Error on Chebishev parameters for Disk-re |
| N_GALFIT_DEG_D          | INT    | Disk: Degree of freedom for sersic index of primary source |
| N_GALFIT_BAND_D         | FLOAT  | Disk-Sersic Index in each band |
| NERR_GALFIT_BAND_D      | FLOAT  | Error on Disk-Sersic Index in each band |
| N_GALFIT_CHEB_D         | FLOAT  | Chebishev parameters for Disk-Sersic Index |
| NERR_GALFIT_CHEB_D      | FLOAT  | Error on Chebishev parameters for Disk-Sersic Index |
| Q_GALFIT_DEG_D          | INT    | Disk: Degree of freedom for axis ratio of primary source |
| Q_GALFIT_BAND_D         | FLOAT  | Disk-Axis Ratio in each band |
| QERR_GALFIT_BAND_D      | FLOAT  | Error on Disk-Axis Ratio in each band |
| Q_GALFIT_CHEB_D         | FLOAT  | Chebishev parameters for Disk-Axis Ratio |
| QERR_GALFIT_CHEB_D      | FLOAT  | Error on Chebishev parameters for Disk-Axis Ratio |
| PA_GALFIT_DEG_D         | INT    | Disk: Degree of freedom for position angle of primary source |
| PA_GALFIT_BAND_D        | FLOAT  | Disk-Position Angle in each band |
| PAERR_GALFIT_BAND_D     | FLOAT  | Error on Disk-Position Angle in each band |
| PA_GALFIT_CHEB_D        | FLOAT  | Chebishev parameters for Disk-Position Angle |
| PAERR_GALFIT_CHEB_D     | FLOAT  | Error on Chebishev parameters for Disk-Position Angle |
