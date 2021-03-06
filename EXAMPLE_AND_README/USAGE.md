##GALAPAGOS-2 README (v.2.2.1)

Please read this file for details on input format and new features off the code.  
Especially read this file regarding your image normalisation and how to set up and run galapagos in the most efficient way.  
Please note that this code (all versions of Galapagos-2) is NOT (!) backwards compatible to old Galapagos input files, please make sure you adapt your files appropriately.

In everything not explained here, the code and data setup should follow Barden et al. 2012.  
Please contact us in case of problems or questions at BorisHaeussler.astro@gmail.com  
(Website with FAQ and further explanations in preparation)  
The MegaMorph projects also has a Google+ community for questions and requests:  
https://plus.google.com/u/0/communities/101731295964036445086  
This is also where new versions of the code will be released and announced.

If you have a single-band survey only, and do not require the multi band capabilities of Galapagos-2, you can still use Galapagos-2 for single-band fits.  
However, there is a new Galapagos (one-band) version, coded in C, which can be run on big clusters, so is over all significantly faster.  
Please contact Andreas Hiemer (andreas.hiemer@uibk.ac.at) for this C-version with several additional features  (e.g. fitting Fourier modes), but missing some other features, e.g. variable PSFs 

 ---
###CONTENT OF THIS FILE:  
A) New Features  
B) Most important tips as they cause the most problems  
C) Setup file and explanations in detail. Also ellains internal changed within the cose for the user to be aware of.
D) Advised way to run GALAPAGAOS-2 in a stable fashion (using 'screen')
E) Advised scheme when running GALAPAGOS-2 on large datasets
F) Advide when re-running GALAPAGOS-2, e.g. after server crash or after you have killed it
G) Version history   

 ---
####SEE END OF THIS FILE FOR TIPS ON HOW TO RUN GALAPAGOS!!  
- screened, e.g. stable against connection loss  
- running on large samples, using several computers   
- what you have to do after you killed galapagos or after it crashed

 ---
####SEE END OF THIS FILE FOR VERSION HISTORY!!  
Make sure you check the version history when you are transferring from an older version.  
The input files might have changed (e.g. v2.0.7, v2.1.0) compared to what you are used to in order to accommodate new features
 
####A) Major new features in GALAPAGOS-2 (compared to original galapagos)
- Galapagos can now use multi-band data simultaneously  
- Galapagos can now do Bulge-Disk decomposition
- Galapagos can now deal with more than one PSF for a survey, making it more usable for ground-based surveys
- Galapagos can now use CPU time more efficiently. The code itself is slightly quicker, but especially the 
  queuing system is new and more effective
- Galapagos can now pass on SIGMA images to Galfit/GalfitM, if the user requires this
- Galapagos can now use RMS maps rather than WEIGHT maps for the SExtractor step
- Some parameters (e.g. more constraints) are now user inputs, instead of being hard-wired into the code
- Many other, small changes, mostly for cleaning up the code, increases efficiency, computation speed-up

####B) Most important tips as they cause the most problems
1) Make sure that the galapagos folder where all the code files are is in your IDL path and relatively at the start of it.  
Some procedures (e.g. mrd_struct.pro) have changed in the astrolib and some even within IDL.  
Galapagos requires particular versions of all files (e.g. because it uses additional parameters that don’t exist in all versions), so please make sure your IDL uses the files provided in all cases.  
THESE are the tested procedures and using other ones can and will cause problems. Related to this: If you find a script is missing in the galapagos distribution, please let us know. We would like the Galapagos-2 package to be as complete as possible

2) Make sure your images are calibrated correctly! Galfit/GalfitM, when creating an internal sigma map for the fitting, required images to be in COUNTS, not counts/sec! When using your own sigma map, this is not important, but Galapagos does not (yet) allow this. (see tip 3!)  

3) All images for GalfitM must contain some data. GalfitM will crash (segfault) when one of the images in entirely blank or entirely masked out. Hence, Galapagos-2 will only work where ti can cut these postages stamps accordingly, e.g. NOT where one filter has a huge gap while others do not.  
If you have such a dataset, I would recommend 2 independent runs, e.g. one where all data is available, one in which only a subset of filters are available (e.g. when your different wavelength images have different sky coverage).

4) Please read http://users.obs.carnegiescience.edu/peng/work/galfit/TOP10.html  for top 10 tips for galfit, most of which are also true for Galapagos-2, e.g. image calibration,…

5) Please make sure you can actually RUN Galapagos/Galapagos-2 on your computer. E.g. IDL (at least v6.4, I think) has to be installed properly, including (and especially) IDL\_IDLBRIDGE. Galapagos will work without this feature, as long as only 1 processore is used (D18 == 1), but will not work for parallelization.
(the error message  
% Attempt to call undefined method: 'IDL\_IDLBRIDGE::ExecuteTimer'  
seems to be connected to this)  
This is how you test this:  
We provide a script check\_idl\_bridge.pro for this purpose. (please see utility fodler and README therein)  
Simply run this script in your IDL. 
It should simply write a file into your home directory called ~/IDL\_bridge\_works, using the IDL_bridge.

If this script tells you that IDL\_IDLBRIDGE is not working, ask your IT department to fix this.

6) Please make sure you use an appropriate number for D18. It is explained below, how to find and set this number.

7) due to limitations of string lengths that can be stored in a FITS table (71 characters?), please make sure that your paths are not too long. Avoid fitting in a subfolder of a subfolder of a subfolder,.... Try to make a shortcut from your home directory straight to the galapagos output folder.
(Also keeps your Galfit start files tidier)


#### C)  SETUP AND EXAMPLE SETUP FILE
 
EXAMPLE.SETUP is the main file.  
The setup largely follows the setup file of the original (single-band) Galapagos, but Galapagos-2 is NOT fully backwards compatible!!  
The original setup is explained in detail on the Galapagos website ( http://astro-staff.uibk.ac.at/~m.barden/galapagos/ ) 
and in Barden et al 2012.

#####===========FILE LOCATIONS===========

    A00) /path/to/images.gala     #file containing ALL input files, including SExtractor (1st line) and all bands, weight,...)

FILE FORMAT HAS CHANGED DUE TO MULTI\_BAND CAPABILITIES!  
The old format of images.gala works only for single band fits.  
For multi-band fits, several more files are required:  
- images.gala  
- images_[sex,r,u,g,i,z]


#####images.gala  
- in one-band surveys, this is the same as in original galapagos  
- in multi-band surveys, this has a different format  

_format:_  

    \#folder\_pre(name) #effective\_wavelength #mag\_offset("SED") #/path/to/images\_[].gala #ZP #EXPTIME

_example:_   
  
      sex  6231    0.00 /path_to/sex.gala   30.0  1.0  
      i    7625   -0.60 /path_to/i.gala     30.0  1.0  
      u    3543    1.16 /path_to/u.gala     30.0  1.0  
  […]

_explanation:_  
**'folder_pre'**  
are used for naming files and folder  
- each band gets its own folder for postage stamps  
- masks and other files follow this naming convention  
- the output parameters in the GalfitM output follow this naming convention
          
**'wavelength'**  
is the 'wavelength' passed over to galfitm for use of Chebyshev parameters (any units)  
- I use ‘effective wavelength’ of the filters, as provided by the surveys.  
- the 'sex' value will not be used anywhere in the code   
- does not necessarily have to be wavelength, log(wavelength) might work as well. Your call!  
- please see Haeussler 2013 and Bamford et al 2014, in prep for details

**'mag\_offset'**  
defines how Galapagos-2 transfers the 'sex' magnitudes from the source extraction step into starting values for each band. SExtractor only produces one catalogue, but starting values are required for each band. Therefore a correction from the SExtractor value and the starting value need to be estimated.  
- again, the SExtractor value will not be used anywhere (I set it to 0.00 anyway in the example)  
- e.g. a MEDIAN/MEAN SED can be used to derive these numbers  
- I defined my values by running SExtractor on images of all bands, correlating them and defining the median colours in respect to the combined image/image used for source extraction of the brightest objects between the sex and the [band] image)  
- Yes, this would be better if Galapagos-2 ran Sextractor in dual band mode over all SEx/BAND combinations, to get good starting values for all bands.  
- No, this has not been done yet. Might be done in the future, but turns out to be of less importance given that Galfit and GalfitM seem to be very insensitive to the magnitude input values.


**'/path/to/images\_[].gala'**   
- input file, where all images for that band are defined  
- images\_[].gala themselves hold the paths to the images (tiles of images) and weight images: 

     /path/to/science_tile1_1 /path/to/wht_tile1_1 output_directory_to_be_created object_file_preposition
     [..tile1_2]
     
**'zeropoint'**  
defines the magnitude zeropoint  

**'exposure\_time'**  
defines the exposure time

‘zeropoint’ and ‘exposure\_time’ are only defined ONCE for all images of a respective band. This means that  
- galapagos needs a 'uniform' survey setup or images to be normalized in this matter.  
- E.g. all images of a certain band must have the same zeropoint and exposure time (but different bands can be different)  
- While a more complicated setup might be possible, it has not been implemented

**IMPORTANT** notes on the usage of multi-band setups:  
- the images in all bands have to be micro-registered to each other and of the same setup — that is: the same 'mosaic’/cutouts — has to be provided in all bands  
This is due to the nature of Galapagos/Galapagos-2 working mostly in pixel coordinates (other than during the Source Extractor block)  
- the **first** band defines the images on which SExtractor is carried out. This could e.g. be a COADDED image, to ensure deepest possible source extraction.  
It is NOT however used in the fitting process, You can choose whatever band or combination of bands you deem fit for the purpose.  
- the first **normal** band is somewhat important, too:  
This is the band on which all deblending and masking decisions are taken. I would advice to use the deepest or highest-quality band for this purpose.  
Avoid extreme bands here. Due to the nature of GalfitM, the values at the edges of the wavelength range are potentially somewhat less certain, which could lead to bad deblending decisions.  
- the other bands do not have to be in any specific order, all bands are ordered according to wavelength within galapagos before passing over to GalfitM.


#####images_[r,u,g,i,z,sex]
images\_i\_sigma  
- equivalent to images.gala in single-band surveys  
- format for multi-band surveys:  

    For SExtractor:
        /path/to/image_sex_1_1.fits /path/to/weight_sex_1_1.fits tile1_1/ t1_1.

the 3rd (output folders for each ‘tile’) and 4th (‘pre’ for file naming) parameters are only defined here  

    For fitting bands:
        /path/to/image_g_1_1.fits /path/to/weight_g_1_1.fits
    or (of required)
        /path/to/image_g_1_1.fits /path/to/weight_g_1_1.fits /path/to/sigma_map_g_1_1.fits

As you can see, this now also includes the possibility to feed in a SIGMA_map if the user choses to.  
This image is simply cut and handed over to GalfitM as sigma map.  
If no sigma map is given, galfit creates it’s own sigma map internally.

Note on SIGMA images:  
In Galapagos-2 (from version 2.1.0) it is possible to give sigma\_maps for some bands, and not for others, but the entire band has to be consistent, e.g. ALL g-band images need sigma\_maps, or none.  
HOWEVER: galfitm-version < 1.1.3 do NOT support this feature, either ALL or NO images have to have sigma images.  
This has been changed in galfitm-1.1.4, which now also allows a mixture of images with and without sigma map.  
For Galapagos, the entire BAND has to be consistent, though.

Please note that any provided WEIGHT (or RMS) image does **NOT** get handed over to GalfitM!  It is mostly used to create the mask image used in the fit  (and source detection in SExtractor step)  

If no sigma\_map is given, galfitM creates its own internal sigma map.  
IF one is given, it is only passed over and has to be set up properly by the USER, Galapagos-2 does nothing to it, other than cutting postages

 ---
**Some notes on the format of the IMAGES, WEIGHT, RMS and SIGMA MAPS**

IMAGES (more on this, see GALFIT webpage):  
If no sigma map is given, Galfit/GalfitM creates it’s own sigma map internally. In this case the image normalisation is very important.  
Galfit/GalfitM then prefers images in COUNTS (NOT counts/sec), and the GAIN header keyword set correctly.   
Only then are Galfit’s assumptions true and only then will the sigma map that it creates internally be correct.

ALL images for GalfitM must contain some data. GalfitM will crash (segfault) when one of the images in entirely blank or entirely masked out (as it can not create a sigma map. Not sure whether it works when you feed one in). Hence, Galapagos-2 will only work where ti can cut these postages stamps accordingly, e.g. NOT where one filter has a huge gap while others do not.  

WEIGHTS (more on this, also see SExtractor manual):  
The weight images used in Galapagos-2 serve 2 purposes: Object detection (for the normalisation here see SExtractor manual) and masking.  
For the purpose of masking, set all pixels to be ignored == 0. This way, Galapagos-2 will create MASK maps to be handed over to GalfitM which will mask out these regions (Galfit/GalfitM will NOT see them). As of this, all areas where a band has no data, not only the image would be 0, but the mask should be set to 0 as well!! If this area is NOT masked, the image containing 0s will be handed over to Galfit for a fit, which obviously would create garbage.   
All pixels equals 0 will be masked, all other pixels will NOT be masked.
From the SExtractor manual:  
_The FITS image specified by the WEIGHT IMAGE file name must contain a weight-map in units of relative weights. The data are converted to variance units (by definition variance ∝ 1/weight), and scaled as for MAP VAR. MAP WEIGHT is the most commonly used type of weight-map: a flat-field, for example, is generally a good approximation to a perfect weight-map._  
E.g. see  http://www.ifa.hawaii.edu/~rgal/science/sextractor_notes.html

RMS (more on this, see SExtractor manual):
SExtractor:  
_The FITS image specified by the WEIGHT IMAGE file name must contain a weightmap in units of absolute standard deviations (in ADUs per pixel)._  
E.g. see  http://www.ifa.hawaii.edu/~rgal/science/sextractor_notes.html
 
SIGMA MAPS (more on this, see GALFIT webpage):  
E.g. see  http://users.obs.carnegiescience.edu/peng/work/galfit/CHI2.html  
SIGMA maps have to be created externally by the user. Galapagos-2 only cuts them and passes them on to Galfit/GalfitM. It does NOT tamper with them.
While some users might prefer other handling, this is overall the most flexible.

(From private communication)
SIGMA images can be created from an RMS and the image itself, similar to the following way.
The trick here is to normalize images properly and adding the poisson noise of the objects themselves on top of the RMS images. This can be done in approximation using:

    sigma = sqrt{rms^2 + [(img > 0)]}    (sqrt of image uncertainties (or 0, whatever is higher) and rms are added in quadrature. Not the missing ^2 on the image. Image uncertainties are sqrt(img). sqrt(img)^2 = img)

'img' in this case is normalized to 1 second (which is not preferred by Galapagos/Galfit, but easier to explain here! If you have your image normalized to please adapt accordingly!)

If the wht and rms image are correct (i.e., taking the pixel scale and gain into account, all images in electrons/second) then the correct formula to add Poisson noise is rms_total ^ 2 = rms^2 + (sqrt(sci) / exp).  
Here, sci is the science mosaic, and exp the exposure time mosaic. 

In this context it is important that exp is normalized to the right pixel scale!  
Usually, an exposure time map gives the actual number of seconds that the drizzled image has been exposed, but this doesn't correspond to the number of photons it has seen.  
For this to be correct one needs to multiply exp by the change in pixel area when drizzling. To put it simply, conserve the total number of photons in the mosaic. 

 ---
#####===========SEXTRACTOR SETUP===========
**Introduced new setup parameter** to be able to use rms weighting in the SExtractor step.  
B15) == ‘rms’ switches on this feature. 
Other than SExtractor, everything is unaffected.

EVERYTHING AFTERWARDS HAS BEEN RENAMED!! i=i+1


#####===========STAMP SETUP AND SKYMAP CREATION=========
**No changes**, other than the original galapagos, galapagos-2 can use up to 4 CPUs for creating the stamp files and up to D18) cores for postage stamp cutting in order to speed up the process on large datasets.  
Set D18) = 1 to switch this feature off (seems to be needed also for a single image in this step, see further down)  
When sigma maps are provided, the same postage stamps are cut from this image.  
**WARNING**, see below (D22): If you rerun the postage stamp cutting again for a different sample, the stamp files obviously get overwritten.  
Your stamp files and the target list that you use have to match each other.

#####===========SKY PREPARATION =====
**New parameters, changes in numbering!**  

    OLD D12) 100      # fraction of sources to be treated first (in %; 5=5%)  
**has been taken out**.   
This number actually had no effect in the code anymore and had been replaced by other means (e.g. 'target list') 
It is easier and more precise to select target objects in other means (see below)

**EVERYTHING AFTERWARDS HAS BEEN RENAMED!! i=i-1**

    D18) 12          # maximum number of parallel processes (see Barden 2012 for details)  
(At least) On MY machines, it turns out that a number >16 is not possible. This comes down to IDL not being able to 'destroy' the 'bridge' object  that it uses for parallelization

To figure out whether this is true on your system, run (for any number D18):  

    arr = objarr(D18)
    FOR i=0, D18-1 DO arr[i] = obj_new('IDL_IDLBridge')
    obj_destroy, arr

If IDL can do this, you can use the number given in D18), if it ‘hangs’ itself, a smaller number is needed.  
As I said, on my system (and confirmed by others), 16 turned out to be the highest possible and this seems to be an IDL limitation (but might change with IDL version?)

    D19) 300      # minimum distance (in arcseconds) between sources processed in parallel (see text for details)  
    D20) 80       # minimum distance (in arcseconds) to already blocked objects  
                  # (to make sure that no faint object next to a very bright one is fit. standard value: D19)/3. )
The queueing system in the code changed completely. Galapagos-2 fits objects from bright to faint. If the next bright object to do is too close to a currently fitted object, original Galapagos would WAIT for this object to finish, leaving some CPUS without work and slowing down the overall fitting process.  
Galapagos-2 instead continues with the next brightest object and gets back to this object as soon as possible.  
This means that some blocking radius around blocked object is needed (I have found that when using the same, very conservative radius D19) in densely populated fields, Galapagos-2 blocked all objects in the survey, making a smaller radius necessary).   
If you prefer to be conservative, just set to high radius, but live with the odd slowing down of your code.
For both D19) and D20): The larger, the better, but the slower the fits.

    D21) /path/to/target_objects   # optional list containing primary targets (format: RA DEC
                               # feature switched off when file does not exist, everything will be fit
    D22) 1.0                       # search/correlation radius for the above list [arcsec]
This list lets you target specific objects (e.g. only the objects you have redshifts for, only your highest mass objects, …)  
The format of the file is easy and shown in the according example file (RA DEC, one line per object).  
My advice would be to not only fit the objects of interest, but ADDITIONALLY objects within some radius [e.g. within D19) ] that are BRIGHTER than the object of interest itself. This ensures that the bright object is fit first and then dealt with properly when the interesting object will be done. However, this obviously slows down the overall progress.  
We have not tried how much of an effect this scheme would have compared to only fitting the objects of interest directly, as it will be highly survey dependent.

**IMPORTANT NOTE: This target list will also influence block C!!**  
Postage stamps will only be cut for the objects in this file in order to save disk space. If you want postage stamps for all objects, disable this feature when running block C.  
Should you notice that you require more postage stamps at a later time, simply change the target list and run block C again (avoid re-running block B).  

**WARNING (only just noticed), see block STAMP CREATION**:  
**This is fixed in version 2.1.2**, which will not change the stamp files, but will select the postages to cut by other means.  
If you rerun the postage stamp cutting again for a different sample, the stamp files obviously get overwritten.
Your stamp files and the target list that you use, however, have to match each other. Best rename the stamp files (in the setup) for a second sample.  
The stamp files are also read in when determining the sky values, so they are important during the running of the code.  
_As long as your first run is not finished, hence to not change these files, it will make those fits fail!_  
However, as objects are identified with the SExtractor number, changing the order in the stamp files does not matter, e.g. ADDING more objects would work.  
Ideal solution (though requires more disk space [**Again, not neccessary in version 2.1.2 onwards]**):  
- create postage stamps for ALL objects (no target list used)  
- then carry out the fits for your target list   
This way ALL objects can be primary objects.  
As this is how I always ran my codes, I have only just noticed this when I ran it a different way for a very large survey.  
**(Fixed in galapagos-2.1.2 onwards)**

If you are using a large target list on a large survey the source correlation in this step can take a VERY long time. In order to speed this up, (and having to do it every time you run it) Galapagos-2 creates a file in which is saves the correlation in a *sav file (easily identifiable in your output folder).  
When you run the code again, it checks the CREATION DATES of your input and that *sav file and only correlates the objects again when your input catalogue changed (e.g. is newer than the *save file).  
To be sure the code recreates it, simply delete the *sav file (with matching name to your input file) in your output folder.


#####===========GALFIT SETUP=========== switched on at D00)
**New parameters & renamed parameters!**  

    E01) batch1   # filename for list of tiles in current batch (format: image)
This batch mode had been taken out in previous versions of Galapagos-2 although being present in original Galapagos. It has now been re-introduced in a much cleaner and faster manner in order to have several computers working on the same data.  
The file ‘batch1’ (and similar) simply contains image names (SExtractor file names!! It compares the strings) that are to be dealt with. Only galaxies on these images are fit. Galaxies on any other image are not being dealt with in this run of Galapagos-2.  
By using different batch files, it is possible to run several machines and many cores on the same dataset but splitting up the survey into several areas
(See example on how to run Galapagos-2 on a large survey at the bottom of this file).

    E04) /path/to/psf_r,/path/to/psf_u,/path/to/psf_g,/path/to/psf_i,/path/to/psf_z   # PSF filename including path (ensure same order as in A00)  
Original Galapagos allowed ONE file at this point, e.g. it uses the SAME PSF for the entire survey.
This is unfeasible for  
a) most ground-based surveys (with changing PSF over the field)  
b) multi-band surveys where the PSF obviously is different in different bands

Instead, Galapagos-2 has 4 different ways to define PSFs  
1) one CAN define individual PSF images for each band here (separated by ',') if one wants to use the PSF for the entire area  (possible e.g. for HST surveys with very stable PSF). These images have to be fits files. '.fits' in the filname is how galapagos sets this feature.   
2) one can define (ascii) PSF lists here, which contain information about the PSFs and where they should be used.  
This can be done in 3 ways (the code decides from the number of columns in these files which way is used.  
For this to work, make sure you use SPACES to separate the columns, not TABS or similar!
2a) 2 columns: 1 PSF per input image
  
    format: tile_name psf_file
tile\_name as in sextractor setup file. Feasible e.g. for mosaiced surveys where individual patches are taken at very different times, so the PSF can be quite different, and not PSF homogenisation is carried out.  
2b) 3 columns: closest PSF in RA&DEC  
  
    format: RA DEC psf_file
For any object in the survey, the CLOSEST PSF in the list is used (my preferred method).  
2c) 4 columns: defines a box in RA&DEC in which a PSF is used. If there is overlap, the PSF from the closest box center is used. (Not sure who needs this, it seemed useful at some point)
  
    format: RA_min RA_max DEC_min DEC_max psf_file

I would suggest version 2b) for most surveys, but every BAND (!) can use it’s own method, e.g. when using data from different surveys with different requirements. It SHOULD be able to use different setups in different bands, but this has not been well tested!  
PSFs have to be created by the user beforehand, Galapagos-2 does NOT do this on-the-fly (at least yet).  
PSFs can be created by PSFEx, for example.  

PSFs MUST be given in the same order as the bands are given in A00) (exlcuding SExtractor), e.g. first the MAIN band, then the other bands in the same order.  

    E11) 400      # constraint max Re
as information: as minimum radius 0.3 pixels is used in the code as we do not expect smaller galaxies to be fitted well. If afterwards you only believe a larger radius, please cut out in the output catalogue. The minimum size was introduced as — at very small radii — the fit iterations can take very long, due to oversampling issues (please read Barden et al. 2012 for details)

    E14) 0.2      #constraint min sersic index
    E15) 8        #constraint max sersic index
now lets the USER define the allowed range in sersic index. 0.2-8 was hardcoded in the original version of Galapagos, but some people argue that galaxies with n>8 are real, so these parameters hand over setting the constraints to the user.  
As for minimum size, please be aware that very extreme values will cause over-sampling issues.  
To my knowledge (although that might have changed), Galfit (and hence Galfitm) look up normalisation numbers in a table. This table only  exists for values of n<20, so larger values should be avoided (or not trusted)

    E16) nice     #nice the GALFIT process ("nice" = true)
If your IDL session is niced, the galfit is already niced automatically. (renamed, previously named E14)

    E17) 4.0      #GALFIT version string. E.g. 2.0.3c
This defines the input file format for the galfit start files. (renamed, previously E15)
In order to use galfitm (with multi-wavelength capability), use a version 4.0 or higher (this is how Galapagos will know what you're trying to use)

    E18) galfitm-0.1.3.1  # string used to identify running galfit processes in order to kill them after E19 minutes (string seen in 'top')
    E19) 240              # time (in minutes) after which galfit processes are killed
                          # feature switched off when time == 0 
Galapagos-2 has a new feature to kill galfit processes when they have been running too long (as we have seen those to crash or produce garbage at some point anyway, but occupying CPUS for ages).  
This is done by a combination of 'ps', 'grep' and 'kill', 

        ps -u YOU | grep E18
so E18 should be the string that the user can see when running 'top'.  
Please notice: Using this feature will kill ALL galfit processes with this string by the user who runs galapagos (other users are un-affected), even the ones NOT started by Galapagos-2. Please use with care.  
Switch this feature off way setting E19) == 0  

    E20) 0,0,9,2,2,0,0  # x,y,mag,re,n,AR,PA  order of Chebyshev polynomials in the individual parameters to be used in GALFIT
                        # 0 = constant over all wavelength, 1 = linear over wavelength,... #band-1 = free
                        # These are ADDITIONAL degrees of freedom, e.g. 0 STILL means, the parameters can be fit 
                        # (something which is decided by GALAPAGOS, not the user)
                        # This is DIFFERENT to the number that has to be given to GALFIT, in galfit = gala+1

**This is the most important step regarding multi-band fitting.**  
As explained in Haeussler 2013, Vika 2013 and Bamford 2014 (in prep), the strength of GalfitM and the MegaMorph approach is that profiles use all data simultaneously, but constrain the parameters over the different images in the different bands, hence utilising the wealth of the data. **The choice of degree of freedom used in the process is important!** Too high degrees of freedom mean that the multi-band approach loses it’s advantages (e.g. using all parameters with degree of freedom equals number of bands would essentially carry out single-band fits on all images, just all at the same time — and slower).  
This degree of freedom should probably be somewhat targeted at the science one wants to carry out, the galaxy sample in question and the images (bands) used.  

- If all bands are close together, a real colour advantage to separate bulges and disks is probably not possible. One might only be interested in fitting a constant (with wavelength) profile to several images in order to take advantage of better signal-to-noise (e.g. ‘0’ would be the correct freedom to use above)
- Measuring colour gradients in large galaxies in single-sersic fits could require linear or second order polynomials to be fit (1 or 2)
- On large, nearby galaxies, using some degree of freedom even for individual galaxy components is probably possible (set in F01 and F02).
- On faint(ish) distant galaxies, where B/D decomposition will be most desired, but most difficult, probably fitting a constant profile for each bulge and disk might be advisable, ignoring internal gradients of the components.

The degree of freedom used is down to the user and should be  
a) dealt with wisely  
b) adapted to their needs  
c) carefully tested!  

**CAUTION IN THIS CONTEXT:**  
- GalfitM and Galapagos-2 return 2 sets of parameters for all fitting values:

-   *\_band, that give the values at the specified wavelengths, and  
-   *\_cheb, which give the parameters of the polynomials.

*\_cheb are the actual values used in the minimisation process, but *\_band are more useful for most users.  
While the *\_band values can be used at all times, the *\_cheb values only make sense when the degree of freedom is sufficiently small.

 - if the degree of freedom is small the polynomial can also be used e.g. to derive ‘restframe’ values (if you want to do this, ask me for an IDL routine that helps you). 
 - If the degree of freedom is high (equal or close to the number of images) (as e.g. desired for magnitudes), the polynomial itself will oscillate between the defined wavelengths (‘Runge’s phenomenon’). The *\_band values will still be correct (for the observed wavelengths), but the polynomial should NOT be used to estimate restframe values. E.g. use other software to derive restframe magnitudes

**IMPORTANT:**  
There is **one** special case in GalfitM, which CAN happen in Galapagos-2, especially for B/D fitting.  
Generally, if one defines starting values and fits a certain order (say, ‘linear’ or second order polynomial), the code fits this polynomial to the starting values to derive it’s ‘true’ starting values (Chebyshev parameters). GalfitM hence makes sure that the outcome always follows the required polynomial shape.  
**HOWEVER**, if you are fitting a CONSTANT value, but the input values are NOT the same (!), GalfitM KEEPS the offsets!  
This can be very helpful if e.g. the **shape** of a SED of an object is known (e.g. a Supernova, AGN, …), but its brightness is **not**.
Galapagos-2 makes sure (I think) that the starting values are always homogenised (e.g. by taking a median value as starting parameters, other than for magnitudes (!) when starting B/D fits), but this is a feature of GalfitM that a user should urgently be aware off!

    E21) galfit   # output folder name for all galfit output files
creates an output folder in which to store all files related to the fits:  
- galfit setup files  
- constraint files  
- mask images  
- galfit output files  
This was introduced to keep all folders nice and clean.
Original Galapagos puts all files for each tile into the same folder, with big images and several bands this could produce 100s of 1000s of files.  
These are now all stored in different folders (defined here for galfit and in A00 for the bands), making finding files afterwards much simpler.  
All image names are stored in the Galapagos output table, e.g. these can be used in an automated fashion to carry out follow-up analysis, e.g. analysing the fitting residuals or similar.

#####===========B/D DECOMPOSITION SETUP===========
**completely new block**  
Please note that B/D decomposition will NOT work when you use original Galfit, due to the way the fits are set up in gala_bd_bridge.pro!
The script needs information from the image header that original Galfit does not provide. Re-writing this would be a major task.
If you want to carry out B/D decompositions, make sure you are using a GalfitM version, even when using single-band data.

    F00) execute         # execute the B/D fitting block ('execute' = true)
    F01) 0,0,9,0,0,0,0   # similar to E18 for BULGE (see README for details) 
    F02) 0,0,9,0,-1,0,0  # similar to E18 for DISK  (see README for details) 

The input should all be easy to understand with the above information at E20).
Additionally:  
If freedom of the sersic index is set to -1 (!), Galapagos-2 uses:  
n==1 (fixed) in case of disks  
n==4 (fixed) in case of bulges  
(more variation such that the user could set the fixed value is planned, but not yet implemented, turns out to be more complicated than I thought)

    F03) bd       # additional output label, e.g. output folder for galfit files: E21+'_'+F03 to keep files ordered
This is an additional label for BD fit folders and files.  
it allows to carry out different fits (e.g. by using different F01 and F02 settings), without overwriting any fit output files.
Note that only ONE of these will have the output stored in the Galapagos-2 output catalogue. You need to changed F03) and G02) in order to get a new output that does not overwrite the old one.

    F04) /path/to/target_objects   # optional list containing primary targets, can be different (a subset) of D21)
    F05) 1.0                       # search/correlation radius for the above list
    F06) 18.0                      # additional magnitude limit (sersic fit, main band) down to which B/D will be done
The combination of these defines the sample to be fit with B/D fits.  
F04) **can** be different to D21), but it has to be a **subset** of those (e.g. only the galaxies in question). The reason being that B/D fits can only be carried out for an object if the single-sersic fit already exists.  
The B/D fit is set up using the **single-sersic output** file. This means that :  
- neighbours are handled in the same way (in fact, they are held fixed at the profiles determined in the single-sersic fit)  
- B/D will only be carried out for objects where a SS fit output file already exists! Where single-sérsic fits have crashed, no B/D fit can be carried out in the current Galapagos-2 version.

The starting values for the fits are also taken from the single-sersic fits following this scheme (since galapagos-2.0.6 for now, subject to change).  
This could be changed by the user in "gala_bd_bridge.pro" if he wishes to do so, e.g. by trying his own setups:

    Disk:
    x & y : same as ss result
    mag   : half the flux (mag+2.5*alog10(2))
    re    : median(re_ss)*1.2 > 1 [px]
    n     : median(n_ss) < 1.5  (or 1, when fixed value) (including variability with wavelength, see comments on E20 above!)
    q     : q_ss
    pa    : pa_ss

    Bulge:
    x & y : same as ss result
    mag   : half the flux (mag+2.5*alog10(2))
    re    : median(re_ss)*0.3 > 0.5 [px]
    n     : median(n_ss) > 1.5  (or 1, when fixed value) (including variability with wavelength, see comments on E20 above!)
    q     : q_ss
    pa    : pa_ss

These values, especially for n and re, are still somewhat subject to testing, but they turned out to work nicely on nearby and 
artificially redshifted images as explained in Vika 2014, in prep.
They seem to work in Galapagos-2, too. Slightly different starting values did not seem to have big effects on the output values 
(Haeussler et al 2014, in prep).

While writing this README, I have noticed that technically different parameters are handled slightly differently (e.g. pa and q do not use MEDIAN(ss_values). 
Every offset from the single-sérsic fitting — if present — will be carried over to the starting values, SEE IMPLICATIONS AT E20))
This might be corrected at some point, but has not yet been an issue due to the degrees of freedom we use (and we would advice to be used).

Please note that both the bulge and the disk are started using the **same magnitudes** (and hence the same SED)!!
While some authors argue that this might make it difficult for GalfitM to deal with, it appears to us that it is not.
Bulge and disk can be separated quite well down to faint levels (Haeussler et al, in prep, Vika et al, in prep). 
Again, if a user disagrees, the code can be changed in gala_bd_bridge.pro easily.


##### Experimental and will NOT work for other users without code adaptation

    F07) nHPC                                # switches to HPC mode ("HPC" = true), paths will be adapted (F08 and F09 become important)
    F08) /path/to/HPC/output/folder          # alternative path for Supercomputer use.
    F09) /path/to/data/, /path/to/data/HPC   # PSF filename correction (!). [1] is replaced by [2]

**The HPC setup is VERY experimental and VERY targeted to the computer setup in Nottingham.**  
As the B/D fits in their current form are entirely independent of each other, it is possible to have Galapagos-2 only write out the galfit start files. They can then be transferred to a HPC with many more cores and carried out there very quickly, saving overall computing time.   
If a user wants to use this feature, I am certain that he will have to change the Galapagos-2 code at this point to make it work for his/her setup.  
Please use with care and only if necessary.  
By using the 'batch' mode in E01, a speedup is possible by using several computers doing a part of the data each (as long as they can see the same hard drive and an initial run has been carried out) and without using a HPC. Please see below for advice on this scheme.  
Additionally, as of version 2.1.1, the queueing system is effectively disabled for B/D fits, so all CPUs will be busy simultaneously.

[on a different, but related, note: If a true HPC version on SINGLE BAND DATA (!) is desired, please contact Andreas Hiemer (andreas.hiemer@uibk.ac.at) who has translated Galapagos (original) into a C-version with several additional features  (e.g. fitting Fourier modes).   
This only uses one band data, but can be fully used in a HPC environment, carrying out the fits much faster.

Yes, we are planning to combine the 2 versions.  
No, this has not been done yet and will take some time to do]


#####===========OUTPUT CATALOGUE SETUP===========
ALL RENAMED FROM PREVIOUS BLOCK F)

    G00) execute      # execute catalogue combination block (read single_sersic)
allows to read out only the single-sersic part of the fits

    G01) execute      # execute catalogue combination block (read single-sersic & B/D), switches on G00 automatically
reads out both the single-sersic and B/D fits

Please be aware that if the output file already exists, it will be overwritten!!  
Don’t forget to rename your output catalogues, if you want to keep your old one (e.g. because you ran different ‘degrees of freedom’)


 --- 
#### D) ADVISED WAY TO RUN GALAPAGOS IN A STABLE WAY (USING ‘SCREEN’)

There are 2 issues that Galapagos-2 (better: IDL) is sensitive to when you run it:  
  - loss of ssh connection   
  - Some internal tasks need a ‘screen’ to ‘print to’, even though no printout is delivered. Some of these are used in the IDL_BRIDGE, so their error messages would be hidden from the user.

A way to get around these problems is the following scheme:

    - ssh into your computer/server
      ‘ssh me@my_computer.my.department’
    - create a VIRTUAL DISPLAY!!
      ‘nohup Xvfb :99 -screen scrn 800x600x16 &’   (or similar, I needed different versions for different LINUX system and hardware)
    - ‘screen’
    - ‘export DISPLAY=':99' ‘  (pipes all output to the ‘virtual display’)
    - make sure your IDL path is set correctly (e.g. via .bashrc, which is run when you start ‘screen’, or compile by hand)
    - ‘nice idl’   (this makes sure your Galapagos runs in the background, using only free CPU time
      It will then not slow down (and annoy) other processes and users.
      If you use ‘nice idl’, it’s a good idea to NOT nice the galfit as well, they will be niced automatically, but not doubly so)
    - start galapagos:
    ‘galapagos, ‘/path/to/gala.setup_file’ ‘ 
    - Ctrl^A, Ctrl^D : puts the screen away. Get it back via ‘screen -r’

Your Galapagos-2 does now run even when you close the ssh connection.  
Unless your server needs an actual re-boot, your Galapagos-2 run will be busy.  
I have so far only tried Xvfb for this purpose. As has been reported, some users have difficulties with this, it always worked for me. Alternative methodes **do** exist, but I can not name any.

 ---
#### E) ADVISED SCHEME WHEN RUNNING GALAPAGOS-2 ON LARGE DATASETS

1. Set your IDL path right for the Galapagos-2 run!!!!  
2. Cut all images into pieces  
   Due to the nature of the code, Galapagos-2 runs several tasks over the entire input tile. The more objects there are, the longer it takes. Smaller input tiles hence make the execution faster and should be preferred for large surveys. Make sure there is enough overlap between tiles. Galapagos-2 will take care of objects that are detected in multiple tiles.  
   You can cut the image e.g. using IDL:
   
    writefits, image_1_1.fits', large_tile[0:2000,0:2000], hdr

    or using fits copy (part of cfitsio which comes with Chien Pengs galfit package which most of you will have anyway, but which also can be downloaded and installed manually)
    ./fitscopy large_tile.fits[0:2000,0:2000] image_1_1.fits    (or similar)
           
  I cut my images into 2000x2000 tiles with 500 pixels overlap. The overlap should be defined by the size of the largest object in the survey

3. Set up your masks/weights/rms/SIGMA images  
The weight images should be set ==0 for all pixels that should be masked out during the fit (other than neighbouring objects, which Galapagos-2 handles itself internally)  
See above on notes on image calibrations, weight, sigma and rms maps.
4. Decide (and/or create):
   - the image that SExtractor will be run on
   - the mag_offset in A00 that Galapagos-2 needs to transfer SExtractor magnitudes into starting magnitudes, e.g. a median SED in comparison to the SExtractor values
   - which image/band you want the deblending decisions to be made on. Advice would be to use the 'deepest' band.
5. Create setup files
6. Run blocks B and C (remember that C only cuts postage stamps for the objects defined in D21) using the **entire list**  of images!!  
This creates a SExtractor catalogue on which the entire rest of Galapagos is based. In order for the batch modes (below) to work, this has to contain the **entire** list of objects. If you run SExtractor only over a subset of images, files will get overwritten and the batch modes will NOT work!  
You can run block C again with a different selection of objects later, but I would advice against running block B again! Ever!  
If you do, all ‘SExtractor numbers’ (which are used as object IDs in Galapagos) might change and hence all file names which could mean that Galapagos-2 can not find the correct images to fit or that it confuses/mixes up different galaxies. Once SExtractor is run, you should stick with what you have.  
Running the postage stamp cutting again is easy and possible in case you decide to fit a larger sample at a later time. Just change the target list and run block C (and obviously D/E/F) again.
7. use the SExtractor output catalogue to define the values for 

    D13) -0.25    # slope in log(fwhm_image) vs. mag_best below which object is star
    
    D14) 5.8      # zeropoint in log(fwhm_image) vs. mag_best below which object is star
8. create batch files (if required)
9. Run different setups (everything identical, other than the batch file in E01) on different machines.  
Make sure they all see your galapagos run under the same path as defined in A01, e.g. by cross-mounting your hard drive!!
The batch mode restricts each run to primary objects on the defined input tiles, while knowing the entire object catalog 
(e.g. making deblending decisions with full knowledge of the entire survey).  
For example, I have 4 computers with 16 cores each, each crunching through 1/4 of my tiles, but the limiting number of computers that can be used is possibly only limited by the disk reading/writing speed.
10. Maybe run one additional run without any batch files, to make sure you didn't ‘forget any objects’ (Yes, embarrassingly, that happens)
11. Run one last run with only reading out ALL results (although the batch mode does not technically affect this part of the code)
12. Do awesome science under full use of the MegaMorph fitting results!

 ---
#### F) ADVISE WHEN RE-RUNNING GALAPAGOS, E.G. AFTER SERVER CRASH OR AFTER YOU’VE KILLED IT

Galapagos-2 saves and deletes several files during it’s run, e.g. to pass over information to the ‘bridge’ processes that take care of individual galaxies.  
When you re-run Galapagos-2 some of these files need to be deleted, otherwise the targets in question will NOT be dealt with.  
At no point when you start Galapagos-2, should there be any *sav files in the galfit output folders!  
In case you CTRL-C’d your galapagos, the same is true, as the *sav files will not be deleted by the code.  

In case you even killed running Galfits/GalfitMs (e.g. in ‘top’ or by using 'kill_galfit' in the galapagos scripts), please do the following:

- If you changed the setup and fits might change (e.g. postage stamp sizes or deblending decisions), you need to delete ALL \*obj and \*sav files in the /tile*/galfit/ folders, effectively fitting all objects again.
- If all fits that have already finished will be fine as you didn’t change anything, you still need to find the *obj and *sav files of the objects that you killed, e.g. using ls -tr *obj and ls -tr *gf.fits (do not delete the ones that successfully finished!).   
Best is you delete everything related to these objects (identified by their SExtractor number in all file names) so Galapagos-2 thinks this object has not yet been tried and does everything properly.   
If you don’t delete the files (most importantly the *obj file), Galapagos-2 will think that the objecthas been started but the fit crashed! It will then accordingly only ‘read’ out the result (e.g. the sky values) and set flag_galfit == 1 in the output catalogue, but it will NOT retry fitting this object!

There is now a script called ‘clean_galfit_folder.pro; which you can use to delete the files as described above.
Please find details about this in the README in the utilities subfolder.
 
 ---
#### G) VERSION HISTORY

**V2.0.7**  
- D12 has been taken out of the input files as it had no effect in the code  
  -> hence the main input file changes slightly, everything in block D and i>12 should be renamed to i=i-1  
- Galapagos-2 now works with original Galfit (v.3.0.4) again when using one-band data (and is very fast doing so!! Mind blown!)  
- The input files have been cleaned up. All the tile subfolders are now defined in the sextractor file list
  The format is hence slightly different:

    For SExtractor:
    /path/to/image_sex_1_1.fits  /path/to/weight_sex_1_1.fits tile1_1/  t1_1.
    
the 3rd (output folder) and 4th (‘pre’ for file naming) parameters are only defined here

    For bands:
       /path/to/image_g_1_1.fits  /path/to/weight_g_1_1.fits

**v2.1.0**  
B15) RMS map now allowed for SExtractor  
   (FROM Sextractor manual:)  
   _- MAP RMS: the FITS image specified by the WEIGHT IMAGE file name must contain a weightmap in units of absolute standard deviations (in ADUs per pixel)._  
   _- MAP WEIGHT: the FITS image specified by the WEIGHT IMAGE file name must contain a weight-map in units of relative weights. The data are converted to variance units (by definition variance ∝ 1/weight), and scaled as for MAP VAR. MAP WEIGHT is the most commonly used type of weight-map: a flat-field, for example, is generally a good approximation to a perfect weight-map._  
- Galapagos-2 passes on SIGMA images to galfit now, if user specifies them. They have to be EXTERNALLY created!  


    For SExtractor:  
    Format as before, but ‘weight’ image can now be ‘rms’ image (indicate at B15) )  

    For bands:
       /path/to/image_g_1_1.fits  /path/to/weight_g_1_1.fits
    or: 
       /path/to/image_g_1_1.fits  /path/to/weight_g_1_1.fits  (/path/to/rms_map_g_1_1.fits)

BOTH works!  
As you can see, this now also includes the possibility to feed in a sigma\_map if the user choses to.  
If no sigma map is provided, Galfit/GalfitM creates it’s own sigma map.  
It is possible to give sigma\_map for some bands, and not for others, but the entire band has to be consistent, e.g. ALL g-band images need sigma_maps, or none

**CORRECTION:**
galfitm-versions =< 1.1.3 do NOT support this feature, either ALL or NO images have to have SIGMA images  
This has been changed in galfitm-1.1.4 , which now also allows a mixture of images with and without sigma map.  
For Galapagos, the entire BAND has to be consistent, though.

**v2.1.1**  
- Now backwards compatible to IDL7.x , instead of requiring IDL 8, possibly even works for older version. IDL 6.3, introducing IDL_IDLBRIDGE, is STRICTLY required!!  
- some minor bug fixes that could make the code crash (results unaffected, if produced), only internal book-keeping at a position where it didn't really matter  
- kill_galfit actually works reliably now.  
- bug fix, B/D fitting now works in batch mode  
- effectively disabled fancy queuing system for B/D fits as these fits are independent anyway (at the moment)  
This is done by automatically setting the blocking radii to 0, so some unneccessary calculations are still done.
The code is not as effective as it could be. However, most of the time is still spent on the actual GALFIT fit.

**v2.1.2**  
- slight change in the selection of postage stamps to create. Which objects are cut has now no influence on rest of the code   
- The ENTIRE setup is now copied to a setup folder in the output folder, including sextractor setup files  
- code now comes with a LICENSE and a top level README

**v2.1.3**  
- replaced routine create_struct with function create_struct. Does not need compilation and should be better for running in IDL runtime mode

**v2.1.4**  
- Both GalfitM1.1.6 and Galapagos can now deal with entirely empty images, e.g. when the survey footprint id different in different bands (even if the 'primary' band). Added readout of NGOOD and NMASK (for each band) into the output catalogue, which give indications for empty images in the fit stack. 
- In the same context, Galapagos does now restrict the number of degrees of freedom of a polynomial to the number of bands with 'useful amount of data'. For this 3 more user inputs have been created (E22-E24). As an example, setting E23) to '30' will reduce the maximum number by 1 if an postage stamp contains more than 30% empty pixels (by 2 if 2 images show this, etc..)
- Galapagos now uses a different routine (perl) to shut down galfit instead of the old kill_galfit.pro. The time limit now works even if only one core is being used. Requires perl to be installed on the machine!
- removed "execute commands" from several routines, e.g. disabled them when using only one core (e.g. on HPC in runtime mode when execute causes the code to crash otherwise as it is disabled). In the same process, replaced delvarx.pro with a version that does not need the execute command to work
- some minor bug fixes (if the code ran, everything was correct, but code could crash under certain circumstances)
- fixed bug in PSF routine that now allows to use one PSF per band

**v2.1.5**
- Fixed one minor bug, which (in case of bad setup) can cause the code to crash. Should not affect anyone
- Removed duplicate information from the output catalogue
- Reordered columns in the output catalogue into a more sensible way
- Added README_catalogue.md in the EXAMPLE folder, which explains all columns in the output catalogue

**v2.1.6**
- some edits to save disk space, e.g. mask images are now saved in BYTE format, rather than full FLOAT
- the galapagos version is now saved in the output catalogue for version control issues along with the GalfitM version used for each fit.
- the 'weight' images for each image can now be BYTE (0/1) format as well, in order to save disk space. As they are only used in the main part of the code to flag bad pixels, this is possible even at older versions. This, however, is DIFFERENT for the SEXTRACTOR weights. SExtractor DOES care about the actual weight values, so the user should take care with these.
- added some more flexibility to the models. It's now possible to run B/D with a higher factor in a parameter than the single-sersic fits.
- along the same lines, Bulge and Disk now behave differently (so far many parameters, x,y,mag) werehardwired to behave the same way.
- In this context, Galapagos now makes the start values uniform if DOF==1 is chosen for a parameter as GalfitM requires.

**v2.1.7**
- 2 bug fixes
- Galapagos now takes the expsosure time for the SExtractor images from the setup file, rather than from the image header. While this is more complicated, it is more in line with the other filters and allow more flexibility for different surveys (we encountered that some software needs the header keyword to be different, which is avoided this way).
- BUG FIX & NEW INPUT: The input file now requires a line E25), which is the minimum number of 'good' bands (bands with data) in order for the fit to actually be carried out. In previous versions, it could happen that Galapagos restricts the maximum number of degrees of freedom to 0. This would mean that the galfit fit is being carried out, but without free parameters. This would write out a galfit 'output' file, with all the model parameters being the input parameters, mostly as derived by SExtractor. If this object then becomes a secondary for a different object, Galapagos would hold its parameters fixed at these values, which is obviously a very bad idea. Not carrying out the fit will prevent this, the object would be impemented as an object with free parameters. However, such cases should BEST be dealt with in the sextractor mask/weight, such that this problem does not even appear. This feature is a security measure to prevent the worst. These objects can be identified in the Galapagos output catalogue by having flag_galfit (or flag_galfit_bd) == -1
- There's a new, improved script to test the installation of IDL_IDLBRIDGE, essential for running Galapagos on multiple cores. .

**v2.1.8**
- bug fix: If Galapagos can not determnine a sky background (e.g. input tiles are too small, masks are too big, objects too dense,...), it uses fall-back values instead. One of these values was (useful in single-band fitting) the SExtractor values. As in multi-band fitting, this value will be from some co-added image (or a different band in most cases), this is not very wise. Instead, Galapagos avoids this now by using a median[skypixels] in the entire image if this happens.
- added flag /bridgejournal to Galapagos. This writes a journal for all bridge processes into files (in a 'journal' folder in the output folder), which is handy for debugging. WARNING! As these files will contain the GalfitM output stream, these files become HUGE (>1MB per galaxy fit in my test examples, so easily hundreds of MB or even GB for large surveys), which at some point will slow down the fitting. Thi feature should hence only be used for debugging purposes and NOT once the code succesfully runs. Please restart in this case without this flag being set.
- (see utilities folder and README therein) added a script to run automatic single-vs-multi-band comparison (single_band_comparison.pro and two little scripts needed by this to run) for any galapagos style (sky first, then objects) GalfitM setup file. The only input needed is the multi-band file, all information is found in there. The fits will still need to find the input files, obviously, so needs to be run on the same machine. The script is trivial to run, does not interfere with Galapagos output files and creates both a little FITS table with the comparision results for the object and, if wanted, a simple plot showing the comparison. This script is NOT run from within GALAPAGOS, but can easily be run by a user. Please look at the script for more details and options.
- removes PA constraints from constraints file as those are not needed anymore in GalfitM

**v2.2.0**
- The system introduced in 2.1.7 for reducing the DOF in case of masked images did not work in crowded fields, as in most cases, MOST of the images will be masked out. The scheme has now been changed so it only analyses the pixels within the primaries SExtractor ellipse. This makes setup parameter E23 mean something else (fraction of masked pixels within the primary ellipse), E24 obsolete and renames E25->E24. Sorry for the trouble. I thought the old scheme had fixed the problems, which it had in shallow data. In deep data, it had not.
- There is more output for progress

**v2.2.1**
- this update comes with a change in setup file, E18 (used for different purpose) and F07 (moving old F07 and following backwards by 1 number).
- BUGFIX! In galapagos-2.2.0, the B/D part is broken. This has been fixed again. Sorry for that, I missed a typo.
- incorporating recent changes to GalfitM (v 1.2.0):
    - as the galfit.xx files are now being named in a more useful fashion
    - GalfitM v1.2.0 and Galapagos-2 v 2.2.1 allow to define the format of the GalfitM output file. This is useful to either save diskspace or to set up the GalfitM/Galapagos output files for further analysis. In E18 and F07, the user can now specify the format of the galfit output files. This is done in the same string format that Galfit itself uses (the string is in fact, only passed on to Galfit).
However, NOT ALL (!) galfit values are allowed here!
        - Valid input: blank (as layer 1 in normal Galfit3 (one band)), input, model, residual, component (individual model components, only useful in case of B/D), sigma (sigma image, either input or created), psf (input psf image), none (no images output, just results fits tables. Useful to save diskspace, Galapagos is unaffected and all imaging information is actually already stored in other files). No matter which order the user puts these, they will always appear in the above order and can easily be identified and accesses by their layer names.
DEFAULT: blank,input,model,residual (as assumed by GalfitM if no input is specified)

        - POSSIBLE, BUT DON'T MAKE SENSE (as non-parametric fitting is not utilized in Galapagos): nonparam (nonparam image), datasub (input minus nonparam image)
        - NOT ALLOWED (!!): itertable (table of parameters at each iteration) as this woudl create an output file straight away, which will confuse Galapagos if the fit crashes at a later stage.

**v2.2.2**
- Galapagos itself was unchanged
- provide some more useful utilities/scripts in the utilities folder with README that can be used to:
   - check_idl_bridge.pro: check that the IDL bridge works as needed to run on >1 cores
   - clean_galfit_folder.pro: clean the galapagos output folder after e.g. a system crash
   - kill_galfit.pro: kill running galfits on your machine
   - package_objects.pro: package up individual objects (or several) in order to send them to collaborators. 
   - single_band_comparison.pro:	run a comparison of sinngle-band to multi-band galfit fitting

Please read the README in the utilities folder for details of what the codes do and how to run them

**v2.2.3**
- Galapagos creates some more ds9 region files (for cold, hot and combined for each tile individually)
- Added 3 more utility scripts.
    - 2 new utilities to re-create galfit output from
        - re-running the fit with different file content (slow)
        - running galfit from the galfit.??.band file with fixed parameters (fast)
        - These enable the user to run galapagos without output images (setting E18 and F07). If required for further analysis at a later stage, the images, residuals, sub-component etc can be created at a later stage using these scripts.
    - tiling_helper.pro can be run on an intermediate Galapagos output catalogue (check the utility readme file) and displays the number of objects to be fit in each input tile. This should make finding an optimal batch setup somewhat easier if more parallel capacity is required.

**v2.2.4**
- create_output_from_fits.pro uses different standard. It now starts from the galfit.xx file instead of galfit.xx.band (as this is more precise) and as default now renames the output file.

**v2.2.5b**
- One major, two small bugfix:
    - Major: In case the single-sersic output files do not contain any images (as possible and advised now), the B/D fits would all crash, as one value is read out from the model image header. This number is now read out from somewhere else so this causes no issues anymore.
    - Minor:
        - a problem occurred in 1-band fits if some objects had sextractor magnitude 99. This has been fixed now.
        - Galapagos can now work well with the (official) galfit3 (by Chien Peng) again

**v2.2.7**
- Fixed a bug in which all sky values turned out to be ==0 as the value was only printed to 3 digits behind the comma/dot. Critical for sky subtracted data in cts/s. Instead, the code now uses >4 SIGNIFICANT digits.
- added new utility script to automatically derive the colour offsets that need to be defined in file A00). Useful for easy setup
- Smaller changes:
    - if the target list does not exist, a warning is printed. 
    - option \galfitoutput now sends the galfit output into a [ID].out file for checking on e.g. long running or crashed fits
    - tighter constraints on objects xmax -> xmax/10. for objects ON the postage stamp xmax -> xmax/5. for objects outside the postage stamp ("contributing sources"). This should keep secondary objects from running into the primary or to a position outside of the postage stamp.
    - update to tiling_helper.pro to return more information to split the survey up into batches.
    - galapagos now checks that input images and weights exist. Helps to find typos in image names before running the code.

**v2.2.8**
- now calculating all mean values in double precision. Otherwise, this could have lead to significantly wrong values, e.g. in the sky determination in scertain rare cases. Changed throughout the code.
- added a missing procedure from the astroloib to make the galapagos package stand-alone
- updated the procedures to determine the sky values to work even with very few pixels or a sky value distribution too narrow to fit a gaussian to it, which caused the code to crash.
- fixed a small bug in which ar_b>0.6 for starting values was not imposed if DOF>1 for this parameter.
- properly disabled the usages of IDL_BRIDGE when only 1 core is used (D18 == 1), to avoid the code to crash.
- updates to USAGES.md, especially regarding how to calculate the sigma images when creating them by hand and feeding them into Galapagos-2
- cosmetic changes to some output, e.g. for marking wrong detections
- some changes to utility scripts
