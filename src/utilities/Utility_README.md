## GALAPAGOS-2 Utility README (v.2.2.1)

This file explains the utility programs provided with Galapagos in the /src/utilities folder.
These scripts can be used when **restarting the code**, e.g. after a system crash or for quick analysis/evaluation after the run.
Please read this file before you use any of the utilities.

Please note that all these scripts are somewhat experimental and are expecially aimed at my own naming conventions of files 
(e.g. Galfit start files being called '\*obj'). Please be somewhat careful when using them.
They are, however, all very simple scripts and should be easy enough to adapt to your own needs.

Before you run the script, make sure they are in your IDL path, or complie them by hand.

### check_idl_bridge.pro
Before you try to run Galapagos using multiple cores, you have to make sure you can actually RUN Galapagos/Galapagos-2
on your computer this way. E.g. IDL (at least v6.4) has to be installed properly, including (and especially) IDL\_IDLBRIDGE. 
Galapagos will work without this feature, as long as only 1 processore is used (D18 == 1), but will not work for parallelization,
which is obviously helpful when running on big surveys.

ckeck_idl_bridge.pro allows you to test this (and works fine on all machines I have tried them on)

Simply run the script like this:

IDL>>  check_idl_bridge

The script tries to write a few  files into your home directory (e.g. ~/IDL\_bridge\_works, and similar), 
and deletes them again at the end, printing the result)
The script can return 3 outcomes:

    - Bridge seems to work fine with \nowait command (necessary)
    - Bridge seems to work fine without \nowait command
    - Bridge seems not to be working

Ideally, you'd want the first one, the other 2 will give you a hint where the problem is.
Please work with your IT department to solve this. Due to the number of different system setups out there, I can't give much
help myself.
E.g. I have seen systems where the IDL\_Bridge works when IDL is started in a certain folder, but not when started anywhere else, which is
something I do not understand (not beeing a sysadmin myself)


### clean_galfit_folder.pro
(If used, please use *before* kill_galfit.pro, reason see below)
In case you need to restart Galapagos because either you stopped it or your computer/server crashed, clean\_galfit\_folder.pro 
can help it to continue where it left off.

IDL>> clean_galfit_folder, '/path/to/your/output/folder'   ;(the content of setup file A01)

The script removes all files connected to the objects that were currently busy either in the IDL\_Bridge itself or in Galfit, by:
   - looking for *sav files in the galfit folders  
   - removeing the \*sav file and the \*obj file of the same object  
   - (and leaves everything else like masks,constraint files, …. These files will be overwritten then anyway)

If you don’t delete these files (most importantly the *obj file), Galapagos-2 will think that the objecthas been started 
but the fit had crashed! It will then accordingly only ‘read’ out the result (e.g. the sky values) and set flag_galfit == 1 
in the output catalogue, but it will NOT retry fitting this object!

When you then restart Galapagos with the same setup script as before, it will -- instead of fitting them -- read in the result 
from all objects that already exist, and will redo the ones that were active at the time (and which were deleted with this script).

Please note that the new run will NOT be completely identical to the old run.
Due to the blocking of 'close' objects in the Galapagos runs, a different order of objects can be realized in the second run.
This should -- given a sensible setup -- not influence the fittingresults significantly.

*Warning!*
If you changed the setup meanwhile and fits might change (e.g. postage stamp sizes or deblending decisions), 
you need to delete ALL \*obj and \*sav files in the /tile*/galfit/ folders *manually*, effectively fitting all objects again.


IDL>> clean_galfit_folder, '/path/to/your/output/folder', \bd
does the same job during the B/D stage of the code (assuming the galfit output folder is called galfit\_bd\* )

There is a second, more strict version to start this script:

IDL>> clean_galfit_folder, '/path/to/your/output/folder', max_file_age=xx
(where xx is time in hours)

This way, the script *additionally*:
    - looks for all \*obj files newer than xx hours
    - deletes all files connected to those objects.

This deletes additional files and was found usefule when running Galapagos on different machines, but on the same survey/hard-drive.
Additional to the above, this script also deletes all files connected to objects that 'crashed' within the given time.

It was found that occasionally (rarely) when IDL fails (e.g. due to lack of memory), it creates \*sav and \*obj files, but galfit 
doesn't properly carry out the fit. Leaving these files unchanged, Galapagos will again thing the fit crashed, when in fact it 
wasn't properly tried. These objects will simply be done again (and will fail again if they had failed before).


### kill_galfit.pro
(use *after* clean_galfit_folder.pro)
In case you want/need to kill Galapagos during a run (to reboot the machine or because one of your computers crashed),
you can use this script to kill all GALFITs running on that machine.
It's equivalent to killing them in 'top'.

IDL>>  kill_galfit, task, time_limit, mac=mac
where 'task' is the name of the galfit that you can see in 'top', time_limit is the time_limit in minutes (0 doesn't work, 
has to be >=1) and \mac is a flag if you're running on a MAC machine (as the syntax to kill processes is slightly different).

Importantly, this script should *only* be run *after* clean_galfit_folder and after you killed (Ctrl^c) Galapagos.
Otherwise, Galfit stops, but Galapagos will simply start a new one or Galapagos will delete the \*sav file itself, making it 
impossible for clean_galfit_folder.pro to identify the active objects and clean the folder properly!


### package_objects.pro
This scipt can be used to package up one (or several) files into a tar file to be send to collaborators and/or for debugging.

IDL>> package_objects, ['path/to/\*obj_1','path/to/\*obj_2'], 'path\to\new\folder', notar=notar
or for 1 file alternatively
IDL>> package_single_object, 'path/to/\*obj_1', 'path\to\new\folder', notar=notar

It will analyse the \*obj file and find ALL files necessary to carry out the fit of said object and package them up in a tar
file (unless specified with \notar, in which case it will remain a folder).
the script will
    - copy all files needed into the new folder including constraint files, PSFs, images, masks,... the lot)
    - adapt the \*obj file by removing the paths of all filenames (a new *obj_adapt is creatd that can be run on the spot)

This makes it trivial to send one or multiple tiles to collaborators for further analysis and to people for demonstration/debugging
purposes.
This script had been tested with several versions of Galapagos an *should* also work on non-galapagos created files (but untested).


### single_band_comparison.pro
(might need compiling by hand due to bad file/pro naming)
This script runs automatic single-vs-multi-band comparison for any galapagos style (sky first, then objects) GalfitM setup file. 
It's hence a handy script to create a simple comparison plot for talks and demonstration of what multi-band fitting does.

The only input needed is the multi-band file, all information is found in there. 
The new fits (if carried out) will still need to find the input files, obviously, so this needs to be run on the same machine as 
Galapagos. 

IDL>> single_comparison_all, 'path/to/\*obj_1', galfit='path/to/galfit/executable'
IDL>> single_comparison_all, 'path/to/\*obj_1_bd', galfit='path/to/galfit/executable',/bd

Naming the galfit executable starts the actual fits (instead of only writing the start files).
/bd indicates that this is a B/D fit, which creates a different plot and results file.

The overall script carries out several steps, which can also be used individually (please check single_band_comparison.pro to see 
how)
The script creates both a little FITS table with the fit results for the object and, if wanted, a simple plot (ps file) 
showing the comparison if multi-band with single-band fitting results. 
This script is NOT run from within GALAPAGOS, but can easily be run by the user at a later stage. 

All files are created in the same folder as the \*obj file used to start the script and get *similar* names.
These files do *not* interfere with Galapagos output files and hence can be run while Galaagos is still running, if desired.

### create_output_from_obj.pro and create_output_from_fits.pro
These scripts allows the user to re-create the galfit output files again, but with different content.

Starting from either an object file or a fits file (to be exact, this routine uses the *galfit.??.band file matching that filename, which has the highest number, e.g. from the run which CREATED the fits file), they allow the user to re-create the galfit results, but allows for different components in the output file.

create_output_from_obj.pro actually re-runs the fit, with different output layers switched on, hence is pretty slow, especially for objects with many neighbours.
create_output_from_fits.pro starts where the fit finished and holds all values fixed (hence only creating the output itself), hence is pretty fast.

These routines are handy as they allow the user to run galapagos itself with creating only the minimal neccessary output in the galfit output file, saving a VAST amount of disk space (especially when compared to 'components' being used).
The user can create the needed output (e.g. components) easily with these scripts only for the objects where they are actually required, e.g. when used in any further analysis.

WARNING: When using create_output_from_fits.pro without renaming the output file (keyword 'namepost') the fit will overwrite the original GALFIT output file. While this is not generally a problem, it BECOMES a problem when Galapagos is still running or could be re-run. THe fit results will not change, but some of the other parameters will, e.g. degree of freedom in the fit (for obvisou reasons). So if there's a chance that you'd need to read out the results from GALFIT again using Galapagos, please rename your files and look at the renamed ones instead!!
