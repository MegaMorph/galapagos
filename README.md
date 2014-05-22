##Galapagos-2
 ---
This is the multi-band Galapagos version.

*Galapagos: Galaxy Analysis over Large Areas: Parameter Assessment by GALFITting Objects from SExtractor*

See [here](http://astro-staff.uibk.ac.at/~m.barden/galapagos/) for details of the original version, described in this paper: [Barden, Häußler et al, 2012, MNRAS,422,449](http://adsabs.harvard.edu/abs/2012MNRAS.422..449B)

The MegaMorph project (specifically Häußler, Bamford and Vika) has added many new features to this code: (selection)
- Galapagos can now use multi-band data simultaneously  
- Galapagos can now do Bulge-Disk decomposition  
- Galapagos can now deal with more than one PSF for a survey, making it more usable for ground-based surveys  
- Galapagos can now use CPU time more efficiently. The code itself is slightly quicker, but especially the queuing system is new and more effective  
- Galapagos can now pass on SIGMA images to Galfit/GalfitM, if the user requires this  
- Galapagos can now use RMS maps rather than WEIGHT maps for the SExtractor step  
- Some parameters (e.g. more constraints) are now user inputs, instead of being hard-wired into the code  
- Many other, small changes, mostly for cleaning up the code, increases efficiency, computation speed-up  


IDL 6.4 (which intriduces IDL\_IDLBRIDGE) is strictly required to run this software. Recent developements have mainly been tested on IDL8, but IDL7 has been shown to work.  
Perl is needed for this code to run properly.  
For an extensive README and help of usage, please read USAGE.md in the EXAMPLE_AND_README folder!  

If you have additions, reqeust or questions to/about this code, please email us under BorisHaeussler.astro@gmail.com

This code is used in the following papers:  
[Häußler et al., 2013, MNRAS, 430, 330](http://adsabs.harvard.edu/abs/2013MNRAS.430..330H)
