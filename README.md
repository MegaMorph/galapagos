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
- Galapagos can now be run on data with different coverage in different bands
- Some parameters (e.g. more constraints) are now user inputs, instead of being hard-wired into the code  
- Thanks to changes in GalfitM, it is now possible to store only the fit results, rather than all images, saving large amounts of disk space. Utility tools are provided to create the 'normal' output again.
- Many other, small changes, mostly for cleaning up the code, increases efficiency, computation speed-up  


IDL 6.4 (which intriduces IDL\_IDLBRIDGE) is strictly required to run this software. Recent developements have mainly been tested on IDL8, but IDL7 has been shown to work.  
Perl is needed for this code to run properly.  
For an extensive README and help of usage, please read USAGE.md in the EXAMPLE_AND_README folder!  

If you have additions, request or questions to/about this code, please email us under BorisHaeussler.astro@gmail.com

This code or previous version thereof is used in the following papers:  
[Häußler et al., 2013, MNRAS, 430, 330](http://adsabs.harvard.edu/abs/2013MNRAS.430..330H) (please cite when using the code)
[Lani et al., 2013, MNRAS, 435, 207](http://adsabs.harvard.edu/abs/2013MNRAS.435..207L)
[Cerulo et al., 2014, MNRAS, 439, 2790](http://adsabs.harvard.edu/abs/2014MNRAS.439.2790C)
[Vulcani et al., 2014, MNRAS, 441, 1340](http://adsabs.harvard.edu/abs/2014MNRAS.441.1340V)
[Kennedy et al., 2015, MNRAS, 454, 806](http://adsabs.harvard.edu/abs/2015MNRAS.454..806K)
[Maier et al., A&A, 590, 108](http://adsabs.harvard.edu/abs/2016A%26A...590A.108M)
[Kennedy et al., 2016, MNRAS](http://adsabs.harvard.edu/doi/10.1093/mnras/stw1176)
[Huertas-Company et al., 2016, MNRAS](http://arxiv.org/abs/1606.04952)
