GFX_LIBRARY v1.2 beta 

(c) Andreas Moser, amoser@amoser.de

GFX_LIBRARY is a collection of graphic tools.
GFX_LIBRARY is freeware, for details see below. (Item "ATTENTION")
If you use GFX_LIBRARY I just want to be mentioned in your application or 
in the documentation of the application.
Anyway, feel free to send me a email if you like...

Delphi version : Delphi  4 (maybe 3 and 5 also...not tested)

WHY
-------------------------
The reason for coding such a library for free is easy to explain.
If you look at Linux and see all the open source available for it you may unterstand
my motivation. Because I think such a powerful and fast programing language like Delphi
needs also support by the users. I don´t know wether my code is useful or not, but I´ve
never seen a such package anywhere in the web. I hope you enjoy it.

INSTALLATION:
-------------------------

Just put all the gfx_*.* files into your LIB directory ( or in another one,
then you must set the path-specs in your delphi options)
The LIBPNG.DLL should be copied into the WINDIR, e.g. C:\WINDOWS 
NOTE: Delphi JPEG support must be installed.
      GIF SUPPORT NEEDS TGIFIMAGE (I USE V2.2) FROM ANDERS MELANDER, ANDERS@MELANDER.DK
      AVAILABLE E.G. AT WWW.TORRY.RU. TGIFIMAGE IS NOT INCLUDED IN THIS PACKAGE. 
      IF YOU USE TGIFIMAGE, YOU MUST SET THE DIRECTIVE `GIF´ IN THE PROJECT OPTIONS
After that the 2 components "PreviewPanel" and "ExtTrackBar" must be installed.
The components are not needed for gfx_library itself, just for the example program.
These 2 components may be used in your freeware applications without any permission from me,
but I will give no support for them, neither yet nor later.


DOCUMENTATION
-------------------------

Sorry that the documentation is very bad but I had no time to do that.
But remember: It´s a very hard work (It took a long time up to now coding all the effects
and combining the codes of other freeware-authors with my project). I think the example
program is easy enough to teach you in the functionality of this package...


NOTE FOR DEMO-PROJECT:
-------------------------

Install these two components first:
	- TPreviewPanel
	- TExtTrackBar
(Both included in this archive)

New in v1.2 the flw components:
	- TExtImage
	- TCstatBar
	- TColorWheel
Just check them out!

FEATURES
-------------------------

        - Graphic fileformat support

          // NOTE : The File format converters are still part of this package (stored in the file
                    "gfx_converters.zip". But unfortunately I have no time to implement more file format
                    support. I suggest to use third party products instead. The most powerful package I found
                    is "GraphicEx" from M.Lischke (http://www.lischke-online.de". Currently it is freeware for
                    uncommercial use and NOT included in gfx_lib.
                    The converters shipped with gfx_lib are left for educational purpose.

          - Native converters
            - TIFF
              - Load: b/w, 16 colors , 256 colors, 24 bit truecolor
              - Save: b/w, 16 colors , 256 colors, 24 bit truecolor
              - Uncompressed and Packedbits decoding (encoding uncompressed only at the moment)
              // tiff-lzw removed to avoid copyright problems
            - TGA
              - Load: 256 colors, 24bit truecolor
              - Save: 256 colors, 24bit truecolor
              - Uncompressed and packed decoding (encoding uncompressed only at the moment)
            - PCX
              - Load: 256 colors, 24bit truecolor
              - Save: 256 colors, 24bit truecolor
              - Uncompressed and packed decoding (encoding uncompressed only at the moment)
            - PCD
              - Loading
          - Supported Filetypes needing external components
              - PNG Files (LIBPNG.DLL needed)
              - GIF (TGIFImage needed)

        - Resampling
	  - Stretching
          - Spline filter
          - Bell filter
          - Triangle filter
          - Box filter
          - Hermite filter
          - Lanczos3 filter
          - Mitchell filter

        - Enlarge Canvas
        - Crop

        - Rotating
          - 90, 180, 260 degrees
          - Free rotating
          - Auto enlarge

        - Colors
          - Colorspace conversions and support
            - RGB
            - RGBA
            - BGRA
            - CMYK
            - CMY
            - HSV
            - HSL
            - CYcYr  (alpha stadium)
            - CIELAB (alpha stadium)
          - Histogram functions
          - Channel splitting
          - Statistics
          - Palette routines
	  - Gamma correction
	  - Brightness / contrast
	  - r/g/b and h/s/v correction
	  - Solarization
	  - Posterization
	  - Stretching
	  - Colorization
	  
        - Effects
	  - Antialiasing
	  - Grayscale
	  - Add noise
          - Linear / Static filters
            - Many predefined filters
              - Emboss (light,medium, dark)
              - Sharpen
              - Blur (Gaussian, Bartlett etc.)
              - Soften
              - Edge detection
              - Trace contour
              - Average
              - Minimum, Median Cut, Maximum
              - Laplacian
              - etc.
            - Userdefined filters possible
          - Multipass filters
          - Image arithmetic
            - logical operations (AND, OR, XOR etc.)
            - substract, multiply etc.
            - blending
          - Deformations
            - Fisheye
            - Sinus
            - Whirl
            - Sand
            - Spray
            - Mosaic

    UPDATES
    v1.1 04/2001
                 - bugfix gfx_errors (for delphi 5 compatibility)
                 - update function SetBmpEqual
                 - new unit gfx_crypt
                 - new predefined filters in gfx_effectexd (glow, waggle, pattern, churchpattern)
    v1.2 08/2001 - new unit gfx_mask


        ... to be continued


ATTENTION:
-------------------------
    YOU MAY USE THIS SOURCECODE FOR YOUR FREEWAREPRODUCTS.
    YOU MAY MODIFY THIS SOURCE-CODE FOR YOUR OWN USE.
    YOU MAY RECOMPILE THIS SOURCE-CODE FOR YOUR OWN USE.

    ALL FUNCTIONS, PROCEDURES AND CLASSES MAY NOT BE USED IN COMMERCIAL
    PRODUCTS WITHOUT THE PERMISSION OF THE AUTHOR. FOR PARTS OF THIS LIBRARY
    NOT WRITTEN BY ME, YOU HAVE TO ASK FOR PERMISSION BY THEIR
    RESPECTIVE AUTHORS.

    DISCLAIMER OF WARRANTY: "THIS SOFTWARE IS SUPPLIED AS IS. THE AUTHOR
    DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED, INCLUDING, WITHOUT
    LIMITATION, THE WARRANTIES OF MERCHANTABILITY AND OF FITNESS FOR ANY
    PURPOSE. THE AUTHOR ASSUMES NO LIABILITY FOR DAMAGES, DIRECT OR
    CONSEQUENTIAL, WHICH MAY RESULT FROM THE USE OF THIS SOFTWARE."

    ALL BRAND AND PRODUCT NAMES ARE MARKS OR REGISTERED MARKS OF THEIR
    RESPECTIVE COMPANIES.

    Any parts of source code not completely written by me are mentioned in the
    units. (So far i know the authors...if i have forgotten somebody, please tell me,)



    Please report bugs and improvements / inspirations to:
    Andreas Moser  amoser@amoser.de


