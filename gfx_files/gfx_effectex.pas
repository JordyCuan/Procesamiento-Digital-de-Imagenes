{***********************************************************************
Unit gfx_EffectEx.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi 4

    gfx_EffectEx is part of the gfx_library collection

    You may use this sourcecode for your freewareproducts.
    You may modify this source-code for your own use.
    You may recompile this source-code for your own use.

    All functions, procedures and classes may NOT be used in commercial
    products without the permission of the author. For parts of this library
    not written by me, you have to ask for permission by their
    respective authors.

    Disclaimer of warranty: "This software is supplied as is. The author
    disclaims all warranties, expressed or implied, including, without
    limitation, the warranties of merchantability and of fitness for any
    purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this software."

    All brand and product names are marks or registered marks of their
    respective companies.

    Please report bugs to:
    Andreas Moser  amoser@amoser.de

********************************************************************************}
unit gfx_effectex;
interface

uses gfx_Effects;

// Standard filters & examples

//******************************************************************************
//
//  Linear filters
//
//******************************************************************************
const cnt_predefined_linflt = 32;
      cnt_predefined_multipass = 2;

var mxEmbossColor:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
        Matrix:
        (( 0, 0, 0, 0, 0, 0, 0),
        ( 0, 0, 0, 0, 0, 0, 0),
        ( 0, 0,-1,-1,-1, 0, 0),
        ( 0, 0, 0, 1, 0, 0, 0),
        ( 0, 0, 1, 1, 1, 0, 0),
        ( 0, 0, 0, 0, 0, 0, 0),
        ( 0, 0, 0, 0, 0, 0, 0));
        Divisor:1;
        Bias:0;
        FilterName:'Emboss color (Effects linear)';);

var mxEmbossLight:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
        Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0,-1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
        Divisor:1;
        Bias:192;
        FilterName:'Emboss light (Effects linear)';);

var mxEmbossMedium:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
        Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
        Divisor:1;
        Bias:192;
        FilterName:'Emboss medium (Effects linear)';);

var mxEmbossDark:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
        Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
        Divisor:1;
        Bias:128;
        FilterName:'Emboss Dark (Effects linear)';);

var mxEdgeEnhance:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
        Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0,-2,16,-2, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
        Divisor:4;
        Bias:0;
        FilterName:'Edge enhance (Sharpen linear)';);

var mxBlurBartlett:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx7;
        Matrix:
      (( 1, 2, 3, 4, 3, 2, 1),
      ( 2, 4, 6, 8, 6, 4, 2),
      ( 3, 6, 9,12, 9, 6, 3),
      ( 4, 8,12,16,12, 8, 4),
      ( 3, 6, 9,12, 9, 6, 3),
      ( 2, 4, 6, 8, 6, 4, 2),
      ( 1, 2, 3, 4, 3, 2, 1));
        Divisor:256;
        Bias:0;
        FilterName:'Blur Bartlett (Blur linear)';);

var mxBlurGaussian:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx7;
        Matrix:
     (( 1, 4, 8, 10, 8, 4, 1),
      ( 4,12,25,29,25,12, 4),
      ( 8,25,49,58,49,25, 8),
      (10,29,58,67,58,29,10),
      ( 8,25,49,58,49,25, 8),
      ( 4,12,25,29,25,12, 4),
      ( 1, 4, 8, 10, 8, 4, 1));
      Divisor:999;
      Bias:0;
      FilterName:'Blur Gaussian (Blur linear)';);

var mxNegative:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0,-1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:255;
      FilterName:'Negative (Effects linear)';);

var mxAverage:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 1, 1, 0, 0),
      ( 0, 0, 1, 1, 1, 0, 0),
      ( 0, 0, 1, 1, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:9;
      Bias:0;
      FilterName:'Average (Blur linear)';);

var mxBlur:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 2, 4, 2, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:16;
      Bias:0;
      FilterName:'Blur (Blur linear)';);

var mxBlurSoftly:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 3, 1, 0, 0),
      ( 0, 0, 3,16, 3, 0, 0),
      ( 0, 0, 1, 3, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:32;
      Bias:0;
      FilterName:'Blur softly (Blur linear)';);

var mxBlurMore:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx5;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 1, 4, 6, 4, 1, 0),
      ( 0, 2, 6, 8, 6, 2, 0),
      ( 0, 1, 4, 6, 4, 1, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:64;
      Bias:0;
      FilterName:'Blur more (Blur linear)';);

var mxPrewitt:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 1, 1, 0, 0),
      ( 0, 0, 1,-2, 1, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Prewitt (Edge detect linear)';);

var mxTraceContour:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
      (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-6,-2,-6, 0, 0),
      ( 0, 0,-1,32,-1, 0, 0),
      ( 0, 0,-6,-2,-6, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:240;
      FilterName:'Trace contour (Edge detect linear)';);

var mxSharpen:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0,-1,16,-1, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:8;
      Bias:0;
      FilterName:'Sharpen (Sharpen linear)';);

var mxSharpenMore:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0,-1,12,-1, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:4;
      Bias:0;
      FilterName:'Sharpen more (Sharpen linear)';);

var mxSharpenLess:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0,-1,24,-1, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:16;
      Bias:0;
      FilterName:'Sharpen less (Sharpen linear)';);

var mxUnSharpMask:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0,-2,16,-2, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:4;
      Bias:0;
      FilterName:'Unsharp mask (Sharpen linear)';);

var mxEdgesStrong:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 3, 1, 0, 0),
      ( 0, 0, 3,-16,3, 0, 0),
      ( 0, 0, 1, 3, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Edges strong (Edge detect linear)';);

var mxEdgesWeak:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 1, 0, 0, 0),
      ( 0, 0, 1,-4, 1, 0, 0),
      ( 0, 0, 0, 1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Edges weak (Edge detect linear)';);


var mxEtch:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-6, 2,-6, 0, 0),
      ( 0, 0,-1,32,-1, 0, 0),
      ( 0, 0,-6,-2,-6, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:240;
      FilterName:'Etch (Effects linear)';);

var mxLaplacianHV:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0,-1, 0, 0, 0),
      ( 0, 0,-1, 4,-1, 0, 0),
      ( 0, 0, 0,-1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Laplacian horz./vert. (Edge detect linear)';);

var mxLaplacianOmni:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0,-1, 8,-1, 0, 0),
      ( 0, 0,-1,-1,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Laplacian omnidir. (Edge detect linear)';);

var mxSharpenDirectional:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-3,-3,-3, 0, 0),
      ( 0, 0, 0,16, 0, 0, 0),
      ( 0, 0, 1, 1, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:10;
      Bias:0;
      FilterName:'Sharpen directional (Sharpen linear)';);


var mxSobelPass:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 2, 1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0,-1,-2,-1, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Sobel pass (Edge detect linear)';);

var mxGlow:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx7;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 1, 2, 2, 2, 1, 0),
      ( 0, 2, 0, 0, 0, 2, 0),
      ( 0, 2, 0,-20,0, 2, 0),
      ( 0, 2, 0, 0, 0, 2, 0),
      ( 0, 1, 2, 2, 2, 1, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:8;
      Bias:0;
      FilterName:'Glow (Effects linear)';);

var mxWaggle:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx7;
      Matrix:
     (( 0, 1, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 1),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 1, 0, 0, 0, 0));
      Divisor:3;
      Bias:0;
      FilterName:'Waggle (Effects linear)';);

var mxPattern:TGraphicFilter
      =(FilterType:ftLinear;MatrixSize:mx5;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, -4, -9, -4, 0, 0),
      ( 0, -4, -24, -1, -24, -4, 0),
      ( 0, -9, -1, 168, -1, -9, 0),
      ( 0, -4, -24, -1, -24, -4, 0),
      ( 0, 0, -4, -9, -4, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Pattern (Edge detect linear)';);

//******************************************************************************
//
//  Two-Pass filters
//
//******************************************************************************

var nmxLaplacianInvert:TMultiPassGraphicFilter=
      (FilterType:ftMultiPass;
       Filters:(@mxLaplacianOmni,@mxNegative,@mxZero,@mxZero);
       Functions:(gaNone,gaNone,gaNone);
       FilterName:'Laplacian Negative');

var nmxChurchPattern:TMultiPassGraphicFilter=
      (FilterType:ftMultiPass;
       Filters:(@mxZero,@mxPattern,@mxZero,@mxZero);
       Functions:(gaAverage,gaNone,gaNone);
       FilterName:'Church Pattern');

implementation

end.
