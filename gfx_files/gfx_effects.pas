{***********************************************************************

Unit gfx_Effects.PAS v1.0 0799
    (c) by Andreas Moser, amoser@amoser.de,

    gfx_Effects is part of the gfx_library collection

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

    for more information about image processing look at this book:

    Computer Graphics - Principles and Practice, Foley et al

********************************************************************************}

UNIT gfx_effects;


INTERFACE

USES Windows, Graphics,classes,sysutils{,forms},comctrls,gfx_basedef,math;

TYPE

    {definiton used by EffectSwapChannels}
    TSwapPlanes= (swapNone,swapRedBlue,swapRedGreen,swapBlueGreen);

    {definition used by EffectArithmetic}
    TGraphicArithmetic=(gaNone,gaAdd,gaSubstract,gaMultiply,gaDivide,gaDarkest,
                        gaLightest,gaDifference,gaBinaryOr,gaBinaryAnd,gaAverage);

    {Static filters}
    TStaticFilterType=(sfMedian,sfMin,sfMax);

    {Filter types}
    TFilterType=(ftStatic,ftLinear,ftMultiPass);

    {Matrix size (3x3, 5x5, 7x7}
    TMatrixSize = (mxDefault,mx3,mx5,mx7);

    {TEffectMatrix used by EffectLinearFilterRGB}
    TEffectMatrix = array[0..6,0..6] of Integer;
    pEffectMatrix = ^TEffectMatrix;

    {TGraphicFilter used by EffectLinearFilterRGB /EffectStaticFilterRGB}
    TGraphicFilter = packed record
      FilterType:   TFilterType;
      MatrixSize:   TMatrixSize;     // Size: mx3=3x3, mx5=5x5, mx7=7x7
      Matrix:       TEffectMatrix;   // Every pixel of source would be calculated by the matrix
      Divisor:      Integer;         // The result of calculation would be divided by divisor (>0!)
      Bias:         Integer;         // Finally bias value would be added
      FilterName:   Array [0..128] of Char;
    end;
    pGraphicFilter=^TGraphicFilter;

    {TMultiPassGraphicFilter used by EffectMultiPassFilter}
    TMultiPassGraphicFilter=packed record
       FilterType:TFilterType;
       Filters:array [1..4] of pGraphicFilter;
       Functions:array [1..3] of TGraphicArithmetic;
       FilterName:Array [0..100] of Char;
    end;
    pMultiPassGraphicFilter=^TMultiPassGraphicFilter;

    type TDirection      = (TopToBtm, BtmToTop, LftToRgt,RgtToLft,TopLftToBtmRgt, BtmLftToTopRgt, All);

    TEffectCallBack = procedure (const Min,Max,Pos:Integer);

    procedure EffectAddNoise(SrcBitmap,DestBitmap:TBitmap;Value:Integer;MonoNoise:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectAntiAlias(SrcBitmap,DestBitmap:TBitmap;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectArithmetic(SrcBitmapOne,SrcBitmapTwo,DestBitmap:TBitmap;Arithmetic:TGraphicArithmetic;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectBlend(SrcBitmapOne,SrcBitmapTwo,DestBitmap:TBitmap;Multiplicator,MaxBlendValue:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectCircleAround(SrcBitmap,DestBitmap:TBitmap;Value:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectColorize(SrcBitmap,DestBitmap:TBitmap;Hue,Saturation,Lightness:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectContrast(SrcBitmap,DestBitmap:TBitmap;Value:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectEllipse(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectFishEye(SrcBitmap,DestBitmap:TBitmap;Value:Extended;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectGamma(SrcBitmap,DestBitmap:TBitmap;Gamma:Double;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectGreyScale(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectIncDecRGB(SrcBitmap,DestBitmap:TBitmap;dR,dG,dB:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectLightness(SrcBitmap,DestBitmap:TBitmap;Value:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectMosaic(SrcBitmap,DestBitmap:TBitmap;Width,Height:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectNegative(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectPosterize(SrcBitmap,DestBitmap:TBitmap;BitsPerChannel:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectSinus(SrcBitmap,DestBitmap:TBitmap;SinusAmpVert,VertDelta,SinusAmpHorz,HorzDelta,VertStart,HorzStart:Integer;ChngVertAtAnyCol:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectSolarize(SrcBitmap,DestBitmap:TBitmap;Threshold:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectSpray(SrcBitmap,DestBitmap:TBitmap;Value:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectStretch(SrcBitmap,DestBitmap:TBitmap;Low,High:Integer;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectSwapChannels(SrcBitmap,DestBitmap:TBitmap;WhichPlanes:TSwapPlanes;const EffectCallBack:TEffectCallBack);stdcall;

    {Filter functions: For universal processing use EffectFilter..the class looks for itself which filter will be used}
    procedure EffectFilter(SrcBitmap,DestBitmap:TBitmap;Filter:TGraphicFilter;Size:TMatrixSize;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectStaticFilter(SrcBitmap,DestBitmap:TBitmap;StaticFilterType:TStaticFilterType;Diameter:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectMultiPass(SrcBitmap,DestBitmap:TBitmap;MultiPassFilter:TMultiPassGraphicFilter;Passes:Integer;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
    procedure EffectLinearFilter(SrcBitmap,DestBitmap:TBitmap;Filter:TGraphicFilter;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;

    procedure SaveLinearFilterToFile(FileName:PChar;const Filter:TGraphicFilter);stdcall;
    procedure LoadLinearFilterFromFile(FileName:PChar;var Filter:TGraphicFilter);stdcall;
    procedure SaveMultipassFilterToFile(FileName:PChar;const Filter:TMultiPassGraphicFilter);stdcall;
    procedure LoadMultiPassFilterFromFile(FileName:PChar;var Filter:TMultiPassGraphicFilter);stdcall;



    {Default Filter (NULL-Filter}
CONST mxZero:TGraphicFilter
      =(FilterType:ftLinear;
      MatrixSize:mx3;
      Matrix:
     (( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 1, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0),
      ( 0, 0, 0, 0, 0, 0, 0));
      Divisor:1;
      Bias:0;
      FilterName:'Null (Linear)');

      {Default static filters}
      mxMedian:TGraphicFilter
          =(FilterType:ftStatic;
          FilterName:'Median (Static)');
      mxMaximum:TGraphicFilter
          =(FilterType:ftStatic;
          FilterName:'Maximum (Static)');
      mxMinimum:TGraphicFilter
          =(FilterType:ftStatic;
          FilterName:'Minimum (Static)');

IMPLEMENTATION

// -----------------------------------------------------------------------------
//
//			Blending two Bitmaps
//
//     Parameter:
//               SrcBitmapOne  : 1st Bitmap
//               SrcBitmapOne  : 2nd Bitmap
//               DestBitmap    : Result
//               Multiplicator,
//               MaxBlendValue  : The mixing of the two pictures...
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectBlend(SrcBitmapOne,SrcBitmapTwo,DestBitmap:TBitmap;Multiplicator,MaxBlendValue:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var
  bmpTwo                  :TBitmap;
  r, n, l                 :Integer;
  SrcRow1,SrcRow2,DestRow :pRGBArray;
begin
    bmptwo:=TBitmap.Create;
    try
      BMPTwo.PixelFormat:=pf24Bit;
      SetBitmapsEql(SrcBitmapOne,DestBitmap);
      BmpTwo.Width:=SrcBitmapOne.Width;
      BmpTwo.Height:=SrcBitmapOne.Height;
      if SrcBitmapTwo.Empty = TRUE then
         bmpTwo.Canvas.FillRect(Rect(0,0,SrcBitmapOne.Width,SrcBitmapOne.Height))
      else
      begin
         bmpTwo.Canvas.StretchDraw(Rect(0,0,SrcBitmapOne.Width,SrcBitmapOne.Height),SrcBitmapTwo);
      end;

      if MaxBlendValue < 1 then raise EGraphicEffects.Create('BlendRate must be between 0 AND 256');

      r:=Multiplicator;
      for n := 0 to SrcBitmapOne.Height - 1 do
      begin

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((n/SrcBitmapOne.Height)*100));

        SrcRow1 := SrcBitmapOne.Scanline[n];
        SrcRow2 := bmpTwo.Scanline[n];
        DestRow := DestBitmap.Scanline[n];
        for l := 0 to SrcBitmapOne.Width - 1 do WITH DestRow[l] do
        begin
          rgbtRed:=SrcRow1[l].rgbtRed+MulDiv(r, (SrcRow2[l].rgbtRed-SrcRow1[l].rgbtRed), MaxBlendValue);
          rgbtGreen:=SrcRow1[l].rgbtGreen+MulDiv(r, (SrcRow2[l].rgbtGreen-SrcRow1[l].rgbtGreen), MaxBlendValue);
          rgbtBlue:= SrcRow1[l].rgbtBlue+MulDiv(r, (SrcRow2[l].rgbtBlue-SrcRow1[l].rgbtBlue), MaxBlendValue);
        end;
      end;
    finally
      bmpTwo.Free;
    end;
end;

// -----------------------------------------------------------------------------
//
//			Arithmetic between two Bitmaps
//
//     Parameter:
//               SrcBitmapOne  : 1st Bitmap
//               SrcBitmapOne  : 2nd Bitmap
//               DestBitmap    : Result
//               Arithmetic    : Arithmetic type
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectArithmetic(SrcBitmapOne,SrcBitmapTwo,DestBitmap:TBitmap;Arithmetic:TGraphicArithmetic;const EffectCallBack:TEffectCallBack);stdcall;
var
  bmpTwo                  :TBitmap;
  Row, Col                :Integer;
  SrcRow1,SrcRow2,DestRow :pRGBArray;
begin
    bmptwo:=TBitmap.Create;
    try
      SetBitmapsEql(SrcBitmapOne,DestBitmap);
      BmpTwo.Width:=SrcBitmapOne.Width;
      BmpTwo.Height:=SrcBitmapOne.Height;
      BmpTwo.PixelFormat:=pf24Bit;
      if SrcBitmapTwo.Empty = TRUE then bmpTwo.Canvas.FillRect(Rect(0,0,SrcBitmapOne.Width,SrcBitmapOne.Height))
      ELSE bmpTwo.Canvas.StretchDraw(Rect(0,0,SrcBitmapOne.Width,SrcBitmapOne.Height),SrcBitmapTwo);
      for Row := 0 to SrcBitmapOne.Height - 1 do
      begin

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmapOne.Height)*100));

        SrcRow1 := SrcBitmapOne.Scanline[Row];
        SrcRow2 := bmpTwo.Scanline[Row];
        DestRow := DestBitmap.Scanline[Row];
        for Col := 0 to SrcBitmapOne.Width - 1 do
        begin
          WITH DestRow[Col] do
          CASE Arithmetic of
            gaNone:begin
                     rgbtRed:=SrcRow1[Col].rgbtRed;
                     rgbtGreen:=SrcRow1[Col].rgbtGreen;
                     rgbtBlue:=SrcRow1[Col].rgbtBlue;
                  end;
            gaAdd:begin
                    rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed+SrcRow2[Col].rgbtRed);
                    rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen+SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue+SrcRow2[Col].rgbtBlue);
                  end;
            gaSubstract:begin
                    rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed-SrcRow2[Col].rgbtRed);
                    rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen-SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue-SrcRow2[Col].rgbtBlue);
                  end;
            gaMultiply:begin
                    rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed*SrcRow2[Col].rgbtRed);
                    rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen*SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue*SrcRow2[Col].rgbtBlue);
                  end;
            gaDivide:begin
                    if SrcRow2[Col].rgbtRed<>0 then rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed DIV SrcRow2[Col].rgbtRed);
                    if SrcRow2[Col].rgbtGreen<>0 then rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen DIV SrcRow2[Col].rgbtGreen);
                    if SrcRow2[Col].rgbtBlue<>0 then rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue DIV SrcRow2[Col].rgbtBlue);
                  end;
            gaDifference:begin
                    rgbtRed:=Abs(SrcRow1[Col].rgbtRed-SrcRow2[Col].rgbtRed);
                    rgbtGreen:=Abs(SrcRow1[Col].rgbtGreen-SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=Abs(SrcRow1[Col].rgbtBlue-SrcRow2[Col].rgbtBlue);
                  end;
            gaDarkest:begin
                    rgbtRed:=MinInt2(SrcRow1[Col].rgbtRed,SrcRow2[Col].rgbtRed);
                    rgbtGreen:=MinInt2(SrcRow1[Col].rgbtGreen,SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=MinInt2(SrcRow1[Col].rgbtBlue,SrcRow2[Col].rgbtBlue);
                  end;
            gaLightest:begin
                    rgbtRed:=MaxInt2(SrcRow1[Col].rgbtRed,SrcRow2[Col].rgbtRed);
                    rgbtGreen:=MaxInt2(SrcRow1[Col].rgbtGreen,SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=MaxInt2(SrcRow1[Col].rgbtBlue,SrcRow2[Col].rgbtBlue);
                  end;
            gaAverage:begin
                    rgbtRed:=TrimInt(0,255,(SrcRow1[Col].rgbtRed+SrcRow2[Col].rgbtRed)DIV 2);
                    rgbtGreen:=TrimInt(0,255,(SrcRow1[Col].rgbtGreen+SrcRow2[Col].rgbtGreen)DIV 2);
                    rgbtBlue:=TrimInt(0,255,(SrcRow1[Col].rgbtBlue+SrcRow2[Col].rgbtBlue)DIV 2);
                  end;
            gaBinaryOr:begin
                    rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed OR SrcRow2[Col].rgbtRed);
                    rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen OR SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue OR SrcRow2[Col].rgbtBlue);
                  end;
            gaBinaryAnd:begin
                    rgbtRed:=TrimInt(0,255,SrcRow1[Col].rgbtRed AND SrcRow2[Col].rgbtRed);
                    rgbtGreen:=TrimInt(0,255,SrcRow1[Col].rgbtGreen AND SrcRow2[Col].rgbtGreen);
                    rgbtBlue:=TrimInt(0,255,SrcRow1[Col].rgbtBlue AND SrcRow2[Col].rgbtBlue);
                  end;
          end;
        end;
      end;
    finally
      bmpTwo.Free;
    end;
end;

// -----------------------------------------------------------------------------
//
//			Swap colorchannels
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Which planes : see TSwapPlanes (at the top of this unit)
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectSwapChannels(SrcBitmap,DestBitmap:TBitmap;WhichPlanes:TSwapPlanes;const EffectCallBack:TEffectCallBack);stdcall;
var Row,Col        :Integer;
    SrcRow,DestRow :pRGBArray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for Row:=0 to DestBitmap.Height-1 do
  begin

     if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

     SrcRow:=SrcBitmap.ScanLine[Row];
     DestRow:=DestBitmap.ScanLine[Row];
     for Col:=0 to DestBitmap.Width-1 do
     begin
       WITH DestRow[Col] do
         CASE WhichPlanes of
           swapRedGreen:begin
               rgbtBlue:=SrcRow[Col].rgbtBlue;
               rgbtGreen:=SrcRow[Col].rgbtRed;
               rgbtRed:=SrcRow[Col].rgbtBlue;
             end;
           swapRedBlue:begin
               rgbtBlue:=SrcRow[Col].rgbtRed;
               rgbtGreen:=SrcRow[Col].rgbtGreen;
               rgbtRed:=SrcRow[Col].rgbtBlue;
             end;
           swapBlueGreen:begin
               rgbtBlue:=SrcRow[Col].rgbtGreen;
               rgbtGreen:=SrcRow[Col].rgbtBlue;
               rgbtRed:=SrcRow[Col].rgbtRed;
             end;
         end;
     end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Greyscale Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectGreyScale(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
var Row,Col            :Integer;
    SrcRow,DestRow :pRGBArray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for Row:=0 to DestBitmap.Height-1 do
  begin

   if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

   SrcRow:=SrcBitmap.ScanLine[Row];
   DestRow:=DestBitmap.ScanLine[Row];
   for Col:=0 to DestBitmap.Width-1 do WITH DestRow[Col] do
   begin
     rgbtBlue:=RgbLightness(SrcRow[Col]);
     rgbtGreen:=RgbLightness(SrcRow[Col]);
     rgbtRed:=RgbLightness(SrcRow[Col]);
   end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Posterize Bitmap
//
//     Parameter:
//               SrcBitmap      : Bitmap to be processed
//               DestBitmap     : Result
//               BitsPerChannel : How many Bits per channel
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectPosterize(SrcBitmap,DestBitmap:TBitmap;BitsPerChannel:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var Row,Col            :Integer;
    SrcRow,DestRow :pRGBArray;
    Mask           :Byte;
begin
  if not BitsPerChannel in [1..8] then exit;
  SetBitmapsEql(SrcBitmap,DestBitmap);
  mask:=$FF;
  CASE BitsPerChannel of
    7:Mask:=$fe;
    6:Mask:=$fc;
    5:Mask:=$f8;
    4:Mask:=$f0;
    3:Mask:=$e0;
    2:Mask:=$c0;
    1:Mask:=$80;
  end;
  for Row:=0 to DestBitmap.Height-1 do
  begin

   if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

   SrcRow:=SrcBitmap.ScanLine[Row];
   DestRow:=DestBitmap.ScanLine[Row];
   for Col:=0 to DestBitmap.Width-1 do WITH DestRow[Col] do
   begin
     rgbtBlue:=(SrcRow[Col].rgbtBlue AND Mask);
     rgbtGreen:=(SrcRow[Col].rgbtGreen AND Mask);
     rgbtRed:=(SrcRow[Col].rgbtRed AND Mask);
   end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Solarize Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Threshold    : Process only pixels above this value
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectSolarize(SrcBitmap,DestBitmap:TBitmap;Threshold:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var Row,Col            :Integer;
    SrcRow,DestRow :pRGBArray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for Row:=0 to DestBitmap.Height-1 do
  begin

   if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

   SrcRow:=SrcBitmap.ScanLine[Row];
   DestRow:=DestBitmap.ScanLine[Row];
   for Col:=0 to DestBitmap.Width-1 do
   begin
       if SrcRow[Col].rgbtBlue>=Threshold then DestRow[Col].rgbtBlue:=not SrcRow[Col].rgbtBlue
       else DestRow[Col].rgbtBlue:=SrcRow[Col].rgbtBlue;
       if SrcRow[Col].rgbtGreen>=Threshold then DestRow[Col].rgbtGreen:=not SrcRow[Col].rgbtGreen
       else DestRow[Col].rgbtGreen:=SrcRow[Col].rgbtGreen;
       if SrcRow[Col].rgbtRed>=Threshold then DestRow[Col].rgbtRed:=not SrcRow[Col].rgbtRed
       else DestRow[Col].rgbtRed:=SrcRow[Col].rgbtRed;

   end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Negative Bitmap
//
//     Parameter:  (Remark: this is just the same as Solarize, just a threshold of zero)
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectNegative(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
begin
  EffectSolarize(SrcBitmap,DestBitmap,0,EffectCallBack);
end;

// -----------------------------------------------------------------------------
//
//			Lighten/Darken Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Value to be added/substracted (-255..0..255)
//               R,G,B        : Apply filter to R,G,B Planes (Boolean)
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectLightness(SrcBitmap,DestBitmap:TBitmap;Value:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  Row,Col         :Integer;
  TargetRow       :pRGBArray;
  SourceRows      :PPRows;
begin
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
    SrcBitmap.PixelFormat:=pf24Bit;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Width:=SrcBitmap.Width;
    DestBitmap.Height:=SrcBitmap.Height;

    for Row:= 0 to SrcBitmap.Height - 1 do
    SourceRows[Row]:=SrcBitmap.Scanline[Row];

    for Row := 0 to SrcBitmap.Height - 1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
      for Col := 0 to SrcBitmap.Width - 1 do
      begin
       if R then TargetRow[Col].rgbtRed   :=TrimReal(0,255,(SourceRows[Row][Col].rgbtRed)+Value)ELSE TargetRow[Col].rgbtRed :=SourceRows[Row][Col].rgbtRed;
       if G then TargetRow[Col].rgbtGreen :=TrimReal(0,255,(SourceRows[Row][Col].rgbtGreen)+Value) ELSE TargetRow[Col].rgbtGreen :=SourceRows[Row][Col].rgbtGreen;
       if B then TargetRow[Col].rgbtBlue  :=TrimReal(0,255,(SourceRows[Row][Col].rgbtBlue)+Value)ELSE TargetRow[Col].rgbtBlue :=SourceRows[Row][Col].rgbtBlue;
     end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			Change Contrast
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Value to be added/substracted (-255..0..255)
//               R,G,B        : Apply filter to R,G,B Planes (Boolean)
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectContrast(SrcBitmap,DestBitmap:TBitmap;Value:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  i,Row,Col         :Integer;
  TargetRow       :pRGBArray;
  SourceRows      :PPRows;
  ColArray        :Array [0..256] of Byte;
begin
  for i:=0 to 126 do
  begin
    ColArray[i]:=TrimInt(0,255,i-((Abs(128-i)*Value)div 256));
  end;
  for i:=127 to 255 do
  begin
    ColArray[i]:=TrimInt(0,255,i+((Abs(128-i)*Value)div 256));
  end;

  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
    SrcBitmap.PixelFormat:=pf24Bit;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Width:=SrcBitmap.Width;
    DestBitmap.Height:=SrcBitmap.Height;

    for Row:= 0 to SrcBitmap.Height - 1 do
    SourceRows[Row]:=SrcBitmap.Scanline[Row];

    for Row := 0 to SrcBitmap.Height - 1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
      for Col := 0 to SrcBitmap.Width - 1 do
      begin
       if R then TargetRow[Col].rgbtRed   :=ColArray[SourceRows[Row][Col].rgbtRed] else TargetRow[Col].rgbtRed:= SourceRows[Row][Col].rgbtRed;
       if G then TargetRow[Col].rgbtGreen   :=ColArray[SourceRows[Row][Col].rgbtGreen] else TargetRow[Col].rgbtGreen:= SourceRows[Row][Col].rgbtGreen;
       if B then TargetRow[Col].rgbtBlue   :=ColArray[SourceRows[Row][Col].rgbtBlue] else TargetRow[Col].rgbtBlue:= SourceRows[Row][Col].rgbtBlue;
     end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			Colorize Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Hue          : Value to be added to Hue (0..359)
//               Saturation   : Value to be added to Saturation (0..255)
//               Value        : Value to be added to Value (0..255)
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------
procedure EffectColorize(SrcBitmap,DestBitmap:TBitmap;Hue,Saturation,Lightness:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var
  Row,Col         :Integer;
  TargetRow       :pRGBArray;
  hsl             :THSLTriple;
  SourceRows      :PPRows;
begin
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    SrcBitmap.PixelFormat:=pf24Bit;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Width:=SrcBitmap.Width;
    DestBitmap.Height:=SrcBitmap.Height;

    for Row:= 0 to SrcBitmap.Height - 1 do
    SourceRows[Row]:=SrcBitmap.Scanline[Row];

    for Row := 0 to SrcBitmap.Height - 1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
      for Col := 0 to SrcBitmap.Width - 1 do
      begin
       RGBtoHSL(SourceRows[Row][Col],hSl);
       hsl.hslHue:=hsl.hslHue+Hue;
       hsl.hslSaturation:=hsl.hslSaturation+Saturation;
       hsl.hslLightness:=hsl.hslLightness+Lightness;
       hsl.hslHue:=LoopInt(0,359,hsl.hslHue);
       hsl.hslSaturation:=TrimInt(0,255,hsl.hslSaturation);
       hsl.hslLightness:=TrimInt(0,255,hsl.hslLightness);
       HSLToRGB(hsl,TargetRow[Col]);
     end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			IncDecRGB Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               dR           : Value to be added to Red (0..255)
//               dG           : Value to be added to Green (0..255)
//               dB           : Value to be added to Blue (0..255)
//               EffectCallBack  :  CallBack for user interface
//
//     (c) September 1998 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
procedure EffectIncDecRGB(SrcBitmap,DestBitmap:TBitmap;dR,dG,dB:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var
  Row,Col         :Integer;
  TargetRow       :pRGBArray;
  SourceRows      :PPRows;
begin
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
    SrcBitmap.PixelFormat:=pf24Bit;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Width:=SrcBitmap.Width;
    DestBitmap.Height:=SrcBitmap.Height;

    for Row:= 0 to SrcBitmap.Height - 1 do
    SourceRows[Row]:=SrcBitmap.Scanline[Row];

    for Row := 0 to SrcBitmap.Height - 1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
      for Col := 0 to SrcBitmap.Width - 1 do
      begin
       TargetRow[Col].rgbtRed   :=TrimReal(0,255,(SourceRows[Row][Col].rgbtRed)+dR);
       TargetRow[Col].rgbtGreen :=TrimReal(0,255,(SourceRows[Row][Col].rgbtGreen)+dG);
       TargetRow[Col].rgbtBlue  :=TrimReal(0,255,(SourceRows[Row][Col].rgbtBlue)+dB);
     end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			Apply filter to Bitmap
//                      (just an interface for the 3 following procedures)
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Size         : Optional:Size of the calculated area 1..7 (neighbours of the pixel)
//                              if set to 0, the default size for the filter will be used
//                              (normally for linear 3x3, for static filters a diameter of 3)
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectFilter(SrcBitmap,DestBitmap:TBitmap;Filter:TGraphicFilter;Size:TMatrixSize;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var d:Integer;
    sft:TStaticFilterType;
begin
  CASE Filter.FilterType of
  ftLinear:begin
             if Size > mxDefault then
             Filter.MatrixSize:=Size;
             EffectLinearFilter(SrcBitmap,DestBitmap,Filter,ColorSpace,Channel1,Channel2,Channel3,EffectCallBack);
           end;
  ftStatic:begin
             d:=3;
             CASE Size of
               mx3:d:=3;
               mx5:d:=5;
               mx7:d:=7;
             end;
             if Filter.FilterName='Minimum (Static)' then sft:=sfMin ELSE if
             Filter.FilterName='Maximum (Static)' then sft:=sfMax ELSE sft:=sfMedian;
             EffectStaticFilter(SrcBitmap,DestBitmap,sft,d,Channel1,Channel2,Channel3,EffectCallBack);
           end
  end;
end;



// -----------------------------------------------------------------------------
//
//			Apply Median static filter to Bitmap
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Diameter     : Size of the calculated area 1..7 (neighbours of the pixel)
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------
procedure EffectStaticFilter(SrcBitmap,DestBitmap:TBitmap;StaticFilterType:TStaticFilterType;Diameter:Integer;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  row,col           :Integer;
  k,i               :Integer;
  Medium,Min,Max    :Integer;
  m,mx,my           :Integer;
  HorzLine,VertLine :Integer;
  MaxCol,MaxRow     :Integer;
  MinCol,MinRow     :Integer;
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  lLightness        :array[0..6,0..6] of Integer;
  BorderDiffX,BorderDiffY :Integer;

begin
 if Diameter in [3,5,7] then
 begin
    Diameter:=Diameter DIV 2;
    SetBitmapsEql(SrcBitmap,DestBitmap);
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
      for Row := 0 to SrcBitmap.Height - 1 do
      begin

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

        SourceRows[Row]:=SrcBitmap.Scanline[Row];
        TargetRow := DestBitmap.Scanline[Row];
        for Col := 0 to SrcBitmap.Width - 1 do
        begin
          MinRow:=-Diameter;
          MaxRow:=Diameter;
          MinCol:=-Diameter;
          Maxcol:=Diameter;
          for HorzLine:=MinRow to MaxRow do
          begin
          {To handle the borders of a Bitmap i use the variable BorderDiff.
          In case of a borderpixel the pixelarray will be mirrored to the "illegal" coordinates}
          if ((Row<= Diameter) AND (Horzline<0)) OR((Row>= SrcBitmap.Height-1-Diameter) AND (Horzline>0))
          then BorderDiffY:=-1 ELSE BorderDiffY:=1;
            for VertLine:=MinCol to MaxCol do
            begin
              if ((Col<= Diameter) AND (Vertline<0)) OR ((Col>= SrcBitmap.Width-1-Diameter) AND (Vertline>0))
              then BorderDiffX:=-1 ELSE
              BorderDiffX:=1;
              lLightness[Horzline+3,Vertline+3]:=RGBLightness(SourceRows[Row+(HorzLine*BorderDiffY)][Col+(VertLine*BorderDiffX)]);
            end;
          end;
          mx:=0;my:=0;
          Min:=0;Max:=255;Medium:=lLightness[3,3];
          for k:=MinRow to MaxRow do
          begin
            for i:=MinCol to MaxCol do
            begin
              m:=lLightness[k+3][i+3];
              CASE StaticFilterType of
                sfMedian:begin
                            if (m>Min) AND (m<Medium) then
                            begin
                             Min:=m
                            end;//   ELSE
                            if (m<Max) AND (m>Medium) then
                            begin
                              Max:=m;
                            end;
                         if (m>=Min) AND (m<=Max) then
                           begin
                              Medium:=m;
                              mx:=k;my:=i;
                            end ;//ELSE

                        end;
                sfMax:   begin
                          if m>Min then begin
                          Min:=m;
                          mx:=k;my:=i;
                          end;
                        end;
                sfMin:  begin
                          if m<Max then begin
                          Max:=m;
                          mx:=k;my:=i;
                          end;
                        end;
             end;
            end;
          end;
          if ((Row<= Diameter) AND (mx<0)) OR((Row>= SrcBitmap.Height-1-Diameter) AND (mx>0))
          then BorderDiffY:=-1 ELSE BorderDiffY:=1;
          if ((Col<= Diameter) AND (my<0)) OR ((Col>= SrcBitmap.Width-1-Diameter) AND (my>0))
          then BorderDiffX:=-1 ELSE
          BorderDiffX:=1;

          if b then TargetRow[Col].rgbtBlue  :=SourceRows[Row+(mx*BorderDiffY)][Col+(my*BorderDiffX)].rgbtBlue;
          if r then TargetRow[Col].rgbtRed  :=SourceRows[Row+(mx*BorderDiffY)][Col+(my*BorderDiffX)].rgbtRed;
          if g then TargetRow[Col].rgbtGreen  :=SourceRows[Row+(mx*BorderDiffY)][Col+(my*BorderDiffX)].rgbtGreen;
        end;
      end;
    finally
      FreeMem(SourceRows);
    end;
 end;
end;

// -----------------------------------------------------------------------------
//
//			Apply linear filter to Bitmap (RGB,HSV,HSL)
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Filter       : Filter (see TGraphicFilter at the top of this unit)
//               ColorSpace   : Which colorspase should be used (csRGB,csHSV,csHSL)
//               Channel 1-3  : Apply filter to Channel 1 -3
//                              csRGB R:Channel1 G:Channel 2 B: Channel 3
//                              csHSV H:Channel1 S:Channel 2 V: Channel 3
//                              csHSL H:Channel1 S:Channel 2 L: Channel 3
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectLinearFilter(SrcBitmap,DestBitmap:TBitmap;Filter:TGraphicFilter;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  row,col                 :Integer;
  i,mxCount               :Integer;
  HorzLine,VertLine       :Integer;
  MaxCol,MaxRow           :Integer;
  MinCol,MinRow           :Integer;
  TargetRow               :pRGBArray;
  Val1,Val2,Val3          :array[0..6] of Integer;
  Val1Sum,Val2Sum,Val3Sum :Integer;
  h1,s1,vl1               :Integer;
  hsv                     :THSVTriple;
  hsl                     :THSLTriple;
  SourceRows              :PPRows;
  BorderDiffX,BorderDiffY :Integer;
  Intensity,Intensity2    :Integer;
begin
   if (@Filter=nil) OR (Filter.Divisor=0) OR (Filter.FilterType<>ftLinear) then
   begin
     DestBitmap.Assign(SrcBitmap);
     Exit;
   end;
   mxCount:=1;
    CASE Filter.MatrixSize of
      mx3:mxCount:=1;
      mx5:mxCount:=2;
      mx7:mxCount:=3;
    end;
    SetBitmapsEql(SrcBitmap,DestBitmap);
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
    for Row := 0 to SrcBitmap.Height - 1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
      for Col := 0 to SrcBitmap.Width - 1 do
      begin
        Val1Sum:=0;
        Val2Sum:=0;
        Val3Sum:=0;
        MinRow:=-mxCount;
        MaxRow:=mxCount;
        MinCol:=-mxCount;
        Maxcol:=mxCount;
        for i:= MinRow to MaxRow do
        begin
          if Channel1 then Val1[i+3]:=0;
          if Channel2 then Val2[i+3]:=0;
          if Channel3 then Val3[i+3]:=0;
        end;
        for HorzLine:=MinRow to MaxRow do
        begin
          {to handle the borders of a Bitmap i use the variable BorderDiff.
           In case of a borderpixel the pixelarray will be mirrored to the "illegal" coordinates}
          if ((Row<= mxCount) AND (Horzline<0)) OR((Row>= SrcBitmap.Height-1-mxCount) AND (Horzline>0))
          then BorderDiffY:=-1 ELSE BorderDiffY:=1;
          for VertLine:=MinCol to MaxCol do
          begin
            if ((Col<= mxCount) AND (Vertline<0)) OR ((Col>= SrcBitmap.Width-1-mxCount) AND (Vertline>0))
            then BorderDiffX:=-1 ELSE
            BorderDiffX:=1;
            CASE ColorSpace of
            csRGB:begin
                    if Channel1 then Val1[HorzLine+3]:=Val1[HorzLine+3]+(SourceRows[Row+HorzLine*BorderDiffY][Col+VertLine*BorderDiffX].rgbtRed*Filter.Matrix[HorzLine+3,VertLine+3]);
                    if Channel2 then Val2[HorzLine+3]:=Val2[HorzLine+3]+(SourceRows[Row+HorzLine*BorderDiffY][Col+VertLine*BorderDiffX].rgbtGreen*Filter.Matrix[HorzLine+3,VertLine+3]);
                    if Channel3 then Val3[Horzline+3]:=Val3[HorzLine+3]+(SourceRows[Row+HorzLine*BorderDiffY][Col+VertLine*BorderDiffX].rgbtBlue*Filter.Matrix[HorzLine+3,VertLine+3]);
                  end;
            csHSV,csHSL:begin
                    case ColorSpace of
                    csHSV:begin
                            RGBtoHSV(SourceRows[Row+HorzLine*BorderDiffY][Col+VertLine*BorderDiffX],hsv);
                            if Channel1 then Val1[HorzLine+3]:=Val1[HorzLine+3]+(hsv.hsvHue*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val1[HorzLine+3]:=hsv.hsvHue;
                            if Channel2 then Val2[HorzLine+3]:=Val2[HorzLine+3]+(hsv.hsvSaturation*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val2[HorzLine+3]:=hsv.hsvSaturation;
                            if Channel3 then Val3[Horzline+3]:=Val3[HorzLine+3]+(hsv.hsvValue*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val3[HorzLine+3]:=hsv.hsvValue;
                          end;
                    csHSL:begin
                            RGBtoHSL(SourceRows[Row+HorzLine*BorderDiffY][Col+VertLine*BorderDiffX],hsl);
                            if Channel1 then Val1[HorzLine+3]:=Val1[HorzLine+3]+(hsl.hslHue*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val1[HorzLine+3]:=hsl.hslHue;
                            if Channel2 then Val2[HorzLine+3]:=Val2[HorzLine+3]+(hsl.hslSaturation*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val2[HorzLine+3]:=hsl.hslSaturation;
                            if Channel3 then Val3[Horzline+3]:=Val3[HorzLine+3]+(hsl.hslLightness*Filter.Matrix[HorzLine+3,VertLine+3])
                            ELSE Val3[HorzLine+3]:=hsl.hslLightness;
                          end;
                    end;
                  end;
            end;
          end;
        end;
        for i:=MinRow to MaxRow do
        begin
            if Channel1 then Val1Sum:=Val1Sum+Val1[i+3] else Val1Sum:=Val1[3];
            if Channel2 then Val2Sum:=Val2Sum+Val2[i+3] else Val2Sum:=Val2[3];
            if Channel3 then Val3Sum:=Val3Sum+Val3[i+3] else Val3Sum:=Val3[3];
        end;

        CASE ColorSpace of
        csRGB:begin
                if Channel1 then TargetRow[Col].rgbtRed   := TrimReal(0,255,(Val1Sum*(1/Filter.Divisor))+Filter.Bias)
                ELSE TargetRow[Col].rgbtRed:=SourceRows[Row][Col].rgbtRed;
                if Channel2 then TargetRow[Col].rgbtGreen :=TrimReal(0,255,(Val2Sum*(1/Filter.Divisor))+Filter.Bias)
                ELSE TargetRow[Col].rgbtGreen:=SourceRows[Row][Col].rgbtGreen;
                if Channel3 then TargetRow[Col].rgbtBlue  :=TrimReal(0,255,(Val3Sum*(1/Filter.Divisor))+Filter.Bias)
                ELSE TargetRow[Col].rgbtBlue:=SourceRows[Row][Col].rgbtBlue;
              end;
        csHSV:begin
                HSV.hsvHue:=   LoopReal(0,359,((1/Filter.Divisor)*Val1Sum)+Filter.Bias);
                HSV.hsvSaturation:=   TrimReal(0,255,((1/Filter.Divisor)*Val2Sum)+Filter.Bias);
                HSV.hsvValue:=   TrimReal(0,255,((1/Filter.Divisor)*Val3Sum)+Filter.Bias);
                HSVToRGB(HSV,TargetRow[Col]);
              end;
        csHSL:begin
                HSL.hslHue:=   LoopReal(0,359,((1/Filter.Divisor)*Val1Sum)+Filter.Bias);
                HSL.hslSaturation:=   TrimReal(0,255,((1/Filter.Divisor)*Val2Sum)+Filter.Bias);
                HSL.hslLightness:=  TrimReal(0,255,((1/Filter.Divisor)*Val3Sum)+Filter.Bias);
                HSLToRGB(HSL,TargetRow[Col]);
              end;
        end;
      end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;



// -----------------------------------------------------------------------------
//
//			Apply multi-pass filter to Bitmap (RGB-Model)
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               MultiPassFilter : MultiPassFilter (see TMultiPassGraphicFilter at the top of this unit)
//               R,G,B        : Apply filter to R,G,B Planes (Boolean)
//               Passes       : Nr of filters to be applied
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
procedure EffectMultiPass(SrcBitmap,DestBitmap:TBitmap;MultiPassFilter:TMultiPassGraphicFilter;Passes:Integer;ColorSpace:TColorSpace;Channel1,Channel2,Channel3:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var  Bmp1,Bmp2,Bmp3:TBitmap;

procedure ClearBitmap(Bitmap:TBitmap);
begin
  Bitmap.Canvas.Brush.Color:=clWhite;
  Bitmap.Canvas.FillRect(Rect(0,0,Bitmap.Width,Bitmap.Height));
end;

begin
  if not (passes in [1..4]) OR (MultiPassFilter.FilterType<>ftMultiPass)  then exit;
  SetBitmapsEql(SrcBitmap,DestBitmap);
  Bmp1:=TBitmap.Create;
  Bmp2:=TBitmap.Create;
  Bmp3:=TBitmap.Create;
  Bmp1.PixelFormat:=pf24Bit;
  Bmp2.PixelFormat:=pf24Bit;
  Bmp3.PixelFormat:=pf24Bit;
  Bmp1.Assign(SrcBitmap);
  try
  if Passes>=2 then
  begin
    EffectFilter(Bmp1,Bmp2,MultiPassFilter.Filters[1]^,mxDefault,ColorSpace,Channel1,Channel2,Channel3,EffectCallBack);
    EffectFilter(Bmp2,Bmp3,MultiPassFilter.Filters[2]^,mxDefault,ColorSpace,Channel1,Channel2,Channel3,EffectCallBack);
    ClearBitmap(Bmp1);
    if MultiPassFilter.Functions[1]<>gaNone then EffectArithmetic(bmp2,bmp3,bmp1,MultiPassFilter.Functions[1],EffectCallBack)
    ELSE Bmp1.Assign(Bmp3);
    ClearBitmap(Bmp2);
    ClearBitmap(Bmp3);
    if Passes>=3 then
    begin
      EffectFilter(Bmp1,Bmp2,MultiPassFilter.Filters[3]^,mxDefault,ColorSpace,Channel1,Channel2,Channel3,EffectCallBack);
      if MultiPassFilter.Functions[2]<>gaNone then
      begin
        EffectArithmetic(bmp1,bmp2,bmp3,MultiPassFilter.Functions[2],EffectCallBack);
        ClearBitmap(Bmp1);
        Bmp1.Assign(Bmp3);
      end ELSE
      begin
        ClearBitmap(Bmp1);
        Bmp1.Assign(Bmp2);
      end;
      ClearBitmap(Bmp3);
      ClearBitmap(Bmp2);
      if Passes = 4 then
      begin
        EffectFilter(Bmp1,Bmp2,MultiPassFilter.Filters[4]^,mxDefault,ColorSpace,Channel1,Channel2,Channel3,EffectCallBack);
        if MultiPassFilter.Functions[3]<>gaNone then
        begin
          EffectArithmetic(bmp1,bmp2,bmp3,MultiPassFilter.Functions[3],EffectCallBack);
          ClearBitmap(Bmp1);
          Bmp1.Assign(Bmp3);
        end ELSE
        begin
          ClearBitmap(Bmp1);
          Bmp1.Assign(Bmp2);
        end;
          ClearBitmap(Bmp3);
          ClearBitmap(Bmp2);
      end;
    end;
  end;
    DestBitmap.Assign(Bmp1);
  finally
    Bmp1.Free;
    Bmp2.Free;
    Bmp3.Free;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Sinus effect
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               SinusAmpVer  : Amplitude vertical
//               VertDelta    : Delta vertical
//               SinusAmpHorz : Amplitude horizontal
//               HorzDleta    : Delta horizontal
//               VertStart    : Vertical start
//               VertStart    : Vertical start
//               R,G,B        : Apply antialias to R,G,B Planes (Boolean)
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------

procedure EffectSinus(SrcBitmap,DestBitmap:TBitmap;SinusAmpVert,VertDelta,SinusAmpHorz,HorzDelta,
                      VertStart,HorzStart:Integer;
                      ChngVertAtAnyCol:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  Row,Col,s,ds,t,dt :Integer;
  TargetRow       :pRGBArray;
  SourceRows      :PPRows;
begin
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
    dt:=HorzStart;

    if VertDelta=0 then VertDelta:=1;
    if HorzDelta=0 then HorzDelta:=1;

    SrcBitmap.PixelFormat:=pf24Bit;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Width:=SrcBitmap.Width;
    DestBitmap.Height:=SrcBitmap.Height;

    for Row:= 0 to SrcBitmap.Height - 1 do
    SourceRows[Row]:=SrcBitmap.Scanline[Row];

    for Row := 0 to SrcBitmap.Height - 1 do
    begin
      if not ChngVertAtAnyCol then if dt/HorzDelta<360 then inc(dt,1) else dt:=0;

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

      TargetRow := DestBitmap.Scanline[Row];
       ds:=VertStart;
       for Col := 0 to SrcBitmap.Width - 1 do
       begin
       s:=Round(SinusAmpVert*Sin(ds / VertDelta));
       if ds/VertDelta<360 then inc(ds,1) else ds:=0;
       if ChngVertAtAnyCol then if dt/HorzDelta<360 then inc(dt,1) else dt:=0;
       if (Row+s<0) or (Row+s> SrcBitmap.Height - 1 )then s:=0;
       t:=Round(SinusAmpHorz*Sin(dt / HorzDelta));
       if (Col+t<0) or (Col+t> SrcBitmap.Width - 1 )then t:=0;
       TargetRow[Col] := SourceRows[Row+S][Col+T];
    end;
    end;
  finally
    FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			Spray effect
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Intensity
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------

procedure EffectSpray(SrcBitmap,DestBitmap:TBitmap;Value:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var
  TargetRow       :pRGBArray;
  SourceRows      :PPRows;
  Row,Col,fCol,fRow,f       :Integer;
begin

    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
    try
    SetBitmapsEql(SrcBitmap,DestBitmap);
  for Row:=0 to DestBitmap.Height-1 do
  begin

    if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/SrcBitmap.Height)*100));

    TargetRow:=DestBitmap.ScanLine[Row];
    for Col:=0 to DestBitmap.Width-1 do begin
      f:=Random(Value);
      fCol:=Col+f-Random(f*2);
      fRow:=Row+f-Random(f*2);
      if(fCOl>-1)and(fCol<SrcBitmap.Width-1)and(fRow>-1)and(fRow<SrcBitmap.Height-1)then
      begin
        TargetRow[Col].rgbtRed:=SourceRows[fRow][fCol].rgbtRed;
        TargetRow[Col].rgbtBlue:=SourceRows[fRow][fCol].rgbtBlue;
        TargetRow[Col].rgbtGreen:=SourceRows[fRow][fCol].rgbtGreen;
      end;
    end;
  end;
  finally
   FreeMem(SourceRows);
  end;
end;

// -----------------------------------------------------------------------------
//
//			Add noise
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               MonoNoise    : Should noisevalue calculated for each plane or not
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------

procedure EffectAddNoise(SrcBitmap,DestBitmap:TBitmap;Value:Integer;MonoNoise:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  TargetRow       :pRGBArray;
  SourceRow       :pRGBArray;
  Row,Col,f           :Integer;
function Calculate(V:Integer):Integer;
begin
 result:=Random(Value)-(Value shr 1);
end;
begin
    SetBitmapsEql(SrcBitmap,DestBitmap);
    for Row:=0 to DestBitmap.Height-1 do
    begin

      if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((Row/TBitmap(@SrcBitmap).Height)*100));

      TargetRow:=DestBitmap.ScanLine[Row];
      SourceRow:=SrcBitmap.ScanLine[Row];
      for Col:=0 to DestBitmap.Width-1 do begin
          f:=Calculate(Value);
          TargetRow[Col].rgbtRed:=TrimInt(0,255,SourceRow[Col].rgbtRed+f);
          if (not MonoNoise) then f:=Calculate(Value);
          TargetRow[Col].rgbtBlue:=TrimInt(0,255,SourceRow[Col].rgbtBlue+f);
          if (not MonoNoise) then f:=Calculate(Value);
          TargetRow[Col].rgbtGreen:=TrimInt(0,255,SourceRow[Col].rgbtGreen+f);
      end;
    end;
end;

// -----------------------------------------------------------------------------
//
//			Anti-aliasing
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               R,G,B        : Apply antialias to R,G,B Planes (Boolean)
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------

procedure EffectAntiAlias(SrcBitmap,DestBitmap:TBitmap;R,G,B:Boolean;const EffectCallBack:TEffectCallBack);stdcall;
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  nbhood            :array[0..3] of TRGBTriple;
  rr,gg,bb          :Integer;
  Row,Col,i         :Integer;
begin

    SetBitmapsEql(SrcBitmap,DestBitmap);
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
      for Row := 0 to SrcBitmap.Height - 1 do
      begin

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));

        TargetRow:=DestBitmap.Scanline[Row];
        for Col := 0 to SrcBitmap.Width - 1 do
        begin
          if Col>0 then                  nbhood[0]:=SourceRows[Row][Col-1] else nbhood[0]:=SourceRows[Row][Col+1];
          if Col<SrcBitmap.Width-1 then  nbhood[1]:=SourceRows[Row][Col+1] else nbhood[1]:=SourceRows[Row][Col-1];
          if Row>0 then                  nbhood[2]:=SourceRows[Row-1][Col] else nbhood[2]:=SourceRows[Row+1][Col];
          if Row<SrcBitmap.Height-1 then nbhood[3]:=SourceRows[Row+1][Col] else nbhood[3]:=SourceRows[Row-1][Col];
          rr:=0;gg:=0;bb:=0;
          for i:=0 to 3 do
          begin
            rr:=rr+nbhood[i].rgbtRed;
            gg:=gg+nbhood[i].rgbtGreen;
            bb:=bb+nbhood[i].rgbtBlue;
          end;
          rr:=rr div 4;
          gg:=gg div 4;
          bb:=bb div 4;
                      
          if b then TargetRow[Col].rgbtBlue  := bb;
          if r then TargetRow[Col].rgbtRed  := rr;
          if g then TargetRow[Col].rgbtGreen  := gg;
        end;
      end;
    finally
      FreeMem(SourceRows);
    end;
end;

procedure LoadLinearFilterFromFile(FileName:PChar;var Filter:TGraphicFilter);stdcall;
var Stream:TFileStream;
    header: Array [0..5] of Char;
begin
 Stream:=TFileStream.Create(FileNAme,fmOpenRead);
 try
   ZeroMemory(PChar(@header),sizeof(header));
   Stream.Read(header,4);
   if header<>'fxlf' then raise EGraphicEffects.Create('Not a valid linear filter file') else
   begin
      Stream.Read(Filter,SizeOf(TGraphicFilter));

   end;
 finally
   Stream.Free;
 end;
end;

procedure SaveLinearFilterToFile(FileName:PChar;const Filter:TGraphicFilter);stdcall;
var stream:TFileStream;
 const c ='Created by pView (c) 1999 by A.Moser';
begin
 Stream:=TFileStream.Create(FileNAme,fmCreate);
 try
   Stream.Write('fxlf',4);
   Stream.Write(Filter,SizeOf(TGraphicFilter));
   Stream.Write(c,Length(c));
 finally
   Stream.Free;
 end;

end;

procedure LoadMultiPassFilterFromFile(FileName:PChar;var Filter:TMultiPassGraphicFilter);stdcall;
var Stream:TFileStream;
    header: Array [0..5] of Char;
begin
 Stream:=TFileStream.Create(FileNAme,fmOpenRead);
 try
   ZeroMemory(PChar(@header),sizeof(header));
   Stream.Read(header,4);
   if header<>'fxmf' then raise EGraphicEffects.Create('Not a valid linear filter file') else
   begin
      Stream.Read(Filter,SizeOf(TMultiPassGraphicFilter));

   end;
 finally
   Stream.Free;
 end;
end;

procedure SaveMultipassFilterToFile(FileName:PChar;const Filter:TMultiPassGraphicFilter);stdcall;
var stream:TFileStream;
 const c ='Created by pView (c) 1999 by A.Moser';
begin
 Stream:=TFileStream.Create(FileNAme,fmCreate);
 try
   Stream.Write('fxmf',4);
   Stream.Write(Filter,SizeOf(TMultiPassGraphicFilter));
   Stream.Write(c,Length(c));
 finally
   Stream.Free;
 end;

end;

// -----------------------------------------------------------------------------
//
//			Stretch
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------

procedure EffectStretch(SrcBitmap,DestBitmap:TBitmap;Low,High:Integer;const EffectCallBack:TEffectCallBack);stdcall;
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row,Col         :Integer;
begin
    SetBitmapsEql(SrcBitmap,DestBitmap);
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
      for Row := 0 to SrcBitmap.Height - 1 do
      begin
        TargetRow:=DestBitmap.Scanline[Row];

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
        for Col := 0 to SrcBitmap.Width - 1 do
        begin

          if RGBIntensity(SourceRows[Row][Col])<=Low then
          begin
            TargetRow[Col].rgbtBlue  := 0;
            TargetRow[Col].rgbtRed  := 0;
            TargetRow[Col].rgbtGreen  := 0;
          end
          else
          if RGBIntensity(SourceRows[Row][Col])>=High then
          begin
            TargetRow[Col].rgbtBlue  := 255;
            TargetRow[Col].rgbtRed  := 255;
            TargetRow[Col].rgbtGreen  := 255;
          end
          else
            TargetRow[Col]:=SourceRows[Row][Col];
          end;
        end;
    finally
      FreeMem(SourceRows);
    end;
end;


procedure EffectGamma(SrcBitmap,DestBitmap:TBitmap;Gamma:Double;const EffectCallBack:TEffectCallBack);stdcall;
// -----------------------------------------------------------------------------
//
//			Gamma correction
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Gamma value
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row,Col,i         :Integer;
  GammaArray        :Array [0..255] of Byte;
begin
    for i:=0 to 255 do
    GammaArray[i]:=IntGamma(Gamma,i);

    SetBitmapsEql(SrcBitmap,DestBitmap);
    GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
    try
    for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
      for Row := 0 to SrcBitmap.Height - 1 do
      begin
        TargetRow:=DestBitmap.Scanline[Row];

        if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
        for Col := 0 to SrcBitmap.Width - 1 do
        begin
          TargetRow[Col].rgbtBlue  := GammaArray[SourceRows[Row][Col].rgbtBlue];
          TargetRow[Col].rgbtGreen  := GammaArray[SourceRows[Row][Col].rgbtGreen];
          TargetRow[Col].rgbtRed  := GammaArray[SourceRows[Row][Col].rgbtRed];
        end;
      end;
    finally
      FreeMem(SourceRows);
    end;
end;



procedure EffectEllipse(SrcBitmap,DestBitmap:TBitmap;const EffectCallBack:TEffectCallBack);stdcall;
// -----------------------------------------------------------------------------
//
//			Ellipse
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
var
  x, y, x1, y1:Integer;
  fx, fy, xmid, ymid, ar:double;
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row         :Integer;

function ComputePixel(x, y:double;var x1,y1:double):Integer; //float &x1, float &y1)
var
  r, nn : double;
begin

  if (x=0) and (y=0) then
  begin
	 x1 := x;
	 y1 := y;
	 result:=1;

  end
  else
  begin

  nn := sqrt(x*x*0.5 + y*y);
  if (abs(x) > abs(y)) then r:=abs(nn/x) else r:=abs(nn/y);

  x1 := (r*x);
  y1 := (r*y);

  result:= 1;
  end;
end;
begin

  xmid := SrcBitmap.Width /2.0;
  ymid := SrcBitmap.Height/2.0;
  ar := SrcBitmap.Height/SrcBitmap.Width ;

  SetBitmapsEql(SrcBitmap,DestBitmap);
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
  for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];
  Row:=0;
  for y:=0 to DestBitmap.Height -1 do
  begin
     if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
     TargetRow:=DestBitmap.Scanline[y];
     for x:=0 to DestBitmap.Width-1 do
      begin
        ComputePixel(ar*(x-xmid), y-ymid, fx, fy);
	x1 := Round(xmid+fx/ar);
	y1 := Round(ymid+fy);
        if (y1>0) and (y1< SrcBitmap.Height-1) and
        (x1>0) and (x1< SrcBitmap.Width-1) then
        TargetRow[x]:=SourceRows[y1][x1]
        else
        begin
          TargetRow[x].rgbtBlue := 0;
          TargetRow[x].rgbtGreen := 0;
          TargetRow[x].rgbtRed := 0;
        end;


     end;
  end;
  finally
      FreeMem(SourceRows);
  end;
end;


procedure EffectMosaic(SrcBitmap,DestBitmap:TBitmap;Width,Height:Integer;const EffectCallBack:TEffectCallBack);stdcall;
// -----------------------------------------------------------------------------
//
//			Mosaic effect
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Width,Height : Dimension of blocks
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row,Col           :Integer;
  source_row,source_col:Integer;
  inc_x,inc_y,
  half_x, half_y:   Integer;
begin
  if(Width<1) or (Height<1) then Exit;

  half_x:=(Width shr 1)+(Width and 1);
  half_y:=(Height shr 1)+(Height and 1);

  SetBitmapsEql(SrcBitmap,DestBitmap);
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
  for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];


  source_Row:=half_y;
  inc_y:=0;
  for Row:=0 to DestBitmap.Height-1 do
  begin
    if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
    source_Col:=half_x;
    inc_x:=0;
    TargetRow:=DestBitmap.Scanline[Row];
    for Col:=0 to DestBitmap.Width-1 do
    begin
      TargetRow[Col]:=SourceRows[source_row][source_col];
      inc(inc_x);
      if inc_x>=Width then
      begin
        source_col:=source_col+Width;
        if source_col>SrcBitmap.Width-1 then source_col:=SrcBitmap.Width-1;
        inc_x:=0;
      end;
    end;

    // increment the position in source_bitmap;
    inc(inc_y);
    if inc_y>=Height then
    begin
       source_row:=source_row+Height;
       if source_row>SrcBitmap.Height-1 then source_row:=SrcBitmap.Height-1;
      inc_y:=0;
    end;
  end;


  finally
    FreeMem(SourceRows);
  end;
end;

procedure EffectCircleAround(SrcBitmap,DestBitmap:TBitmap;Value:Integer;const EffectCallBack:TEffectCallBack);stdcall;
// -----------------------------------------------------------------------------
//
//			Picture distortion
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Amount
//               EffectCallBack  :  CallBack for user interface
//
//     
// -----------------------------------------------------------------------------
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row,Col           :Integer;
  half_x, half_y:   Integer;
  max_x,max_y:Double;
  dx,dy,r:Double;
  dsx,dsy:Double;
  sx,sy:Integer;
  theta:double;
begin

  half_x:=SrcBitmap.Width  div 2;
  half_y:=SrcBitmap.Height div 2;
  dx:=SrcBitmap.Width-1;
  dy:=SrcBitmap.Height-1;
  r:=sqrt(dx*dx + dy*dy);
  if r>=SrcBitmap.Width then max_x:=SrcBitmap.Width-1 else max_x:=r;
  if r>=SrcBitmap.Height then max_y:=SrcBitmap.Height-1 else max_y:=r;

  SetBitmapsEql(SrcBitmap,DestBitmap);
  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
  for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];

  for Row:=0 to Round(max_y) do
  begin
    if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
    TargetRow:=DestBitmap.Scanline[Row];
    for Col:=0 to Round(max_x) do
    begin
      dx:=Col-half_x;
      dy:=Row-half_y;
      r:=sqrt(dx*dx+dy*dy);
      if r=0 then
      begin
        dsx:=0;
        dsy:=0;
      end else
      begin
        if Value=0 then Value:=1; 
        theta:=xArcTan2(dx,dy)-r/Value - (-(Pi / 2));
        dsx:=r* cos(theta);
        dsy:=r* sin(theta);
      end;
      dsx:=dsx+ half_x;
      dsy:=dsy+ half_y;
      sx:=Trunc(dsx);
      sy:=Trunc(dsy);
      if sy>=SrcBitmap.Height then sy:= SrcBitmap.Height-1;
      if sx>=SrcBitmap.Width then sx:= SrcBitmap.Width-1;
      if sx<0 then sx:=0;
      if sy<0 then sy:=0;
      TargetRow[Col]:=SourceRows[sy][sx];
    end;
  end;


  finally
    FreeMem(SourceRows);
  end;
end;

procedure EffectFishEye(SrcBitmap,DestBitmap:TBitmap;Value:Extended;const EffectCallBack:TEffectCallBack);stdcall;
// -----------------------------------------------------------------------------
//
//			FishEye effect
//
//     Parameter:
//               SrcBitmap    : Bitmap to be processed
//               DestBitmap   : Result
//               Value        : Amount
//               EffectCallBack  :  CallBack for user interface
//
//
// -----------------------------------------------------------------------------
var
  SourceRows        :PPRows;
  TargetRow         :pRGBArray;
  Row,Col           :Integer;
  half_x, half_y:   Integer;
  dx,dy,radius1,radius2,radiusMax:Double;
  dsx,dsy:Double;
  sx,sy:Integer;
begin

  half_x:=SrcBitmap.Width  div 2;
  half_y:=SrcBitmap.Height div 2;
  radiusMax:= SrcBitmap.Width * Value;
  SetBitmapsEql(SrcBitmap,DestBitmap);

  GetMem(SourceRows, SrcBitmap.Height * SizeOf(pRGBArray));
  try
  for Row:= 0 to SrcBitmap.Height - 1 do SourceRows[Row]:=SrcBitmap.Scanline[Row];

  for Row:=0 to DestBitmap.Height-1 do
  begin
    if Assigned(EffectCallBack) then EffectCallBack(0,100,Round((row/SrcBitmap.Height)*100));
    TargetRow:=DestBitmap.Scanline[Row];
    for Col:=0 to DestBitmap.Width-1 do
    begin
      dx:=Col-half_x;
      dy:=Row-half_y;
      radius1:=sqrt(dx*dx+dy*dy);
      if radius1=0 then
      begin
        dsx:=0;
        dsy:=0;
      end else
      begin
        if radius1=radiusMax then radius1:=radius1+1;
        radius2:=radiusMax / 2 *(1/(1-radius1/radiusMax)-1);
        dsx:=dx*radius2 /radius1 +half_x;
        dsy:=dy*radius2 /radius1 +half_y;
      end;
      sx:=Trunc(dsx);
      sy:=Trunc(dsy);
//      if sy>=SrcBitmap.Height then sy:= SrcBitmap.Height-1;
//      if sx>=SrcBitmap.Width then sx:= SrcBitmap.Width-1;
//      if sx<0 then sx:=0;
//      if sy<0 then sy:=0;
      if (sy>=SrcBitmap.Height) or
      (sx>=SrcBitmap.Width) or
      (sx<0) or
      (sy<0) then
      begin
       TargetRow[Col].rgbtBlue:=0;
       TargetRow[Col].rgbtGreen:=0;
       TargetRow[Col].rgbtRed:=0;
      end
      else
      TargetRow[Col]:=SourceRows[sy][sx];
    end;
  end;


  finally
    FreeMem(SourceRows);
  end;
end;

end.
