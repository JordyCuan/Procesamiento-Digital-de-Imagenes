{***********************************************************************
Unit gfx_basedef.PAS v1.2 0801
    (c) by Andreas Moser, AMoser@amoser.de

    Delphi version : Delphi 4

    gfx_basedef is part of the gfx_library collection

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

    UPDATES
    v1.1 04/2001
                 - bugfix gfx_errors (for delphi 5 compatibility)
                 - update function SetBmpEqual
                 - new unit gfx_crypt
                 - new predefined filters in gfx_effectexd (glow, waggle, pattern, churchpattern)
    v1.2 08/2001 - new unit gfx_mask


********************************************************************************}

unit gfx_basedef;

interface

uses Windows,Graphics, SysUtils;

// -----------------------------------------------------------------------------
//
//			Types
//
// -----------------------------------------------------------------------------
type
   {Error handling}
   EGraphicEffects = class(Exception);
   EGraphicConvert = class(Exception);
   EGraphicFormat = class(Exception);

const   { Ranges of the IEEE floating point types, including denormals }
  MinSingle        =     1.5e-45;
  MaxSingle        =     3.4e+38;
  MinDouble        =     5.0e-324;
  MaxDouble        =     1.7e+308;
  MinExtended      =     3.4e-4932;
  MaxExtended      =     1.1e+4932;
  MinComp          =     -9.223372036854775807e+18;
  MaxComp          =     9.223372036854775807e+18;

    MaxPixelCount = 32768;

  TYPE

    {---------------------------------

      Colorspace def

    ----------------------------------}

    TColorSpace = (csRGB, csHSV, csHSL, csCMYK, csRGBA, csBGR, csBGRA, csCMY, csCIELAB, csYCBCR, cs8Bit,cs1Bit,cs4Bit,csNone);
    TColorPlane = (cpRGB,cpRed, cpGreen, cpBlue,
                   cpHueHSV, cpSaturationHSV, cpValue,
                   cpCyan, cpMagenta, cpYellow, cpBlack,
                   cpIntensity,
                   cp8bit);

    {---------------------------------

      Palette types

    ----------------------------------}

    TPaletteEntries = array[BYTE] of TPaletteEntry;

    TPaletteArray = Array [0..2,0..255] of Byte;

    TPaletteWordArray = Array [0..2,0..255] of Word;

    {---------------------------------

      Colorspace types

    ----------------------------------}
    TColorRGB = packed record
    r, g, b	: BYTE;
    end;
    PColorRGB = ^TColorRGB;

    pRGBarray  = ^TRGBarray;
    TRGBarray   = array[0..MaxPixelCount-1] of TRGBTriple;
    TPRows = array[0..1000000] of pRGBarray;
    PPRows = ^TPRows;

    pRGBTriple = ^TRGBTriple;
    TRGBQuadArray   = array[BYTE] of TRGBQUAD;

    TScanLine= Array of Byte;
    pScanLine= ^TScanLine;

    TBytearrayL = array[0..65535] of Byte;
    pBytearrayL = ^TBytearrayL;

    T3Bytearray = array[0..3*65535] of Byte;
    p3Bytearray = ^TBytearray;

    TCardinalArray = Array of Cardinal;

    TCMYKQuad = record
       cmykCyan,cmykMagenta,cmykYellow,cmykK: Byte;
    end;
    pCMYKQuad = ^TCMYKQuad;

    TCMYTriple = record
       cmykCyan,cmykMagenta,cymkYellow: Integer;
    end;
    pCMYTriple = ^TCMYTriple;

    THSVTriple = record
       hsvHue,hsvSaturation,hsvValue : Integer;
    end;
    pHSVTriple = ^THSVTriple;

    THSLTriple = record
       hslHue,hslSaturation,hslLightness : Integer;
    end;
    pHSLTriple = ^THSLTriple;

    TRGBAQuad = record
       rgbBlue,rgbGreen,rgbRed,rgbAlpha: Integer;
    end;
    pRGBAQuad = ^TRGBAQuad;

    TBGRTriple = record
       bgrBlue,bgrGreen,bgrRed: Integer;
    end;
    pBGRTriple = ^TBGRTriple;

    TBGRAQuad = record
       bgrBlue,bgrGreen,bgrRed,bgrAlpha: Integer;
    end;
    pBGRAQuad = ^TBGRAQuad;

    TBGRTripleByte = record
       bgrBlue,bgrGreen,bgrRed: Byte;
    end;
    pBGRTripleByte = ^TBGRTripleByte;

    TBGRAQuadByte = record
       bgrBlue,bgrGreen,bgrRed,bgrAlpha: Byte;
    end;
    pBGRAQuadByte = ^TBGRAQuadByte;

    TYCbCrTriple = record
      ycbcrY,ycbcrCB,ycbcrCR:Integer;
    end;
    pYCbCrTriple = ^TYCbCrTriple;

    TCIELABTriple = record
      L,A,B:Byte;
    end;
    pCIELABTriple = ^TCIELABTriple;

    TBool2dArray=array of array of boolean;

//var ScanLineArray:Array[0..65535] Of Byte;


    {---------------------------------

      function & procedure interface

    ----------------------------------}

    {because that borland has not included the math.pas to all delphi versions
     here are some math functions}

    function xArcCos(X: Extended): Extended;
    function xArcSin(X: Extended): Extended;
    function xArcTan2(Y, X: Extended): Extended;
    function xCeil(X: Extended): Integer;
    function xFloor(X: Extended): Integer;

    { some min-max functions }
    function xMinValue(const Data: array of Double): Double;
    function xMinIntValue(const Data: array of Integer): Integer;
    function xMaxValue(const Data: array of Double): Double;
    function xMaxIntValue(const Data: array of Integer): Integer;

    function TrimReal(Lower, Upper: integer; x: double): integer;     //Trim the real value X into the bounds Lower and Upper
    function TrimInt(Lower, Upper, X: integer): integer;              //Trim the integer value X into the bounds Lower and Upper
    function LoopInt(Lower, Upper, X: integer): integer;              //Trim the integer value X into the bounds Lower and Upper, (if X > Upper then X= Lower +(X-Upper)
    function LoopReal(Lower, Upper:Integer; X: Double): integer;      //Trim the integer value X into the bounds Lower and Upper, (if X > Upper then X= Lower +(X-Upper)
    function MinInt2(Value1,Value2:Integer):Integer;                  //Get the maximum of two integers
    function MaxInt2(Value1,Value2:Integer):Integer;                  //Get the minimum of two integers
    function MinDouble2(Value1,Value2:Double):Double;                 //Get the maximum of two double values
    function MaxDouble2(Value1,Value2:Double):Double;                 //Get the minimum of two double values
    procedure MinMaxInt3(const Value1,Value2,Value3:  INTEGER; var min, max:  INTEGER); //Get the min/max of 3 integers
    procedure MinMaxDouble3(const Value1,Value2,Value3:  Double; var min, max:  Double);//Get the min/max of 3 double values
    procedure SetBitmapsEql(const SrcBitmap,DstBitmap:TBitmap);       //Sets the dimensions of DestBitmap to the values of SrcBitmap, an set the pixelformat of both bitmaps to pf24bit
    procedure CopyBitmap(const SrcBitmap,DstBitmap:TBitmap);

    { Color Conversions }
    function RGBTriple(const red, green, blue:  BYTE):  TRGBTriple;
    function RGBtoColor(RGBTriple: TRGBTriple): TColor;
    function ColorToRGB(Color: TColor): TRGBTriple;
    procedure AssignRGBTriple (var TargetTriple:  TRGBTriple;const OriginTriple:  TRGBTriple);
    function RGBIntensity(const RGB:  TRGBTriple):  INTEGER;
    function RGBLightness(const RGB:  TRGBTriple):  INTEGER;
    function RGBSaturation(const RGB:  TRGBTriple):  INTEGER;
    function RGBValue(const RGB:  TRGBTriple):  INTEGER;
    procedure RGBtoCMY(const RGB:  TRGBTriple;var CMY:  TCMYTriple);
    procedure RGBtoCMYK(const RGB:  TRGBTriple;var CMYK:  TCMYKQuad);
    procedure CMYKtoRGB(const CMYK:  TCMYKQuad; var RGB:TRGBTriple);
    procedure HSVtoRGB (const HSV: THSVTriple ;var   RGB:TRGBTriple);  // S, V must be in [0..255], H must be in [0..359]
    procedure RGBtoHSV (const RGB:TRGBTriple;var HSV:THSVTriple);
    procedure RGBtoHSLDbl (RGB: TRGBTriple; var H,S,L:Double);
    procedure RGBtoHSL (RGB: TRGBTriple; var HSL:THSLTriple);
    procedure HSLDblToRGB (H,S,L:Double;var RGB:  TRGBTRiple);        // S, L must be in [0..255], H must be in [0..359]
    procedure HSLtoRGB (HSL:THSLTriple;var RGB:  TRGBTRiple);          // S, L must be in [0..255], H must be in [0..359]
    procedure YCbCrToRGB(ycbcr:TYCbCrTriple; var RGB:  TRGBTRiple);
    procedure CIELABtoBGR(CIELABTriple:TCIELABTriple; var BGRTriple:TBGRTriple);
    procedure RGBAtoBGRA(RGBA:TRGBAQuad; var BGRA:TBGRAQuad);
    procedure RGBAtoBGRA_16(RGBA:TRGBAQuad; var BGRA:TBGRAQuad);

    {Gamma}
    function CalculateGamma(Gamma:Double;Value:Integer):Double;
    function IntGamma(Gamma:Double;Value:Integer):Integer;

    {misc. functions}
    function SwapL(Value:Cardinal):Cardinal;
    function SwapS(Value:WORD):WORD;
    procedure Deprediction1(p:Pointer; Count: Cardinal);
    procedure Deprediction3(p:Pointer; Count: Cardinal);
    procedure Deprediction4(p:Pointer; Count: Cardinal);
{ ---------------------------------------------------------------- }

implementation

// -----------------------------------------------------------------------------
//
//			xArcCos
//
// -----------------------------------------------------------------------------
function xArcCos(X: Extended): Extended;
begin
  Result := xArcTan2(Sqrt(1 - X*X), X);
end;

// -----------------------------------------------------------------------------
//
//			xArcSin
//
// -----------------------------------------------------------------------------
function xArcSin(X: Extended): Extended;
begin
  Result := xArcTan2(X, Sqrt(1 - X*X))
end;

// -----------------------------------------------------------------------------
//
//			xArcTan2
//
// -----------------------------------------------------------------------------
function xArcTan2(Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

// -----------------------------------------------------------------------------
//
//			xCeil
//
// -----------------------------------------------------------------------------
function xCeil(X: Extended): Integer;
begin
  Result := Trunc(X);
  if Frac(X) > 0 then
    Inc(Result);
end;

// -----------------------------------------------------------------------------
//
//			xFloor
//
// -----------------------------------------------------------------------------
function xFloor(X: Extended): Integer;
begin
  Result := Trunc(X);
  if Frac(X) < 0 then
    Dec(Result);
end;

// -----------------------------------------------------------------------------
//
//			xMinValue
//
// -----------------------------------------------------------------------------
function xMinValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

// -----------------------------------------------------------------------------
//
//			xMinIntValue
//
// -----------------------------------------------------------------------------
function xMinIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

// -----------------------------------------------------------------------------
//
//			xMaxValue
//
// -----------------------------------------------------------------------------
function xMaxValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I];
end;

// -----------------------------------------------------------------------------
//
//			xMaxIntValue
//
// -----------------------------------------------------------------------------
function xMaxIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I];
end;


// -----------------------------------------------------------------------------
//
//			TrimReal
//
// -----------------------------------------------------------------------------
function TrimReal(Lower, Upper: integer; x: double): integer;
begin
 if (x < upper) and (x >= lower) then
  result:= trunc(x)
 else
  if x >= Upper then
   result:= Upper
    else
     result:= Lower;
end;

// -----------------------------------------------------------------------------
//
//			TrimReal
//
// -----------------------------------------------------------------------------
function TrimInt(Lower, Upper, X: integer): integer;
begin
 if (X <= Upper) and (X >= Lower) then
  TrimInt:= X
 else
  if X > Upper then
   TrimInt:= Upper
    else
     TrimInt:= Lower;
end;

// -----------------------------------------------------------------------------
//
//			LoopInt
//
// -----------------------------------------------------------------------------
function LoopInt(Lower, Upper, X: integer): integer;
begin
 if (X <= Upper) and (X >= Lower) then
  LoopInt:= X
 else
  if X > Upper then
   LoopInt:= Lower + X-Upper
    else
     LoopInt:= Upper-Lower+X;
end;

// -----------------------------------------------------------------------------
//
//			LoopReal
//
// -----------------------------------------------------------------------------
function LoopReal(Lower, Upper:Integer; X: Double): integer;
begin
 if (X <= Upper) and (X >= Lower) then
  result:= Round(X)
 else
  if X > Upper then
   result:= Round(Lower + X-Upper)
    else
     result:= Round(Upper-Lower+X);
end;

// -----------------------------------------------------------------------------
//
//			MaxInt2
//
// -----------------------------------------------------------------------------
function MaxInt2(Value1,Value2:Integer):Integer;
begin
  if Value1>Value2 then Result:=Value1 else Result:=Value2;
end;

// -----------------------------------------------------------------------------
//
//			MinInt2
//
// -----------------------------------------------------------------------------
function MinInt2(Value1,Value2:Integer):Integer;
begin
  if Value1<Value2 then Result:=Value1 else Result:=Value2;
end;

// -----------------------------------------------------------------------------
//
//			MaxDouble2
//
// -----------------------------------------------------------------------------
function MaxDouble2(Value1, Value2 : double): double;
begin
    if Value1 > Value2 then
      Result := Value1 else  Result := Value2
end;

// -----------------------------------------------------------------------------
//
//			MinDouble2
//
// -----------------------------------------------------------------------------
function MinDouble2 (Value1, Value2 : double): double;
begin
    if Value1 < Value2 then
    Result := Value1 else Result := Value2
end;

// -----------------------------------------------------------------------------
//
//			MinMaxInt3
//
// -----------------------------------------------------------------------------
procedure MinMaxInt3(const Value1,Value2,Value3:  INTEGER; var min, max:  INTEGER);
begin
    if   Value1 > Value2
    then begin
      if   Value1 > Value3
      then max := Value1
      else max := Value3;

      if   Value2 < Value3
      then min := Value2
      else min := Value3
    end
    else begin
      if   Value2 > Value3
      then max := Value2
      else max := Value3;

      if   Value1 < Value3
      then min := Value1
      else min := Value3
    end
end;

// -----------------------------------------------------------------------------
//
//			MinMaxDouble3
//
// -----------------------------------------------------------------------------
procedure MinMaxDouble3(const Value1,Value2,Value3:  Double; var min, max:  Double);
begin
    if   Value1 > Value2
    then begin
      if   Value1 > Value3
      then max := Value1
      else max := Value3;

      if   Value2 < Value3
      then min := Value2
      else min := Value3
    end
    else begin
      if   Value2 > Value3
      then max := Value2
      else max := Value3;

      if   Value1 < Value3
      then min := Value1
      else min := Value3
    end
end;



// -----------------------------------------------------------------------------
//
//			SetBitmapsEql
//
// -----------------------------------------------------------------------------
procedure SetBitmapsEql(const SrcBitmap,DstBitmap:TBitmap);
begin
//    if SrcBitmap.PixelFormat<>pf24Bit then SrcBitmap.PixelFormat:=pf24Bit;
    DstBitmap.PixelFormat:=SrcBitmap.PixelFormat;
    DstBitmap.Width:=SrcBitmap.Width;
    DstBitmap.Height:=SrcBitmap.Height;
    if SrcBitmap.PixelFormat in [pf1bit, pf4bit, pf8bit]
    then DstBitmap.Palette := CopyPalette(SrcBitmap.Palette);
end;


// -----------------------------------------------------------------------------
//
//			CopyBitmap
//
// -----------------------------------------------------------------------------
procedure CopyBitmap(const SrcBitmap,DstBitmap:TBitmap);
begin
  SetBitmapsEql(SrcBitmap,DstBitmap);
  DstBitmap.Canvas.CopyMode:=cmSrcCopy;
  DstBitmap.Canvas.Draw(0,0,SrcBitmap);

end;


// -----------------------------------------------------------------------------
//
//			RGBTriple
//
// -----------------------------------------------------------------------------
function RGBTriple(const red, green, blue:  BYTE):  TRGBTriple;
begin
  with RESULT DO
  begin
    rgbtRed   := red;
    rgbtGreen := green;
    rgbtBlue  := blue
  end
end;

// -----------------------------------------------------------------------------
//
//			ColorToRGB
//
// -----------------------------------------------------------------------------
function ColorToRGB(Color: TColor): TRGBTriple;
begin
  with Result DO
  begin
    rgbtred := Color AND $000000FF;
    rgbtgreen := (Color AND $0000FF00) SHR 8;
    rgbtblue := (Color AND $00FF0000) SHR 16;
  end;
end;

// -----------------------------------------------------------------------------
//
//			RGBToColor
//
// -----------------------------------------------------------------------------
function RGBtoColor(RGBTriple: TRGBTriple): TColor;
begin
    Result := RGBTriple.rgbtred OR (RGBTriple.rgbtgreen SHL 8) OR (RGBTriple.rgbtblue SHL 16);
end;

// -----------------------------------------------------------------------------
//
//			AssignRGBTriple
//
// -----------------------------------------------------------------------------
procedure AssignRGBTriple (var TargetTriple:  TRGBTriple;const OriginTriple:  TRGBTriple);
begin
    TargetTriple.rgbtRed   := OriginTriple.rgbtRed;
    TargetTriple.rgbtGreen := OriginTriple.rgbtGreen;
    TargetTriple.rgbtBlue  := OriginTriple.rgbtBlue
end {AssignRGBTriple};


// -----------------------------------------------------------------------------
//
//			HslDblToRGB
//
// -----------------------------------------------------------------------------
procedure HSLDblToRGB (H,S,L:Double; var RGB:TRGBTriple);
var
  M1,
  M2: double;

  function HueToColourValue (Hue: double) : byte;
  var
    V : double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else

      if Hue > 1 then
        Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
      if 2 * Hue < 1 then
        V := M2
      else
        if 3 * Hue < 2 then
          V := M1 + (M2 - M1) * (2/3 - Hue) * 6
        else
          V := M1;
    Result := round (255 * V)
  end;

var
  R,
  G,
  B: byte;
begin
  if S = 0 then
  begin
    R := round (255 * L);
    G := R;
    B := R
  end else begin
    if L <= 0.5 then

      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue (H + 1/3);
    G := HueToColourValue (H);
    B := HueToColourValue (H - 1/3)
  end;
  with RGB do
  begin
    rgbtRed:=R;
    rgbtGreen:=G;
    rgbtBlue:=B;
  end;
end;

// -----------------------------------------------------------------------------
//
//			HSLtRGB
//
// -----------------------------------------------------------------------------
procedure HSLToRGB (HSL : THSLTriple;var RGB:TRGBTriple);
begin
  HSLDblToRGB (HSL.hslHue / (359-1), HSL.hslSaturation / 255, HSL.hslLightness / 255,RGB)
end;

// -----------------------------------------------------------------------------
//
//			HSVtoRGB
//
// -----------------------------------------------------------------------------
procedure HSVtoRGB (const HSV:  THSVTriple;var   RGB  :  TRGBTriple);
var
  fraction :  INTEGER;
  Hue      :  INTEGER;
  p,q,t    :  INTEGER;
  VSSum    :  INTEGER;
  const
      divisor:  INTEGER = 255*60;
begin
  if   HSV.hsvSaturation = 0
  then AssignRGBTriple(RGB, RGBTriple(HSV.hsvValue, HSV.hsvValue, HSV.hsvValue)) else
  begin
    if   HSV.hsvHue = 360 then Hue := 0 else Hue := HSV.hsvHue;
    fraction := Hue MOD 60;
    Hue := Hue div 60;

    VSSum := HSV.hsvValue *HSV.hsvSaturation;
    p := HSV.hsvValue - VSSum div 255;
    q := HSV.hsvValue - (VSSum*fraction) div divisor;
    t := HSV.hsvValue - (VSSum*(60 - fraction)) div divisor;

    with RGB DO
    begin
      CASE Hue of
        0:  begin  rgbtRed := HSV.hsvValue;   rgbtGreen := t;   rgbtBlue := p  end;
        1:  begin  rgbtRed := q;   rgbtGreen := HSV.hsvValue;   rgbtBlue := p  end;
        2:  begin  rgbtRed := p;   rgbtGreen := HSV.hsvValue;   rgbtBlue := t  end;
        3:  begin  rgbtRed := p;   rgbtGreen := q;   rgbtBlue := HSV.hsvValue  end;
        4:  begin  rgbtRed := t;   rgbtGreen := p;   rgbtBlue := HSV.hsvValue  end;
        5:  begin  rgbtRed := HSV.hsvValue;   rgbtGreen := p;   rgbtBlue := q  end;
      end
    end
  end
end;


// -----------------------------------------------------------------------------
//
//			RGBIntensity
//
// -----------------------------------------------------------------------------
{See [Russ95, p. 41]}
function RGBIntensity(const RGB:  TRGBTriple):  INTEGER;
begin
  with RGB DO
    RESULT := (rgbtRed + rgbtGreen + rgbtBlue) div 3
end;


// -----------------------------------------------------------------------------
//
//			RGBLightness
//
// -----------------------------------------------------------------------------
{See [Foley96, p. 595]}
function RGBLightness(const RGB:  TRGBTriple):  INTEGER;
var
    min:  INTEGER;
      max:  INTEGER;
begin
  with RGB DO
    MinMaxInt3(rgbtRed, rgbtGreen, rgbtBlue, min, max);
  RESULT := (min + max) div 2
end ;


// -----------------------------------------------------------------------------
//
//			RGBSaturation
//
// -----------------------------------------------------------------------------
{See [Foley96, p. 592]}
function RGBSaturation(const RGB:  TRGBTriple):  INTEGER;
var
      MaxValue:  INTEGER;
      MinValue:  INTEGER;
begin
     with RGB DO
       MinMaxInt3(rgbtRed, rgbtGReen, rgbtBlue, MinValue, MaxValue);

    {Calculate saturation:  saturation is 0 if r, g and b are all 0}
    if   MaxValue = 0
    then RESULT := 0
    else RESULT := (255 * (MaxValue - MinValue)) div MaxValue;
end;


// -----------------------------------------------------------------------------
//
//			RGBValue
//
// -----------------------------------------------------------------------------
{See [Foley96, p. 592]}
function RGBValue(const RGB:  TRGBTriple):  INTEGER;
begin
    with RGB DO
      RESULT := xMaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] )
end;


// -----------------------------------------------------------------------------
//
//			RGBtoCMY
//
// -----------------------------------------------------------------------------
// Foley et al, 1996, p. 588
procedure RGBtoCMY(const RGB:  TRGBTriple;    // r, g and b IN [0..255]
                     var   CMY:  TCMYTriple);
begin
    with RGB DO
    begin
      CMY.cmykCyan := 255 - rgbtRed;
      CMY.cmykMagenta := 255 - rgbtGreen;
      CMY.cymkYellow := 255 - rgbtBlue
    end
end;


// -----------------------------------------------------------------------------
//
//			RGBtoCMYK
//
// -----------------------------------------------------------------------------
  // Foley et al, 1996, p. 589
procedure RGBtoCMYK(const RGB:  TRGBTriple;
                      var  CMYK:  TCMYKQuad);
var cmy:TCMYTriple;
begin
    cmy.cmykCyan:=CMYK.cmykCyan;
    cmy.cmykMagenta:=CMYK.cmykMagenta;
    cmy.cymkYellow:=CMYK.cmykYellow;
    RGBtoCMY(RGB, CMY);
    CMYK.cmykK := xMinIntValue([CMYK.cmykCyan, CMYK.cmykMagenta, CMYK.cmykYellow]);
    CMYK.cmykCyan := CMYK.cmykCyan - CMYK.cmykK;
    CMYK.cmykMagenta := CMYK.cmykMagenta - CMYK.cmykK;
    CMYK.cmykYellow := CMYK.cmykYellow - CMYK.cmykK
end ;


// -----------------------------------------------------------------------------
//
//			CMYKtoRGB
//
// -----------------------------------------------------------------------------
procedure CMYKToRGB(const CMYK:TCMYKQuad;var RGB:TRGBTriple);
var r,g,b:Byte;
begin
  r:= 255 - (CMYK.cmykCyan - MulDiv(CMYK.cmykCyan,CMYK.cmykK, 255) + CMYK.cmykK);
  g:= 255 - (CMYK.cmykMagenta - MulDiv(CMYK.cmykMagenta, CMYK.cmykK, 255) + CMYK.cmykK);
  b:= 255 - (CMYK.cmykYellow - MulDiv(CMYK.cmykYellow, CMYK.cmykK, 255) + CMYK.cmykK);
  if RGB.rgbtBlue < 0 then RGB.rgbtRed := 0 else RGB.rgbtRed := b;
  if RGB.rgbtGreen < 0 then RGB.rgbtGreen := 0 else RGB.rgbtGreen := g;
  if RGB.rgbtRed < 0 then RGB.rgbtBlue := 0 else RGB.rgbtBlue := r;
end;


// -----------------------------------------------------------------------------
//
//			RGBtoHSLDbl
//
// -----------------------------------------------------------------------------
  // RGB, each 0 to 255, to HLS.
  // H = 0 to 359 (corresponding to 0..359 degrees around hexcone)
  // S = 0 (shade of gray) to 255 (pure color)
  // S = 0 (black) to 255 {white)
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 595.  Floating point fractions, 0..1, replaced with
  // integer values, 0..255.
procedure RGBtoHSLDbl (RGB: TRGBTriple; var H, S, L : double);
var
  R,G,B,Delta,Max,
  Min: double;

begin
  R :=  RGB.rgbtRed / 255;
  G :=  RGB.rgbtGreen / 255;
  B :=  RGB.rgbtBlue / 255;
  MinMaxDouble3(R,G,B,Min,Max);
{ Max := MaxDouble2 (R, MaxDouble2 (G, B));
  Min := MinDouble2 (R, MinDouble2 (G, B));}

// calculate luminosity
  L := (Max + Min) / 2;

  if Max = Min then  // it's grey
  begin
    H := 0; // it's actually undefined
    S := 0
  end else begin
    Delta := Max - Min;

// calculate Saturation
    if L < 0.5 then
      S := Delta / (Max + Min)
    else
      S := Delta / (2 - Max - Min);

// calculate Hue
    if R = Max then
      H := (G - B) / Delta
    else
      if G = Max then
        H  := 2 + (B - R) /Delta
      else

        H := 4 + (R - G) / Delta;

    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

// -----------------------------------------------------------------------------
//
//			RGBtoHSL
//
// -----------------------------------------------------------------------------
procedure RGBtoHSL(RGB: TRGBTriple; var HSL : THSLTriple);
var
  Hd,
  Sd,
  Ld: double;
begin
  RGBtoHSLDbl (RGB, Hd, sd, ld);
  HSL.hslHue := round (hd * (359-1));
  HSL.hslSaturation := round (sd * 255);
  HSL.hslLightness := round (ld * 255);
end;

  {RGB, each 0 to 255, to HSV.
   H = 0 to 359 (corresponding to 0..359 degrees around hexcone)
   S = 0 (shade of gray) to 255 (pure color)
   V = 0 (black) to 255 {white)

   Based on C Code in "Computer Graphics -- Principles and Practice,"
   Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
   integer values, 0..255.
  }

  procedure RGBtoHSV (const RGB:    TRGBTriple;  {r, g and b IN [0..255]}
                      var   HSV:  THSVTriple);    {h IN 0..359; s,v IN 0..255}
    var
      Delta   :  INTEGER;
      MinValue:  INTEGER;
  begin
    with RGB DO
      MinMaxInt3(rgbtRed, rgbtGreen, rgbtBlue, MinValue, HSV.hsvValue);
    Delta := HSV.hsvValue - MinValue;

    {Calculate saturation:  saturation is 0 if r, g and b are all 0}
    if   HSV.hsvValue = 0
    then HSV.hsvSaturation := 0
    else HSV.hsvSaturation := (255 * Delta) div HSV.hsvValue;

    if   HSV.hsvSaturation = 0
    then HSV.hsvHue := 0   {Achromatic:  When s = 0, h is undefined but assigned the value 0}
    else begin    {Chromatic}

      with RGB DO
      begin
        if   rgbtRed = HSV.hsvValue
        then HSV.hsvHue := (60*(rgbtGreen-rgbtBlue)) div Delta            {degrees -- between yellow and magenta}
        else
          if   rgbtGreen = HSV.hsvValue
          then HSV.hsvHue := 120 + (60*(rgbtBlue-rgbtRed)) div Delta      {degrees -- between cyan and yellow}
          else
            if  rgbtBlue = HSV.hsvValue
            then HSV.hsvHue := 240 + (60*(rgbtRed-rgbtGreen)) div Delta;  {degrees -- between magenta and cyan}
      end;

      if   HSV.hsvHue < 0
      then HSV.hsvHue := HSV.hsvHue + 360;

    end
  end {RGBtoHSV};

// -----------------------------------------------------------------------------
//
//			YCbCrToRGB
//
// -----------------------------------------------------------------------------
procedure YCbCrToRGB(YCbCr:TYCbCrTriple; var RGB:  TRGBTRiple);
const C=256;
      c11:real= 0.0054980*C;
      c12:real= 0.0000000*C;
      c13:real= 0.0051681*C;
      c21:real= 0.0054980*C;
      c22:real=-0.0015446*C;
      c23:real=-0.0026325*C;
      c31:real= 0.0054980*C;
      c32:real= 0.0079533*C;
      c33:real= 0.0000000*C;
var   r,g,b:Integer;
begin

  r:=round(c11*YCbCr.ycbcrY +c12*(YCbCr.ycbcrCB-156) +c13*(YCbCr.ycbcrCR-137));
  g:=round(c21*YCbCr.ycbcrY +c22*(YCbCr.ycbcrCB-156) +c23*(YCbCr.ycbcrCR-137));
  b:=round(c31*YCbCr.ycbcrY +c32*(YCbCr.ycbcrCB-156) +c33*(YCbCr.ycbcrCR-137));
  if r<0   then r:=0;
  if g<0   then g:=0;
  if b<0   then b:=0;
  if r>255 then r:=255;
  if g>255 then g:=255;
  if b>255 then b:=255;
  with RGB do
  begin
    rgbtRed:=r;
    rgbtGreen:=g;
    rgbtBlue:=b;
  end;
end;

// -----------------------------------------------------------------------------
//
//			RGBAtoBGRA
//
// -----------------------------------------------------------------------------
procedure RGBAtoBGRA(RGBA:TRGBAQuad; var BGRA:TBGRAQuad);
begin
    BGRA.bgrBlue := RGBA.rgbBlue;
    BGRA.bgrRed := RGBA.rgbRed;
    BGRA.bgrGreen := RGBA.rgbGreen;
    BGRA.bgrAlpha := RGBA.rgbAlpha;
end;

// -----------------------------------------------------------------------------
//
//			RGBAtoBGRA_16
//
// -----------------------------------------------------------------------------
procedure RGBAtoBGRA_16(RGBA:TRGBAQuad; var BGRA:TBGRAQuad);
begin
    BGRA.bgrBlue := RGBA.rgbRed shr 8;
    BGRA.bgrRed := RGBA.rgbBlue shr 8;
    BGRA.bgrGreen := RGBA.rgbGreen shr 8;
    BGRA.bgrAlpha := RGBA.rgbAlpha shr 8;
end;


// -----------------------------------------------------------------------------
//
//			CIELABtoBGR
//
// -----------------------------------------------------------------------------
procedure CIELABtoBGR(CIELABTriple:TCIELABTriple; var BGRTriple:TBGRTriple);
var
  L, a, b,
  X, Y, Z, T,  YYn3: Double;
  SourcePtr,
  TargetPtr: PByte;
  PixelCount: Cardinal;

begin
    L := CIELABTriple.L / 2.56;
    A := CIELABTriple.A;
    B := CIELABTriple.B;

    YYn3 := (L + 16) / 116;
    if L < 7.9996 then
    begin
      Y := L / 903.3;
      X := A / 3893.5 + Y;
      Z := Y - b / 1557.4;
    end
    else
    begin
         T := YYn3 + a / 500;
	 X := T * T * T;
	 Y := YYn3 * YYn3 * YYn3;
	 T := YYn3 - b / 200;
	 Z := T * T * T;
    end;
    BGRTriple.bgrRed:= TrimInt(0,255,Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
    BGRTriple.bgrGreen:= TrimInt(0,255,Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
    BGRTriple.bgrBlue:=TrimInt(0,255,Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));

end;


// -----------------------------------------------------------------------------
//
//			CalculateGamma
//
// -----------------------------------------------------------------------------
function CalculateGamma(Gamma:Double;Value:Integer):Double;
begin
 if Value >255 then Value:=255;
 if Value >0 then
 result:=255* exp (Gamma * ln(Value/255))+0.5
 else result:=0;
end;

// -----------------------------------------------------------------------------
//
//			IntGamma
//
// -----------------------------------------------------------------------------
function IntGamma(Gamma:Double;Value:Integer):Integer;
begin
  result:=TrimReal(0,255,CalculateGamma(Gamma,Value));
end;


// -----------------------------------------------------------------------------
//
//			SwapL
//
// -----------------------------------------------------------------------------
function SwapL(Value:Cardinal):Cardinal;
asm
  bswap eax;
end;

// -----------------------------------------------------------------------------
//
//			SwapS
//
// -----------------------------------------------------------------------------
function SwapS(Value:WORD):WORD;
asm
@@Loop:       MOV CX, [EAX]
              XCHG CH, CL
              MOV [EAX], CX
              ADD EAX, 2
              DEC EDX
              JNZ @@Loop
end;

// -----------------------------------------------------------------------------
//
//			Deprediction1
//
// -----------------------------------------------------------------------------
procedure Deprediction1(p:Pointer; Count: Cardinal);
asm
      @@1:
         MOV CL, [EAX]
         ADD [EAX + 1], CL
         INC EAX
         DEC EDX
         JNZ @@1
end;

// -----------------------------------------------------------------------------
//
//			Deprediction3
//
// -----------------------------------------------------------------------------
procedure Deprediction3(p:Pointer; Count: Cardinal);
asm
         MOV ECX, EDX
         SHL ECX, 1
         ADD ECX, EDX
      @@1:
         MOV DL, [EAX]
         ADD [EAX + 3], DL
         INC EAX
         DEC ECX
         JNZ @@1
end;

// -----------------------------------------------------------------------------
//
//			Deprediction4
//
// -----------------------------------------------------------------------------
procedure Deprediction4(p:Pointer; Count: Cardinal);
asm
         SHL EDX, 2
      @@1:
         MOV CL, [EAX]
         ADD [EAX + 4], CL
         INC EAX
         DEC EDX
         JNZ @@1
end;

end.
