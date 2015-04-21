{***********************************************************************
Unit gfx_RotResize.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,
    except the Resample function (c) by Anders Melander, anders@melander.dk

    Delphi version : Delphi 4

    gfx_RotResize is part of the gfx_library collection

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

    The Resample algorythm is taken from the sources from
    Bitmap resampler v1.02 release 3,
    written and (c) by Anders Melander, anders@melander.dk

********************************************************************************}
unit gfx_RotResize;

interface
uses Windows,Classes,Graphics,gfx_basedef;

type TFilterProc = function(Value: Single): Single;

     TResampleCallBack = procedure (const Min,Max,Pos:Integer);

     TSizeMode= (smUseZoomValue,smOriginal,smFitBoth,smFitWidth,smFitHeight);

     TCropMode=(crLeft,crRight,crTop,crBottom,CrLeftRight, crTopBottom,crAll);

PROCEDURE StretchResize(SrcBitmap,DestBitmap:TBitmap;dWidth,dHeight:LongInt;WithBorder:Boolean;BorderColor:TColor;Thumb:Boolean);
PROCEDURE FitResize(SrcBitmap,DestBitmap:TBitmap;dWidth,dHeight:LongInt;WithBorder:Boolean;BorderColor:TColor;Mode:Integer);
PROCEDURE RotateBitmap(SrcBitmap,DestBitmap:TBitmap;Degrees,CenterX,CenterY:Integer;EnlargeCanvas:Boolean;BackGrndColor:TColor;
          ResampleCallback:TResampleCallBack);

PROCEDURE FlipBitmap(SrcBitmap,DestBitmap:TBitmap;ResampleCallback:TResampleCallBack);
PROCEDURE MirrorBitmap(SrcBitmap,DestBitmap:TBitmap;ResampleCallback:TResampleCallBack);

procedure Resample(SrcBitmap, DstBitmap: TBitmap;NewWidth,NewHeight:LongInt;Filter: TFilterProc; fwidth: single;
          ResampleCallback:TResampleCallBack);

procedure CropBitmap(SrcBitmap,DestBitmap:TBitmap;Pixels:Integer;CropMode:TCropMode);
procedure CropBitmapToSelection(SrcBitmap,DestBitmap:TBitmap;SelectRect:TRect);
procedure EnlargeCanvas(SrcBitmap,DestBitmap:TBitmap;X,Y:Integer;Center:Boolean);

// Sample filters for use with Resample()
function SplineFilter(Value: Single): Single;
function BellFilter(Value: Single): Single;
function TriangleFilter(Value: Single): Single;
function BoxFilter(Value: Single): Single;
function HermiteFilter(Value: Single): Single;
function Lanczos3Filter(Value: Single): Single;
function MitchellFilter(Value: Single): Single;

implementation

// -----------------------------------------------------------------------------
//
//			Stretchresize Bitmap
//
// -----------------------------------------------------------------------------

PROCEDURE StretchResize(SrcBitmap,DestBitmap:TBitmap;dWidth,dHeight:LongInt;WithBorder:Boolean;BorderColor:TColor;Thumb:Boolean);
var aWidth,aHeight,dx,dy,mWidth,mHeight:LongInt;
    x:Extended;
begin

       aWidth:=SrcBitmap.Width;
       aHeight:=SrcBitmap.Height;
       mWidth:=dWidth;
       mHeight:=dHeight;
  if (aWidth>=mWidth) or (aHeight>=mHeight) then
  begin
     if aWidth> aHeight then
     begin
         x:=aWidth/mWidth;
         aWidth:=mWidth;
         if x <> 0 then aHeight:=round(aHeight*(1/x));
         if aHeight=0 then aHeight:=1;
       end
       else begin
         x:=aHeight/mHeight;
         aHeight:=mHeight;
         if x <> 0 then aWidth:=round(aWidth*(1/x));
         if aWidth=0 then aWidth:=1;
       end;
       if Assigned(DestBitmap) then with DestBitmap do
       begin
        PixelFormat:=SrcBitmap.PixelFormat;
        dx:=0;
        dy:=0;
        if WithBorder then
        begin
          dx:=Round((mWidth-aWidth)/2);
          dy:=Round((mHeight-aHeight)/2);
        end;
        Width:=mWidth;
        Height:=mHeight;
        Canvas.Brush.Color:=BorderColor;
        Canvas.FillRect(Rect(0,0,mWidth,mHeight));
        Canvas.Brush.Color:=clNone;
        Canvas.CopyMode:=cmSrcCopy;
        Canvas.StretchDraw(Rect(dx,dy,dx+aWidth,dy+aHeight),SrcBitmap)
     end;
  end
  else
  if (aWidth<mWidth) and (aHeight<mHeight)  then
  begin
   // if picture should be shown as thumb, then use the original imagedimensions
   if Thumb then
   begin
    if Assigned(DestBitmap) then with DestBitmap do
     begin
        Width:=mWidth;
        Height:=mHeight;
        Canvas.Brush.Color:=BorderColor;
        Canvas.FillRect(Rect(0,0,mWidth,mHeight));
        Canvas.Brush.Color:=clNone;
        Canvas.CopyMode:=cmSrcCopy;
        Canvas.Draw((mwidth-aWidth )div 2,(mHeight-aHeight )div 2,SrcBitmap);
     end;
   end else
   // otherwise, make result larger than original
   begin
     if aWidth> aHeight then
     begin
         x:=mWidth/aWidth;
         aWidth:=mWidth;
         if x <> 0 then aHeight:=round(aHeight*x);
       end
       else begin
         x:=mHeight/aHeight;
         aHeight:=mHeight;
         if x <> 0 then aWidth:=round(aWidth*x);
       end;

    if Assigned(DestBitmap) then with DestBitmap do
     begin
        dx:=0;
        dy:=0;
        if WithBorder then
        begin
          dx:=Round((mWidth-aWidth)/2);
          dy:=Round((mHeight-aHeight)/2);
        end;

        Width:=mWidth;
        Height:=mHeight;
        Canvas.Brush.Color:=BorderColor;
        Canvas.FillRect(Rect(0,0,mWidth,mHeight));
        Canvas.Brush.Color:=clNone;
        Canvas.CopyMode:=cmSrcCopy;
        Canvas.StretchDraw(Rect(dx,dy,dx+aWidth,dy+aHeight),SrcBitmap)
     end;
    end;
   end;
end;

// -----------------------------------------------------------------------------
//
//			Stretchresize Bitmap
//
// -----------------------------------------------------------------------------

PROCEDURE FitResize(SrcBitmap,DestBitmap:TBitmap;dWidth,dHeight:LongInt;WithBorder:Boolean;BorderColor:TColor;Mode:Integer);
var aWidth,aHeight,dx,dy,mWidth,mHeight:LongInt;
    x:Extended;
begin

  if Mode=0 then
  begin
    // change nothing
    DestBitmap.Assign(SrcBitmap);
    Exit;
  end;

  aWidth:=SrcBitmap.Width;
  aHeight:=SrcBitmap.Height;
  mWidth:=dWidth;
  mHeight:=dHeight;

//  if (aWidth>=mWidth) or (aHeight>=mHeight) then
  begin
    x:=1;

    case Mode of
    1:begin
       // fit both
       if aWidth> aHeight then
       begin
         if aWidth>mWidth then
         begin
           x:=aWidth/mWidth;
           aWidth:=mWidth;
           if x <> 0 then aHeight:=round(aHeight*(1/x));
           mHeight:=aHeight;
         end else
         begin
           x:=mWidth/aWidth;
           if x <> 0 then aHeight:=round(aHeight*(x));
           mHeight:=aHeight;
         end;
       end
       else
       begin
         if aHeight>mHeight then
         begin
           x:=aHeight/mHeight;
           aHeight:=mHeight;
           if x <> 0 then aWidth:=round(aWidth*(1/x));
           mWidth:=aWidth;
         end else
         begin
           x:=mHeight/aHeight;
           if x <> 0 then aWidth:=round(aWidth*(x));
           mWidth:=aWidth;
         end;
       end;
     end;
    2:begin
       //fit width
         if aWidth>mWidth then
         begin
           x:=aWidth/mWidth;
           aWidth:=mWidth;
           if x <> 0 then aHeight:=round(aHeight*(1/x));
           mHeight:=aHeight;
         end else
         begin
           mHeight:=aHeight;
           mWidth:=aWidth;

//           x:=mWidth/aWidth;
//           if x <> 0 then aHeight:=round(aHeight*(x));
         end;
     end;
    3:begin
       //fit height
         if aHeight>mHeight then
         begin
           x:=aHeight/mHeight;
           aHeight:=mHeight;
           if x <> 0 then aWidth:=round(aWidth*(1/x));
           mWidth:=aWidth;
         end else
         begin
           mHeight:=aHeight;
           mWidth:=aWidth;
         end;
     end;
    end;

    if Assigned(DestBitmap) then with DestBitmap do
    begin
      PixelFormat:=SrcBitmap.PixelFormat;
      dx:=0;
      dy:=0;
      if WithBorder then
      begin
        dx:=Round((mWidth-aWidth)/2);
        if dx<0 then dx:=0;
        dy:=Round((mHeight-aHeight)/2);
        if dy<0 then dy:=0;
      end;
      Width:=mWidth;
      Height:=mHeight;
      Canvas.Brush.Color:=BorderColor;
      Canvas.FillRect(Rect(0,0,mWidth,mHeight));
      Canvas.Brush.Color:=clNone;
      Canvas.CopyMode:=cmSrcCopy;
      Canvas.StretchDraw(Rect(dx,dy,dx+aWidth,dy+aHeight),SrcBitmap)
   end;
  end
end;

// -----------------------------------------------------------------------------
//
//			Rotate Bitmap
//
// -----------------------------------------------------------------------------

PROCEDURE RotateBitmap(SrcBitmap,DestBitmap:TBitmap;Degrees,CenterX,CenterY:Integer;EnlargeCanvas:Boolean;BackGrndColor:TColor;
          ResampleCallback:TResampleCallBack);
  VAR
    cosTheta,sinTheta,Theta  :  DOUBLE;
    Delta          :  INTEGER;
    ecX1,ecY1:  Integer;
    ecX2,ecY2:  Integer;
    ecX3,ecY3:  Integer;
    ecX4,ecY4:  Integer;
    xDiff,yDiff: Integer;
    minX,maxX:Integer;
    minY,maxY:Integer;
    i,j            :  INTEGER;
    iSrc,jSrc     :  INTEGER;
    iSrcPrime,iDestPrime     :  INTEGER;
    jSrcPrime,jDestPrime     :  INTEGER;
    SrcRow,DestRow           :  pRGBArray;
function GetRotatedY(OrgX,OrgY:Integer;SinTheta,CosTheta:Double):Integer;
begin
  Result:=(ROUND((2*(OrgX) + 1) * sinTheta + (2*(OrgY) + 1) * cosTheta) - 1) div 2;
end;
function GetRotatedX(OrgX,OrgY:Integer;SinTheta,CosTheta:Double):Integer;
begin
  Result:=(ROUND((2*(OrgX) + 1) * CosTheta - (2*(OrgY) + 1) * sinTheta) - 1) div 2;
end;

begin
  SrcBitmap.PixelFormat := pf24bit;
  DestBitmap.PixelFormat := pf24bit;
  Theta := -Degrees * PI / 180;
  sinTheta := SIN(Theta);
  cosTheta := COS(Theta);
  if EnlargeCanvas then
  begin
   ecX1 := GetRotatedX(0,0,SinTheta,CosTheta);
   ecY1 := GetRotatedY(0,0,SinTheta,CosTheta);
   ecX2 := GetRotatedX(SrcBitmap.Width,0,SinTheta,CosTheta);
   ecY2 := GetRotatedY(SrcBitmap.Width,0,SinTheta,CosTheta);
   ecX3 := GetRotatedX(SrcBitmap.Width,SrcBitmap.Height,SinTheta,CosTheta);
   ecY3 := GetRotatedY(SrcBitmap.Width,SrcBitmap.Height,SinTheta,CosTheta);
   ecX4 := GetRotatedX(0,SrcBitmap.Height,SinTheta,CosTheta);
   ecY4 := GetRotatedY(0,SrcBitmap.Height,SinTheta,CosTheta);
   if ecX1>=ecX2 then begin maxX:=ecX1;minX:=ecX2;end else begin maxX:=ecX2;minX:=ecX1; end;
   if ecY1>=ecY2 then begin maxY:=ecY1;minY:=ecY2;end else begin maxY:=ecY2;minY:=ecY1; end;
   if ecX3>=maxX then maxX:=ecX3 else if ecX3<=minX then minX:=ecX3;
   if ecY3>=maxY then maxY:=ecY3 else if ecY3<=minY then minY:=ecY3;
   if ecX4>=maxX then maxX:=ecX4 else if ecX4<=minX then minX:=ecX4;
   if ecY4>=maxY then maxY:=ecY4 else if ecY4<=minY then minY:=ecY4;
   DestBitmap.Width:=Abs(MaxX-MinX);
   DestBitmap.Height:=Abs(MaxY-MinY);
   XDiff :=(DestBitmap.Width-SrcBitmap.Width) div 2;
   YDiff :=(DestBitmap.height-SrcBitmap.Height) div 2;

  end else
  begin
    DestBitmap.Width  := SrcBitmap.Width;
    DestBitmap.Height := SrcBitmap.Height;
    yDiff:=0;
    xDiff:=0;
  end;
  FOR j := DestBitmap.Height-1 DOWNTO 0 DO
  BEGIN
    DestRow  := DestBitmap.Scanline[j];
    jSrcPrime := 2*(j - (YDiff+CenterY)) + 1;

   if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round(((DestBitmap.Height-j)/DestBitmap.Height)*100));

    FOR i := DestBitmap.Width-1 DOWNTO 0 DO
    BEGIN
      iSrcPrime := 2*(i - (XDiff+CenterX)) + 1;
      iDestPrime := ROUND(iSrcPrime * CosTheta - jSrcPrime * sinTheta);
      jDestPrime := ROUND(iSrcPrime * sinTheta + jSrcPrime * cosTheta);
      iSrc := (iDestPrime - 1) div 2 + CenterX;
      jSrc := (jDestPrime - 1) div 2 + CenterY;

      IF   (iSrc >= 0) AND (iSrc <= SrcBitmap.Width-1) AND
           (jSrc >= 0) AND (jSrc <= SrcBitmap.Height-1)
      THEN BEGIN
        SrcRow := SrcBitmap.Scanline[jSrc];
        DestRow[i]  := SrcRow[iSrc]
      END
      ELSE WITH DestRow[i] DO BEGIN
        rgbtBlue  := (BackgrndColor and $00ff0000) shr 16;
        rgbtGreen := (BackgrndColor and $0000ff00) shr 8;
        rgbtRed   := (BackgrndColor and $000000ff);
      END
    END
  END;
END;

// -----------------------------------------------------------------------------
//
//			Flip Bitmap
//
// -----------------------------------------------------------------------------

PROCEDURE FlipBitmap(SrcBitmap,DestBitmap:TBitmap;ResampleCallback:TResampleCallBack);
var i,j            :Integer;
    SrcRow,DestRow :pRGBArray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for i:=DestBitmap.Height-1 downto 0 do
  begin
   SrcRow:=SrcBitmap.ScanLine[DestBitmap.Height-i-1];
   DestRow:=DestBitmap.ScanLine[i];

   if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((i/SrcBitmap.Height)*100));

   for j:=0 to DestBitmap.Width-1 do begin
     DestRow[j].rgbtBlue:=SrcRow[j].rgbtBlue;
     DestRow[j].rgbtGreen:=SrcRow[j].rgbtGreen;
     DestRow[j].rgbtRed:=SrcRow[j].rgbtRed;
   end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			Mirror Bitmap
//
// -----------------------------------------------------------------------------

PROCEDURE MirrorBitmap(SrcBitmap,DestBitmap:TBitmap;ResampleCallback:TResampleCallBack);
var i,j            :Integer;
    SrcRow,DestRow :pRGBArray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for i:=DestBitmap.Height-1 downto 0 do
  begin
   SrcRow:=SrcBitmap.ScanLine[i];
   DestRow:=DestBitmap.ScanLine[i];

   if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((i/SrcBitmap.Height)*100));

   for j:=0 to DestBitmap.Width-1 do begin
     DestRow[j].rgbtBlue:=SrcRow[SrcBitmap.Width-j-1].rgbtBlue;
     DestRow[j].rgbtGreen:=SrcRow[SrcBitmap.Width-j-1].rgbtGreen;
     DestRow[j].rgbtRed:=SrcRow[SrcBitmap.Width-j-1].rgbtRed;
   end;
  end;
end;

{
RESAMPLE PART
}
// -----------------------------------------------------------------------------
//
//			Filter functions
// (c) by Anders Melander, anders@melander.dk
// -----------------------------------------------------------------------------

// Hermite filter
function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Box filter
// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.
function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Triangle filter
function TriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter
function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end else
    Result := 0.0;
end;

// B-spline filter
function SplineFilter(Value: Single): Single;
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5*tt*Value - tt + 2.0 / 3.0;
  end else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0/6.0 * Sqr(Value) * Value;
  end else
    Result := 0.0;
end;

// Lanczos3 filter
function Lanczos3Filter(Value: Single): Single;
  function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B		= (1.0 / 3.0);
  C		= (1.0 / 3.0);
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end else
  if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end else
    Result := 0.0;
end;


procedure Resample(SrcBitmap, DstBitmap: TBitmap;NewWidth,NewHeight:LongInt;Filter: TFilterProc; fwidth: single;
          ResampleCallback:TResampleCallBack);
// -----------------------------------------------------------------------------
//
//			Interpolator
// based on algorythm from Anders Melander, anders@melander.dk
// -----------------------------------------------------------------------------
type
  // Contributor for a pixel
  TContributor = record
    pixel: integer;		// Source pixel
    weight: single;		// Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n		: integer;
    p		: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    r, g, b	: single;
  end;
  // Physical bitmap scanline (row)
  TRGBList = packed array[0..0] of TColorRGB;
  PRGBList = ^TRGBList;
var
  xscale, yscale	: single;		// Zoom scale factors
  i, j, k		: integer;		// Loop variables
  center		: single;		// Filter calculation variables
  width, fscale, weight	: single;		// Filter calculation variables
  left, right		: integer;		// Filter calculation variables
  n			: integer;		// Pixel number
  Work			: TBitmap;              // Temporary Bitmap
  contrib		: PCListList;           // Contributor pointer
  rgb			: TRGB;                 // RGBTriple
  color			: TColorRGB;            // COlorRGBTriple
  SourceLine		,
  DestLine		: PRGBList;
  SourcePixel		,
  DestPixel		: PColorRGB;
  Delta			,
  DestDelta		: integer;
  SrcWidth		,
  SrcHeight		,
  DstWidth		,
  DstHeight		: integer;
  sMode                 : Boolean;
begin
  DstWidth := NewWidth;
  DstHeight := NewHeight;
  DstBitmap.Width:=NewWidth;
  DstBitmap.Height:=NewHeight;
  SrcWidth := SrcBitmap.Width;
  SrcHeight := SrcBitmap.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then exit;

  Work := TBitmap.Create;
  try
    Work.Height := SrcHeight;
    Work.Width := DstWidth;
    if (SrcWidth = 1) then
      xscale:= DstWidth / SrcWidth
    else
      xscale:= (DstWidth - 1) / (SrcWidth - 1);
    if (SrcHeight = 1) then
      yscale:= DstHeight / SrcHeight
    else
      yscale:= (DstHeight - 1) / (SrcHeight - 1);

    SrcBitmap.PixelFormat := pf24bit;
    DstBitmap.PixelFormat := SrcBitmap.PixelFormat;
    Work.PixelFormat := SrcBitmap.PixelFormat;

    //-----------------------------------------------------------
    //
    //  HORIZONTAL PART
    //
    //
    //-----------------------------------------------------------

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth* sizeof(TCList));
    // Horizontal sub-/supersampling

      // set different modes depending on scaling factor
      sMode:=xscale < 1.0;
      if sMode then width := fwidth / xscale else width:=fWidth;
      if sMode then fscale := 1.0 / xscale else fScale:=1;

      for i := 0 to DstWidth-1 do
      begin

       if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((i/DstWidth)*100));

        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        left := xFloor(center - width);
        right := xCeil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for k := 0 to SrcHeight-1 do
    begin

      if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((k/SrcHeight)*100));

      SourceLine := SrcBitmap.ScanLine[k];
      DestPixel := Work.ScanLine[k];
      for i := 0 to DstWidth-1 do
      begin
        rgb.r := 0.0;
        rgb.g := 0.0;
        rgb.b := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := SourceLine^[contrib^[i].p^[j].pixel];
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        color.r:=TrimReal(0,255,RGB.r);
        color.g:=TrimReal(0,255,RGB.g);
        color.b:=TrimReal(0,255,RGB.b);
        // Set new pixel value
        DestPixel^ := color;
        // Move on to next column
        inc(DestPixel);
      end;
    end;

    // Free the memory (horizontal filter weights)
    for i := 0 to DstWidth-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    //-----------------------------------------------------------
    //
    //  VERTICAL PART
    //
    //
    //-----------------------------------------------------------
    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight* sizeof(TCList));
    // Vertical sub-/supersampling

      sMode:=yscale < 1.0;
      if sMode then width := fwidth / yscale else width:=fWidth;
      if sMode then fscale := 1.0 / yscale else fScale:=1;
      for i := 0 to DstHeight-1 do
      begin

       if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((i/DstHeight)*100));

        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        left := xFloor(center - width);
        right := xCeil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
    SourceLine := Work.ScanLine[0];
    Delta := integer(Work.ScanLine[1]) - integer(SourceLine);
    DestLine := DstBitmap.ScanLine[0];
    DestDelta := integer(DstBitmap.ScanLine[1]) - integer(DestLine);
    for k := 0 to DstWidth-1 do
    begin

      if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round((k/DstWidth)*100));

      DestPixel := pointer(DestLine);
      for i := 0 to DstHeight-1 do
      begin
        rgb.r := 0;
        rgb.g := 0;
        rgb.b := 0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := PColorRGB(integer(SourceLine)+contrib^[i].p^[j].pixel*Delta)^;
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        color.r:=TrimReal(0,255,RGB.r);
        color.g:=TrimReal(0,255,RGB.g);
        color.b:=TrimReal(0,255,RGB.b);
        DestPixel^ := color;
        inc(integer(DestPixel), DestDelta);
      end;
      Inc(SourceLine, 1);
      Inc(DestLine, 1);
    end;

    // Free the memory (vertical filter weights)
    for i := 0 to DstHeight-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

  finally
    Work.Free;
  end;
end;


procedure EnlargeCanvas(SrcBitmap,DestBitmap:TBitmap;X,Y:Integer;Center:Boolean);
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  DestBitmap.Width:=X;
  DestBitmap.Height:=Y;
  if not Center then
  begin
    DestBitmap.Canvas.Draw(0,0,SrcBitmap);
  end else
  begin
    DestBitmap.Canvas.Draw((DestBitmap.Width-SrcBitmap.Width)div 2 ,
      (DestBitmap.Height-SrcBitmap.Height) div 2 ,
      SrcBitmap);
  end;
end;



procedure CropBitmap(SrcBitmap,DestBitmap:TBitmap;Pixels:Integer;CropMode:TCropMode);
var  x1,x2:Integer;
     y1,y2:Integer;
     SrcRect, DestRect: TRect;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  x1:=0;
  x2:=0;
  y1:=0;
  y2:=0;
  case CropMode of
  crLeft: x1:=Pixels;
  crRight:x2:=Pixels;
  crTop:y1:=Pixels;
  crBottom:y2:=Pixels;
  crTopBottom:
     begin
       y1:=Pixels;
       y2:=Pixels;
     end;
  crLeftRight:
     begin
       x1:=Pixels;
       x2:=Pixels;
     end;
  crAll:
     begin
       y1:=Pixels;
       y2:=Pixels;
       x1:=Pixels;
       x2:=Pixels;
     end;

  end;
  DestBitmap.Width:=SrcBitmap.Height-x1-x2;
  DestBitmap.Height:=SrcBitmap.Width-y1-y2;
  DestRect:=Rect(0,0,DestBitmap.Width,DestBitmap.Height);
  SrcRect:=Rect(x1,y1,SrcBitmap.Width-x2,SrcBitmap.Height-y2);
  DestBitmap.Canvas.CopyRect(DestRect,SrcBitmap.Canvas,SrcRect);
end;

procedure CropBitmapToSelection(SrcBitmap,DestBitmap:TBitmap;SelectRect:TRect);
var  x1,x2:Integer;
     y1,y2:Integer;
     SrcRect, DestRect: TRect;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  DestBitmap.Width:=SelectRect.Right-SelectRect.Left;
  DestBitmap.Height:=SelectRect.Bottom-SelectRect.Top;
  DestRect:=Rect(0,0,DestBitmap.Width,DestBitmap.Height);
  DestBitmap.Canvas.CopyRect(DestRect,SrcBitmap.Canvas,SelectRect);
end;

end.
