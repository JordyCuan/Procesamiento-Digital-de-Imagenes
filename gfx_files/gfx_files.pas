{***********************************************************************
Unit gfx_files.PAS v1.0  0799
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi 4

    gfx_files is part of the gfx_library collection

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

unit gfx_files;

interface
uses SysUtils,
     classes,
     Windows,
     JPEG,
     Graphics,
     io_files,
     gfx_basedef,
     {$ifdef GIF}
     GIFImage,
     {$endif}
     gfx_errors,
     gfx_tiff,
     gfx_tga,
     gfx_pcd,
     gfx_pcx,
     gfx_png;
{$R-}

//---------------------------------------------------------------------------
//
//			Global
//
// -----------------------------------------------------------------------------

//ModeA and ModeB are reserved for parameters passed to the converters

procedure LoadAnyImageToBMP(Bitmap:TBitmap;Filename:String;AsThumb:Boolean;ModeA,ModeB:Integer);
procedure LoadJPEGExplicit(Graphic:TGraphic;FileName:String;AsThumb:Boolean);
procedure SaveBMPToAnyFile(Bitmap:TBitmap;Filename:String;ModeA,ModeB:Integer);
procedure SetJPEGParams(PixelFormat:TJPEGPixelFormat;Performance:TJPEGPerformance;
                          Scale:TJPEgScale;Grayscale:Boolean;Smoothing:Boolean;ProgressiveDispl:Boolean;SaveQuality:TJPEGQualityRange);
procedure SetJPEGDefaults;

// ** Internal procedures
procedure _LoadJPEG(Bitmap:TBitmap;FileName:String;AsThumb:Boolean);
procedure _LoadBMP(Bitmap:TBitmap;FileName:String);
procedure _LoadPCX(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _LoadTGA(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _LoadWMF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _LoadPCD(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _LoadPNG(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
{$ifdef GIF}
procedure _LoadGIF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _SaveGIF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
{$endif}
procedure _SaveJPEG(Bitmap:TBitmap;FileName:String);
procedure _SaveBMP(Bitmap:TBitmap;FileName:String);
procedure _LoadTIFF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _SaveTIFF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _SavePCX(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _SaveTGA(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
procedure _SavePNG(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);

function GetProcessedWidth:Integer;
function GetProcessedHeight:Integer;
function GetProcessedCRC:Integer;
function GetProcessedDPI:Integer;
function GetProcessedPlanes:Integer;
function GetGenerateCRCState:Boolean;
function SetGenerateCRCState(State:Boolean):Boolean;

PROCEDURE PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);

Var _defJPEGPixelFormat:TJPEGPixelformat;
    _defJPEGPerformance:TJPEGPerformance;
    _defJPEGScale:      TJPEGScale;
    _defJPEGGrayScale:  Boolean;
    _defJPEGPrgrDisplay:Boolean;
    _defJPEGSmoothing:  Boolean;
    _defJPEGProgressEvent:TProgressEvent;
    _defJPEGQualityRange:TJPEGQualityRange;
    _ProcessedImgWidth,
    _ProcessedImgHeight,
    _ProcessedImgDPI,
    _ProcessedImgPlanes:Integer;
    _GenerateCheckSums:Boolean;
    _ProcessedCRC32:DWORD;




const _OpenFilterString ='All Imagefiles|*.bmp;*.emf;*.ico;*.jpg;*.pcd;*.pcx;*.png;*.tga;*.wmf|'+
                     'Bitmap (*.bmp)|*.bmp|'+
                     'Extended Metafile (*.emf)|*.emf|'+
                     'Icon (*.ico)|*.ico|'+
                     'JPEG (*.jpg)|*.jpg|'+
{$ifdef GIF}
                     'GIF (*.gif)|*.gif|'+
{$endif}
                     'Photo CD (*.pcd)|*.pcd|'+
                     'PCX (*.pcx)|*.pcx|'+
                     'PNG (*.png)|*.png|'+
                     'Targa (*.tga)|*.tga|'+
                     'TIFF (*.tif)|*.tif|'+
                     'Metafile (*.wmf)|*.wmf|';
const _SaveFilterString = 'Bitmap (*.bmp)|*.bmp|'+
                     'Extended Metafile (*.emf)|*.emf|'+
                     'Icon (*.ico)|*.ico|'+
                     'JPEG (*.jpg)|*.jpg|'+
{$ifdef GIF}
                     'GIF (*.gif)|*.gif|'+
{$endif}
                     'PCX (*.pcx)|*.pcx|'+
                     'PNG (*.png)|*.png|'+
                     'Targa (*.tga)|*.tga|'+
                     'TIFF (*.tif)|*.tif|'+
                     'Metafile (*.wmf)|*.wmf|';
const _GfxFileMask = '*.jpg;*.jpeg;*.bmp;*.tga;*.pcx;*.pcd'+
{$ifdef GIF}
                     ';*.gif'+
{$endif}
                     ';*.tif'+
                     ';*.png';

implementation





//***********************************************************
//
// Global
//
//***********************************************************


//---------------------------------------------------------------------------
//
//			LoadAnyImageToBMP
//
//-----------------------------------------------------------------------------
procedure LoadAnyImageToBMP(Bitmap:TBitmap;Filename:String;AsThumb:Boolean;ModeA,ModeB:Integer);
var FileExt:String;
begin

  _ProcessedImgWidth:=0;
  _ProcessedImgHeight:=0;
  _ProcessedCRC32:=0;
  if FileExists(FileName) then
  begin
  FileExt:=LowerCase(ExtractFileExt(FileName));
  if FileExt='.bmp' then _LoadBMP(Bitmap,FileName) else
  if FileExt='.jpg' then _LoadJPEG(Bitmap,FileName,AsThumb) else
{$ifdef GIF}
  if FileExt='.gif' then _LoadGIF(Bitmap,FileName,ModeA,ModeB) else
{$endif}
  if FileExt='.pcx' then _LoadPCX(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.tga' then _LoadTGA(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.wmf' then _LoadWMF(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.pcd' then _LoadPCD(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.tif' then _LoadTIFF(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.png' then _LoadPNG(Bitmap,FileName,ModeA,ModeB);
  if _GenerateCheckSums then _ProcessedCRC32:=CRC32File(FileName);
  end;
end;

//---------------------------------------------------------------------------
//
//			SaveBMPToAnyFile
//
//-----------------------------------------------------------------------------
procedure SaveBMPToAnyFile(Bitmap:TBitmap;Filename:String;ModeA,ModeB:Integer);
var FileExt:String;
begin
  FileExt:=LowerCase(ExtractFileExt(FileName));
  if FileExt='.bmp' then _SaveBMP(Bitmap,FileName) else
  if (FileExt='.jpg') or (FileExt='.jpeg') then _SaveJPEG(Bitmap,FileName) else
{$ifdef GIF}
  if FileExt='.gif' then _SaveGIF(Bitmap,FileName,ModeA,ModeB) else
{$endif}
  if FileExt='.pcx' then _SavePCX(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.png' then _SavePNG(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.tif' then _SaveTIFF(Bitmap,FileName,ModeA,ModeB) else
  if FileExt='.tga' then _SaveTGA(Bitmap,FileName,ModeA,ModeB);
end;

//---------------------------------------------------------------------------
//
//			LoadJPEGExplicit
//
//-----------------------------------------------------------------------------
procedure LoadJPEGExplicit(Graphic:TGraphic;FileName:String;AsThumb:Boolean);
var FileExt:String;
begin
  FileExt:=LowerCase(ExtractFileExt(FileName));
  if (FileExt='.jpg') or (FileExt='.jpeg') then
  begin
    Graphic.LoadFromFile(FileName);
  end;
end;

//---------------------------------------------------------------------------
//
//			SetJPEGParams
//
//-----------------------------------------------------------------------------
procedure SetJPEGParams(PixelFormat:TJPEGPixelFormat;Performance:TJPEGPerformance;
                          Scale:TJPEgScale;Grayscale:Boolean;Smoothing:Boolean;
                          ProgressiveDispl:Boolean;SaveQuality:TJPEGQualityRange);
begin
  _defJPEGSmoothing:=Smoothing;
  _defJPEGPrgrDisplay:=ProgressiveDispl;
  _defJPEGGrayScale:=Grayscale;
  _defJPEGScale:=Scale;
  _defJPEGPerformance:=Performance;
  _defJPEGPixelFormat:=PixelFormat;
  _defJPEGQualityRange:=SaveQuality;
end;

//---------------------------------------------------------------------------
//
//			SetJPEGDefaults
//
//---------------------------------------------------------------------------
procedure SetJPEGDefaults;
begin
 _defJPEGPixelFormat    :=jf24Bit;
 _defJPEGPerformance	:=jpBestQuality;
 _defJPEGScale      	:=jsFullSize;
 _defJPEGGrayScale  	:=False;
 _defJPEGPrgrDisplay	:=False;
 _defJPEGSmoothing  	:=True;
 _defJPEGProgressEvent	:=nil;
 _defJPEGQualityRange	:=90;
 _ProcessedImgWidth     :=0;
 _ProcessedImgHeight    :=0;
 _ProcessedImgDPI       :=0;
 _ProcessedImgPlanes    :=0;
end;

//---------------------------------------------------------------------------
//
//			_LoadJPEG
//
//---------------------------------------------------------------------------
procedure _LoadJPEG(Bitmap:TBitmap;FileName:String;AsThumb:Boolean);
var TempJPEG:TJPEGImage;
begin
   TempJPEG:=TJPEGImage.Create;
  try
    if Assigned(_defJPEGProgressEvent) then TempJPEG.OnProgress:=_defJPEGProgressEvent;
    try
      TempJPEG.LoadFromFile(FileName);
      TempJPEG.PixelFormat:=jf24Bit;
      TempJPEG.Grayscale:=_defJPEGGrayScale;
      if (TempJPEG.Width>0) and (TempJPEG.Height>0) then
      with TempJPEG do
      begin
        _ProcessedImgWidth:=Width;
        _ProcessedImgHeight:=Height;
        Case PixelFormat of
          jf24Bit: _ProcessedImgPlanes:=24;
          jf8Bit: _ProcessedImgPlanes:=8;
        end;
        PixelFormat:=_defJPEGPixelFormat;
        ProgressiveDisplay:=_defJPEGPrgrDisplay;
        if AsThumb then
        begin
          if (Width/8)>70 then Scale:=jsEighth else
          if (Width/4)>70 then Scale:=jsQuarter else
          if (Width/2)>70 then Scale:=jsHalf;
        end
        else Scale:=_defJPEGScale;
        Smoothing:=_defJPEGSmoothing;
        Performance:=_defJPEGPerformance;
        Bitmap.Width:=TempJPEG.Width;
        Bitmap.Height:=TempJPEG.Height;
        case TempJPEG.PixelFormat of
          jf24Bit:begin
                    Bitmap.PixelFormat:=pf24Bit;
                    Bitmap.Assign(TJPEGImage(TempJPEG));
                  end;
          jf8Bit: begin
                    TempJPEG.DIBNEeded;
                    Bitmap.PixelFormat:=pf24Bit;
                    Bitmap.Assign(TJPEGImage(TempJPEG));
                  end;
        end;
      end else
      begin
        Bitmap.Width:=1;
        Bitmap.Height:=1;
      end;
    except
      Bitmap.Width:=1;
      Bitmap.Height:=1;
      TempJPEG.Free;
      if GFXRaiseErrors then raise  EGraphicFormat.Create('JPEG loading error');
      GFXFileErrorList.Add('JPEG loading error');
    end;
    finally
      TempJPEG.Free;
    end;

end;

//---------------------------------------------------------------------------
//
//			_LoadBMP
//
//---------------------------------------------------------------------------
procedure _LoadBMP(Bitmap:TBitmap;FileName:String);
var tempBitmap:TBitmap;
begin
  tempBitmap:=TBitmap.Create;
  try
    tempBitmap.LoadFromFile(FileName);
    _ProcessedImgWidth    :=tempBitmap.Width;
    _ProcessedImgHeight   :=tempBitmap.Height;
    Bitmap.Assign(tempBitmap);
  finally
    tempBitmap.Free;
  end;
end;

{$ifdef GIF}
//---------------------------------------------------------------------------
//
//			_LoadGIF
//
//---------------------------------------------------------------------------
procedure _LoadGIF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var GifImage:TGifImage;
begin
        GIFImage := TGIFImage.Create;
        GifImage.Transparent:=False;
        TRY
          try
            GIFImage.LoadFromFile(Filename);
          except
                if GFXRaiseErrors then raise  EGraphicFormat.Create('GIF loading error');
          end;
      {    Bitmap.Height      := GIFImage.Height;
          Bitmap.Width       := GIFImage.Width;
          Bitmap.PixelFormat := pf24bit;
          Bitmap.Canvas.Draw(0,0, GIFImage.Bitmap);
       }   _ProcessedImgWidth:=Bitmap.Width;
          Bitmap.Assign(GifImage.Bitmap);
          Bitmap.PixelFormat := pf24bit;
          _ProcessedImgHeight:=Bitmap.Height;
        FINALLY
          GIFImage.Free
        end;
end;
{$endif}

//---------------------------------------------------------------------------
//
//			_LoadTIFF
//
//---------------------------------------------------------------------------
procedure _LoadTIFF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aTIFF:TTIFFFile;
begin
   aTIFF:=TTIFFFile.Create;
   aTIFF.LoadFromFile(FileName);
   Bitmap.Assign(aTIFF.Bitmap);
   _ProcessedImgWidth  :=Bitmap.Width;
   _ProcessedImgHeight :=Bitmap.Height;
   aTIFF.Free;
end;

procedure _LoadPCX(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aPcx:TPCXFile;
begin
   aPcx:=TPCXFile.Create;
   aPcx.LoadFromFile(FIleName);
   Bitmap.Assign(aPcx.Bitmap);
   _ProcessedImgWidth:=Bitmap.Width;
   _ProcessedImgHeight:=Bitmap.Height;
   aPcx.Free;
end;

//---------------------------------------------------------------------------
//
//			_LoadPNG
//
//---------------------------------------------------------------------------
procedure _LoadPNG(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aPNG:TPNGImage;
begin

   aPNG:=TPNGImage.Create;
   try
     aPNG.LoadFromFile(FIleName);
     Bitmap.Assign(aPNG);
     _ProcessedImgWidth:=Bitmap.Width;
     _ProcessedImgHeight:=Bitmap.Height;
   finally
     aPNG.Free;
   end;
end;

procedure _LoadTGA(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aTga:TTGAFile;
begin
   atga:=TtgaFile.Create;
   atga.LoadFromFile(FIleName);
   Bitmap.Assign(atga.Bitmap);
   _ProcessedImgWidth:=Bitmap.Width;
   _ProcessedImgHeight:=Bitmap.Height;
   atga.Free;
end;

//---------------------------------------------------------------------------
//
//			_LoadWMF
//
//---------------------------------------------------------------------------
procedure _LoadWMF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aWMF:TMetaFile;
begin
   aWMF:=TMetaFile.Create ;
   aWMF.LoadFromFile(FIleName);
   Bitmap.Width:=aWMF.Width;
   Bitmap.Height:=aWMF.Height;
   _ProcessedImgWidth:=Bitmap.Width;
   _ProcessedImgHeight:=Bitmap.Height;
   Bitmap.Canvas.Draw(0,0,aWMF);
   aWMF.Free;
end;

procedure _LoadPCD(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aPCD:TPCDFile;
begin
   aPCD:=TPCDFile.Create;
   if ModeA=0 then ModeA:=3;
   PCDSize:=ModeA;
   aPCD.LoadFromFile(FIleName);
   Bitmap.Assign(aPCD.Bitmap);
   _ProcessedImgWidth:=Bitmap.Width;
   _ProcessedImgHeight:=Bitmap.Height;
   aPCD.Free;
end;

//---------------------------------------------------------------------------
//
//			_SaveBMP
//
//---------------------------------------------------------------------------
procedure _SaveBMP(Bitmap:TBitmap;FileName:String);
begin
    Bitmap.SaveToFile(FileName);
end;

//---------------------------------------------------------------------------
//
//			_SaveJPEG
//
//---------------------------------------------------------------------------
procedure _SaveJPEG(Bitmap:TBitmap;FileName:String);
Var TempJPEG:TJPegImage;
begin
   TempJPEG:=TJPEGImage.Create;
   try
     TempJPEG.Assign(Bitmap);
     TempJPEG.CompressionQuality:=_defJPEGQualityRange;
     TempJPEG.SaveToFile(FileName);
   finally
     TempJPEG.Free;
   end;
end;

{$ifdef GIF}
//---------------------------------------------------------------------------
//
//			_SaveGIF
//
//---------------------------------------------------------------------------
procedure _SaveGIF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);

// Updated. Thanks to Alain Ferrut!
var GifImage:TGifImage;
begin
        Bitmap := ReduceColors(Bitmap, rmQuantize,dmNearest, 8,Bitmap.palette);// from Gifimage, reduce the bitmap palette  with the nearest colors.
        GIFImage := TGIFImage.Create;
        GifImage.Transparent:=False;
        TRY
          GIFImage.Height:=Bitmap.Height;
          GIFImage.Width:=Bitmap.Width;
          GifImage.Compression:=gcRLE; // no royalties for this GIF format
          GifImage.Assign(Bitmap);
          GifImage.SaveToFile(FileName);
        FINALLY
          GIFImage.Free
        end;
end;


{$endif}

//---------------------------------------------------------------------------
//
//			_SaveTIFF
//
//---------------------------------------------------------------------------
procedure _SaveTIFF(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aTIFF:TTIFFFile;
begin
   aTIFF:=TTIFFFile.Create;
   aTIFF.AssignBitmap(Bitmap);
   aTIFF.SaveToFile(FIleName);
   aTIFF.Free;
end;

//---------------------------------------------------------------------------
//
//			_SavePCX
//
//---------------------------------------------------------------------------
procedure _SavePCX(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aPcx:TPCXFile;
begin
   aPcx:=TPCXFile.Create;
   aPCX.AssignBitmap(Bitmap);
   aPcx.SaveToFile(FIleName);
   aPcx.Free;
end;

//---------------------------------------------------------------------------
//
//			_SavePNG
//
//---------------------------------------------------------------------------
procedure _SavePNG(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aPNG:TPNGImage;
begin
   aPNG:=TPNGImage.Create;
   aPNG.Assign(Bitmap);
   aPNG.SaveToFile(FIleName);
   aPNG.Free;
end;

//---------------------------------------------------------------------------
//
//			_SaveTGA
//
//---------------------------------------------------------------------------
procedure _SaveTGA(Bitmap:TBitmap;FileName:String;ModeA,ModeB:Integer);
var aTGA:TTGAFile;
begin
   aTGA:=TTGAFile.Create;
   aTGA.AssignBitmap(Bitmap);
   aTGA.SaveToFile(FIleName);
   aTGA.Free;
end;

//---------------------------------------------------------------------------
//
//			GetProcessedWidth
//
//---------------------------------------------------------------------------
function GetProcessedWidth:Integer;
begin
  GetProcessedWidth:=_ProcessedImgWidth;
end;

//---------------------------------------------------------------------------
//
//			_GetProcessedHeight
//
//---------------------------------------------------------------------------
function GetProcessedHeight:Integer;
begin
  GetProcessedHeight:=_ProcessedImgHeight;
end;

//---------------------------------------------------------------------------
//
//			GetProcessedCRC
//
//---------------------------------------------------------------------------
function GetProcessedCRC:Integer;
begin
  GetProcessedCRC:=_ProcessedCRC32;
end;

//---------------------------------------------------------------------------
//
//			GetProcessedDPI
//
//---------------------------------------------------------------------------
function GetProcessedDPI:Integer;
begin
  GetProcessedDPI:=_ProcessedImgDPI;
end;

//---------------------------------------------------------------------------
//
//			GetProcessedPlanes
//
//---------------------------------------------------------------------------
function GetProcessedPlanes:Integer;
begin
  GetProcessedPlanes:=_ProcessedImgPlanes;
end;

//---------------------------------------------------------------------------
//
//			GetGenerateCRCState
//
//---------------------------------------------------------------------------
function GetGenerateCRCState:Boolean;
begin
  GetGenerateCRCState:=_GenerateCheckSums;
end;

//---------------------------------------------------------------------------
//
//			SetGenerateCRCState
//
//---------------------------------------------------------------------------
function SetGenerateCRCState(State:Boolean):Boolean;
begin
  _GenerateCheckSums:=State;
  SetGenerateCRCState:=_GenerateCheckSums;
end;


//---------------------------------------------------------------------------
//
//			PrintBitmap
//
//---------------------------------------------------------------------------
PROCEDURE PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
VAR
      BitmapHeader:  pBitmapInfo;
      BitmapImage :  POINTER;
      HeaderSize  :  DWORD;
      ImageSize   :  DWORD;
BEGIN
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage,  ImageSize);
    TRY
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     // Destination Origin
                    DestRect.Right  - DestRect.Left, // Destination Width
                    DestRect.Bottom - DestRect.Top,  // Destination Height
                    0, 0,                            // Source Origin
                    Bitmap.Width, Bitmap.Height,     // Source Width & Height
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY)
    FINALLY
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    END
END;

initialization
  SetJPEGDefaults;
  _GenerateCheckSums:=False;
  _ProcessedCRC32:=0;

end.
















