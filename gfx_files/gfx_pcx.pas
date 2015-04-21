unit gfx_pcx;
{***********************************************************************
Unit gfx_pcx.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi  4

    gfx_pcx is part of the gfx_library collection

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

interface
uses SysUtils,
     classes,
     Windows,
     JPEG,
     Graphics,
     io_files,
     gfx_errors,
     gfx_basedef;

// -----------------------------------------------------------------------------
//
//			PCX Classes
//
// -----------------------------------------------------------------------------

type TPCXColorMode=(pcx16,pcx256,pcxTrue);

type TPCXTriple = packed record
       r, g, b: byte;
     end;

     TPCXCol256 = array[0..255] of TPCXTriple;
     TPcxCol16 = array[0..15] of TPCXTriple;

type TPCXHeader=packed record
          pcxManufacturer:Byte;
          pcxVersion     :Byte; // Version information
                             // 0 = Version 2.5 of PC Paintbrush
                             // 2 = Version 2.8 w/palette information
                             // 3 = Version 2.8 w/o palette information
                             // 4 = PC Paintbrush for Windows(Plus for
                             //    Windows uses Ver 5)
                             // 5 = Version 3.0 and > of PC Paintbrush
                             //   and PC Paintbrush +, includes
                             //   Publisher's Paintbrush . Includes
                             //    24-bit .PCX files
          pcxEncoding    :Byte; // 1 = .PCX run length encoding
          pcxBitsPerPixel:Byte; // Number of bits to represent a pixel
                             //    (per Plane) - 1, 2, 4, or 8
          pcxXMin:Word;
          pcxYMin:Word;
          pcxXMax:Word;
          pcxyMax:Word;
          pcxHDpi  :Word;       // Horizontal Resolution of image in DPI*
          pcxVDpi  :Word;       // Vertical Resolution of image in DPI*
          pcxColormap :TPCXCol16; // Color palette setting
          pcxReserved :Byte;
          pcxNPlanes  :Byte;    // Number of color planes
          pcxBytesPerLine :Word;// Number of bytes to allocate for a scanline
          pcxPaletteInfo  :Word;// How to interpret palette- 1 = Color/BW,
                             //    2 = Grayscale (ignored in PB IV/ IV +)
          pcxHscreenSize  :Word;
          pcxVscreenSize  :Word;
          pcxFiller :Array[0..53] of Byte;

     end;
type
  TPCXBitmap = class(TBitmap)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

Type
  TPCXFile = class(TObject)
    FPCXHeader             :TPCXHeader;
    FPCXColorMode          :TPCXColorMode;
    FBitmap                :TBitmap;
    scan_line		   :array [0..65535] of  byte;

    Procedure ReadPCXLine(Stream:TStream);
    procedure FillPCXHeader;

    procedure ReadPCXHeader(Stream:TStream);
    procedure ReadPCXBodyTrue(Stream:TStream);
    procedure ReadPCXBody16(Stream:TStream);
    procedure ReadPCXBody256(Stream:TStream);

    procedure WritePCXHeader(Stream:TStream);
    procedure WritePCXBodyTrue(Stream:TStream);
    procedure WritePCXBody16(Stream:TStream);
    procedure WritePCXBody256(Stream:TStream);
  private
  public
    constructor Create;
    destructor Destroy;override;
    procedure LoadFromFile(filename: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(filename: String);
    procedure SaveToStream(Stream: TStream);
    procedure LoadBitmap(filename:String);
    procedure AssignBitmap(ABitmap:TBitmap);
    property Bitmap:TBitmap read FBitmap;
    property SaveMode:TPCXColorMode read FPCXColorMode write FPCXColorMode;
  end;


implementation
//***********************************************************
//
// TPCXFile
//
//***********************************************************

// -----------------------------------------------------------------------------
//
//			LoadFromFile
//
// -----------------------------------------------------------------------------
procedure TPCXFile.LoadFromFile(FileName: String);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Filename);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromStream
//
// -----------------------------------------------------------------------------
procedure TPCXFile.LoadFromStream(Stream: TStream);
begin
   Stream.Position := 0;
   ReadPCXHeader(Stream);
   if FPCXHeader.pcxVersion<>5 then
   begin
    if GFXRaiseErrors then raise  EGraphicFormat.Create('Invalid PCX Version (Ver.5 needed)');
    GFXFileErrorList.Add('Invalid PCX Version (Ver.5 needed)');
   end else
   try
     case FPCXColorMode of
       pcx16:   ReadPCXBody16(Stream);
       pcx256:  ReadPCXBody256(Stream);
       pcxTrue:  ReadPCXBodyTrue(Stream);
     end;
   except
    if GFXRaiseErrors then raise  EGraphicFormat.Create('PCX Error');
      GFXFileErrorList.Add('PCX Error');
    end;
end;

// -----------------------------------------------------------------------------
//
//			SaveToFile
//
// -----------------------------------------------------------------------------
procedure TPCXFile.SaveToFile(FileName: String);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  SaveToStream(Stream);
  Stream.SaveToFile(filename);
  Stream.Free;
end;

// -----------------------------------------------------------------------------
//
//			SaveToStream
//
// -----------------------------------------------------------------------------
procedure TPCXFile.SaveToStream(Stream: TStream);
begin
   Stream.Position := 0;
   WritePCXHeader(Stream);
   case FBitmap.PixelFormat of
     pf4Bit:   WritePCXBody16(Stream);
     pf8Bit:  WritePCXBody256(Stream);
     pf24Bit: WritePCXBodyTrue(Stream);
   end;
end;

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
constructor TPCXFile.Create;
begin
  FBitmap:=TBitmap.Create;
end;

// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
destructor TPCXFile.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
//
//			ReadPCXHeader
//
// -----------------------------------------------------------------------------
procedure TPCXFile.ReadPCXHeader(Stream:TStream);
begin
  Stream.Position:=0;
  with Stream,FPCXHeader do
  begin
      Read(pcxManufacturer,1);
      Read(pcxVersion,1);
      Read(pcxEncoding,1);
      Read(pcxBitsPerPixel,1);
      Read(pcxXMin,2);
      Read(pcxYMin,2);
      Read(pcxXMax,2);
      Read(pcxYMax,2);
      Read(pcxHDpi,2);
      Read(pcxVDpi,2);
      Read(pcxColormap,48);
      Read(pcxReserved,1);
      Read(pcxNPlanes,1);
      Read(pcxBytesPerLine,2);
      Read(pcxPaletteInfo,2);
      Read(pcxHscreenSize,2);
      Read(pcxVscreenSize,2);
      Position:=128;
  end;

    FBitmap.Width:=FPCXHeader.pcxXMax-FPCXHeader.pcxXMin+1;
    FBitmap.Height:=FPCXHeader.pcxYmax-FPCXHeader.pcxYMin+1;
    if (FPCXHeader.pcxBitsPerPixel=8) and (FPCXHeader.pcxNPlanes=3)
    then begin
       FPCXColorMode:=pcxTrue;
       FBitmap.PixelFormat:=pf24Bit;
    end else
    if (FPCXHeader.pcxBitsPerPixel=1) and (FPCXHeader.pcxNPlanes=4)
    then begin
       FPCXColorMode:=pcx16;
       FBitmap.PixelFormat:=pf4bit;
    end else
    if (FPCXHeader.pcxBitsPerPixel=8) and (FPCXHeader.pcxNPlanes=1)
    then begin
       FPCXColorMode:=pcx256;
       FBitmap.PixelFormat:=pf8Bit;
    end;
end;

// -----------------------------------------------------------------------------
//
//			ReadPCXLine
//
// -----------------------------------------------------------------------------
Procedure TPCXFile.ReadPCXLine(Stream:TStream);
Var
  databyte,db2:byte;
  repeater:SmallInt;
  Planes,n,p:Word;

procedure ReadLine;
Begin
 n:=0;
 repeat
    Stream.Read(databyte,1);
    If databyte >=$C0 Then
    Begin
      repeater:=databyte And $3F;
      Stream.Read(db2,1);
      while repeater>0 do
      begin
        scan_line[p]:=db2;
        Inc(p);
        dec(repeater);
        Inc(n);
      end;
    end else
      Begin
        scan_line[p]:=databyte;
        Inc(p);
        Inc(n);
      end;
 until n>=FPCXHeader.pcxBytesPerLine ;
End;

begin
  p:=0;
  fillchar(Scan_line,FPCXHeader.pcxBytesPerLine* FPCXHeader.pcxNPlanes,0);
  for Planes:=1 to FPCXHeader.pcxNPlanes do ReadLine;
end;

// -----------------------------------------------------------------------------
//
//			ReadPCXBody16
//
// -----------------------------------------------------------------------------
procedure TPCXFile.ReadPCXBody16(Stream:TStream);
begin
 // not implemented yet
end;

// -----------------------------------------------------------------------------
//
//			ReadPCXBody256
//
// -----------------------------------------------------------------------------
procedure TPCXFile.ReadPCXBody256(Stream:TStream);
var i,j:Integer;
    y: pByteArray;
    palArray: TPCXCol256;
    fPal:TMaxLogPalette;
    databyte:Byte;
    aStream:TMemoryStream;
begin
  fillchar(palArray,sizeof(palArray),0);
  aStream:=TMemoryStream.Create;
  Stream.Position:=0;
  aStream.LoadFromStream(Stream);

  fPal.palVersion:=$300;
  fPal.palNumEntries:=256;
  aStream.Position:=Stream.Size-(256*3+1);
  aStream.Read(databyte,1);
  if databyte =$0c then
  begin
    aStream.Read(palArray,SizeOf(palArray));
    for i:=0 to 255 do
    begin
        fPal.palPalEntry[i].peRed := palArray[i].r;
        fPal.palPalEntry[i].peGreen := palArray[i].g;
        fPal.palPalEntry[i].peBlue := palArray[i].b;
        fPal.palPalEntry[i].peFlags:=0;
    end;
  FBitmap.Palette:=CreatePalette(pLogPalette(@fPal)^);
  FBitMap.IgnorePalette:=False;
  end;


  aStream.Seek(128,soFromBeginning);

  for i:=0 to FBitmap.Height-1 do
  begin
    ReadPCXLine(aStream);
    y:=FBitmap.ScanLine[i];
    for j:=0 to FBitmap.Width-1 do y[j]:=scan_line[j];
  end;
  aStream.Free;
End;

// -----------------------------------------------------------------------------
//
//			ReadPCXBodyTrue
//
// -----------------------------------------------------------------------------
procedure TPCXFile.ReadPCXBodyTrue(Stream:TStream);
var i,j:Integer;
    y:pRGBArray;
    aStream:TMemoryStream;
begin
  aStream:=TMemoryStream.Create;
  Stream.Position:=0;
  aStream.LoadFromStream(Stream);
  aStream.Position:=128;
  for i:=0 to FBitmap.Height-1 do
  begin
    ReadPCXLine(aStream);
    y:=FBitmap.ScanLine[i];
    for j:=0 to FPCXHeader.pcxBytesPerLine-1 do
    begin
     y[j].rgbtRed:=scan_line[j];
     y[j].rgbtGreen:=scan_line[j+FPCXHeader.pcxBytesPerLine];
     y[j].rgbtBlue:=scan_line[j+(FPCXHeader.pcxBytesPerLine*2)];
    end;
  end;
  aStream.Free;
End;

// -----------------------------------------------------------------------------
//
//			WritePCXHeader
//
// -----------------------------------------------------------------------------
procedure TPCXFile.WritePCXHeader(Stream:TStream);
begin
  FillPCXHeader;
  with Stream,FPCXHeader do
  begin
    case FBitmap.PixelFormat of
    pf4Bit:  begin
            pcxBitsPerPixel:=4;
            pcxNPlanes:=1;
            end;
    pf8Bit: begin
            pcxBitsPerPixel:=8;
            pcxNPlanes:=1;
            end;
    pf24Bit:begin
            pcxBitsPerPixel:=8;
            pcxNPlanes:=3;
            end;
    end;
      Write(pcxManufacturer,1);
      Write(pcxVersion,1);
      Write(pcxEncoding,1);
      Write(pcxBitsPerPixel,1);
      Write(pcxXMin,2);
      Write(pcxYMin,2);
      Write(pcxXMax,2);
      Write(pcxYMax,2);
      Write(pcxHDpi,2);
      Write(pcxVDpi,2);
      Write(pcxColormap,48);
      Write(pcxReserved,1);
      Write(pcxNPlanes,1);
      Write(pcxBytesPerLine,2);
      Write(pcxPaletteInfo,2);
      Write(pcxHscreenSize,2);
      Write(pcxVscreenSize,2);
      Write(pcxFiller,SizeOf(pcxFiller));
      Position:=128;
  end;
end;

// -----------------------------------------------------------------------------
//
//			WritePCXBodyTrue
//
// -----------------------------------------------------------------------------
procedure TPCXFile.WritePCXBodyTrue(Stream:TStream);
var i,j,l:Integer;
    databyte,k:byte;
    dcount,nCount:Integer;
    y:pRGBArray;

procedure ConvertLine(Line:Integer);
var x:Integer;
begin
  y:=FBitmap.ScanLine[Line];
  for x:=0 to FBitmap.Width-1 do
  begin
     scan_line[x]:=y[x].rgbtRed;
     scan_line[x+FPCXHeader.pcxBytesPerLine]:=y[x].rgbtGreen;
     scan_line[x+(2*FPCXHeader.pcxBytesPerLine)]:=y[x].rgbtBlue;
  end;
end;

begin
  Stream.Position:=128;
  for i:=0 to FBitmap.Height-1 do
  begin
    ConvertLine(i);
    for l:=0 to 2 do begin
    j:=l*FPCXHeader.pcxBytesPerLine;
    nCount:=0;
    dcount:=1;
    repeat
      databyte:=scan_line[j];
      while (databyte=scan_line[j+1]) and (dcount<63) and (nCount<FPCXHeader.pcxBytesPerLine-1)
      do
      begin
        Inc(dcount);
        Inc(j);
        Inc(nCount);
      end;
      if (dcount>1) or (databyte>=$c0) then
      begin
         k:=dcount+$c0;
         Stream.Write(k,1);
         Stream.Write(databyte,1);
         dcount:=1;
         inc (ncount);
         Inc(j);
      end
      else
      begin
        Stream.Write(databyte,1);
        Inc (nCount);
        Inc(j);
      end;
   until ncount>=FPCXHeader.pcxBytesPerLine;
   end;
  end;
  databyte:=$0c;
  Stream.Write(databyte,1);
End;

// -----------------------------------------------------------------------------
//
//			WritePCXBody16
//
// -----------------------------------------------------------------------------
procedure TPCXFile.WritePCXBody16(Stream:TStream);
begin
 // not implemented yet
end;

// -----------------------------------------------------------------------------
//
//			WritePCXBody256
//
// -----------------------------------------------------------------------------
procedure TPCXFile.WritePCXBody256(Stream:TStream);
var i,j:Integer;
    databyte,k:byte;
    dcount,nCount:Integer;
    y:pByteArray;
    fPal:TPaletteEntries;

procedure ConvertLine(Line:Integer);
var x:Integer;
begin
  y:=FBitmap.ScanLine[Line];
  for x:=0 to FBitmap.Width-1 do
  begin
     scan_line[x]:=y[x];
  end;
end;

begin
  Stream.Position:=128;
  for i:=0 to FBitmap.Height-1 do
  begin
    ConvertLine(i);
    j:=0;
    nCount:=0;
    dcount:=1;
    repeat
      databyte:=scan_line[j];
      while (databyte=scan_line[j+1]) and (dcount<63) and (nCount<FPCXHeader.pcxBytesPerLine-1)
      do
      begin
        Inc(dcount);
        Inc(j);
        Inc(nCount);
      end;
      if (dcount>1) or (databyte>=$c0) then
      begin
         k:=dcount+$c0;
         Stream.Write(k,1);
         Stream.Write(databyte,1);
         dcount:=1;
         inc (ncount);
         Inc(j);
      end
      else
      begin
        Stream.Write(databyte,1);
        Inc (nCount);
        Inc(j);
      end;
   until ncount>=FPCXHeader.pcxBytesPerLine;
  end;
  databyte:=$0c;
  Stream.Write(databyte,1);
  GetPaletteEntries(FBitmap.Palette,0,256,fPal);
  for i:=0 to 255 do begin
     Stream.Write(fPal[i].peRed,1);
     Stream.Write(fPal[i].peGreen,1);
     Stream.Write(fPal[i].peBlue,1);
  end;
End;

// -----------------------------------------------------------------------------
//
//			LoadBitmap
//
// -----------------------------------------------------------------------------
procedure TPCXFile.LoadBitmap(filename:String);
begin
   try
     FBitmap.LoadFromFile(filename);
   except
     if GFXRaiseErrors then raise  EGraphicFormat.Create('Invalid bitmapfile (PCX)');
     GFXFileErrorList.Add('Invalid bitmapfile (PCX)');
   end;
end;

// -----------------------------------------------------------------------------
//
//			AssignBitmap
//
// -----------------------------------------------------------------------------
procedure TPCXFile.AssignBitmap(ABitmap:TBitmap);
begin
  FBitmap.Assign(ABitmap);
end;

// -----------------------------------------------------------------------------
//
//			FillPCXHeader
//
// -----------------------------------------------------------------------------
procedure TPCXFile.FillPCXHeader;
var x:Integer;
    fPalEntry:TPaletteEntries;
begin
     with FPCXHeader do
     begin
       pcxManufacturer:=10;
       pcxVersion:=5;
       pcxEncoding:=1;
       pcxXMin:=0;
       pcxYMin:=0;
       pcxXMax:=FBitmap.Width-1;
       pcxyMax:=FBitmap.Height-1;
       pcxHDpi:=FBitmap.Width;
       pcxVDpi:=FBitmap.Height;
//       if FBitmap.PixelFormat=pf4Bit then
       begin
         GetPaletteEntries(FBitmap.Palette,0,15,fPalEntry);
         for x:=0 to 15 do
         begin
           pcxColormap[x].r:=fPalEntry[x].peRed;
           pcxColormap[x].g:=fPalEntry[x].peGreen;
           pcxColormap[x].b:=fPalEntry[x].peBlue;
         end;
       end;
       pcxReserved:=0;
       case FBitmap.PixelFormat of
       pf4bit:begin
                pcxNPlanes:=1;
                pcxBitsPerPixel:=4;
                pcxBytesPerLine:=(FBitmap.Width+7) div 8;
                FPCXColorMode:=pcx16;
              end;
       pf8bit:begin
                pcxNPlanes:=1;
                pcxBitsPerPixel:=8;
                pcxBytesPerLine:=FBitmap.Width;
                FPCXColorMode:=pcx256;
              end;
       pf24bit:begin
                pcxNPlanes:=3;
                pcxBitsPerPixel:=8;
                pcxBytesPerLine:=FBitmap.Width;
                FPCXColorMode:=pcxTrue;
              end;
       end;
       pcxPaletteInfo:=1;
       pcxHscreenSize:=0;
       pcxVscreenSize:=0;
       for x:=0 to 53 do pcxFiller[x]:=0;
     end;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromStream
//
// -----------------------------------------------------------------------------
procedure TPCXBitmap.LoadFromStream(Stream: TStream);
var
  aPCX: TPCXFile;
  aStream: TMemoryStream;
begin
  aPCX := TPCXFile.Create;
  try
    aPCX.LoadFromStream(Stream);
    aStream := TMemoryStream.Create;
    try
      aPCX.Bitmap.SaveToStream(aStream);
      aStream.Position:=0;
      inherited LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  finally
    aPCX.Free;
  end;
end;


initialization
  TPicture.RegisterFileFormat('PCX','PCX-Format', TPCXBitmap);

finalization
   TPicture.UnRegisterGraphicClass(TPCXBitmap);

end.
