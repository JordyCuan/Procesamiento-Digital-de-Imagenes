unit gfx_tga;
{***********************************************************************
Unit gfx_tga.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi  4

    gfx_tga is part of the gfx_library collection

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
     gfx_basedef,
     gfx_errors,
     gfx_colors;

// -----------------------------------------------------------------------------
//
//			TGA classes
//
// -----------------------------------------------------------------------------

Type
  TTGAHeader=Packed Record
     tgaIdentSize:Byte;
     tgaColorMapType:Byte;
     tgaImageType:Byte;
     tgaColorMapStart:Word;
     tgaColorMapLength:Word;
     tgaColorMapBits:Byte;
     tgaXStart:Word;
     tgaYStart:Word;
     tgaWidth:Word;
     tgaHeight:Word;
     tgaBits:Byte;
     tgaDescriptor:Byte;
     End;

type TTGATriple = record
       b,g,r: byte;
     end;

     TTGASaveColorMode=(tga1Bit,tga8bit,tga24bit);

type
  TTGABitmap = class(TBitmap)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

Type
  TTGAFile = class(TObject)
    FTGAHeader             :TTGAHeader;
    FBitmap                :TBitmap;
    FTGASaveColorMode          :TTGASaveColorMode;
    scan_line:Array [0..65535] of Byte;
  private
    Procedure ReadTGALine(Stream:TStream);
    procedure FillTGAHeader;

    procedure ReadTGAHeader(Stream:TStream);
    procedure ReadTGABody(Stream:TStream);

    procedure WriteTGAHeader(Stream:TStream);
    procedure WriteTGABody(Stream:TStream);

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
    property SaveMode:TTGASaveColorMode read FTGASaveColorMode write FTGASaveColorMode;
  end;

// -----------------------------------------------------------------------------
//
//			const
//
// -----------------------------------------------------------------------------
const MyTGADescriptor:String[50]='TARGA-File written by MosLibrary for Delphi'+Chr($1A);



implementation
//***********************************************************
//
// TTGAFile
//
//***********************************************************

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
constructor TTGAFile.Create;
begin
  FBitmap:=TBitmap.Create;
end;

// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
destructor TTGAFile.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromFile
//
// -----------------------------------------------------------------------------
procedure TTGAFile.LoadFromFile(FileName: String);
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
procedure TTGAFile.LoadFromStream(Stream: TStream);
begin
   Stream.Position := 0;
   ReadTGAHeader(Stream);
   ReadTGABody(Stream);
end;

// -----------------------------------------------------------------------------
//
//			SaveToFile
//
// -----------------------------------------------------------------------------
procedure TTGAFile.SaveToFile(FileName: String);
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
procedure TTGAFile.SaveToStream(Stream: TStream);
begin
   Stream.Position := 0;
   WriteTGAHeader(Stream);
   WriteTGABody(Stream);
end;

// -----------------------------------------------------------------------------
//
//			ReadTGAHeader
//
// -----------------------------------------------------------------------------
procedure TTGAFile.ReadTGAHeader(Stream:TStream);
var i:Integer;
    b:Byte;
    w:Word;
    FMaxLogPalette         :TMaxLogPalette;
    Buffer:Array[1..4096] Of Char;
begin
  Stream.Position:=0;
  Stream.Read(FTGAHeader,sizeof(FTGAHeader));
  Stream.Read(Buffer,FTGAHeader.tgaIdentSize);
 If not(FTGAHeader.tgaImageType In [1,2,4,9,10,11]) or
  not(FTGAHeader.tgaBits In [1,4,8,16,24,32]) then
  begin
    if GFXRaiseErrors then raise  EGraphicFormat.Create('Not supported or invalid TGA-File');
      GFXFileErrorList.Add('Not supported or invalid TGA-File');
  end else
  begin
    FBitmap.Width:=FTGAHeader.tgaWidth;
    FBitmap.Height:=FTGAHeader.tgaHeight;
    case FTGAHeader.tgaBits of
      1:begin
           FBitmap.PixelFormat:=pf1Bit;
           FTGASaveColorMode:=tga1bit;
        end;
      8: begin
           FBitmap.PixelFormat:=pf8Bit;
           FTGASaveColorMode:=tga8bit;
         end;
      16,24,32:begin
           FBitmap.PixelFormat:=pf24Bit;
           FTGASaveColorMode:=tga24bit;
           end;
    end;

    if FTGAHeader.tgaColorMapType<>0 then
    begin
        FMaxLogPalette.palVersion:=$300;
        FMaxLogPalette.palNumEntries:=256;
        for i:=FTGAHeader.tgaColorMapStart to FTGAHeader.tgaColorMapStart+FTGAHeader.tgaColorMapLength-1 Do
        begin
          case FTGAHeader.tgaColorMapBits of
          24:begin
               Stream.Read(b,1);
               FMaxLogPalette.palPalEntry[i].peBlue:=b;
               Stream.Read(b,1);
               FMaxLogPalette.palPalEntry[i].peGreen:=b;
               Stream.Read(b,1);
               FMaxLogPalette.palPalEntry[i].peRed:=b;
               FMaxLogPalette.palPalEntry[i].peFlags:=0;
             end;
          16:begin
               Stream.Read(w,2);
               FMaxLogPalette.palPalEntry[i].peRed:=(((w shr 10) and $1F) shl 1)*4;
               FMaxLogPalette.palPalEntry[i].peGreen:=(((w shr 5) and $1F) shl 1)*4;
               FMaxLogPalette.palPalEntry[i].peBlue:=((w and $1F) shl 1)*4;
               FMaxLogPalette.palPalEntry[i].peFlags:=0;
             end;
          end;
        end;
    FBitmap.Palette:=CreatePalette(pLogPalette(@FMaxLogpalette)^);
    FBitMap.IgnorePalette:=False;
    end;
    If ((FTGAHeader.tgaBits=8) and (FTGAHeader.tgaColorMapType=0)) then MakeGreyPalette(FMaxLogPalette)
    else if FTGAHeader.tgaBits = 1 then
    begin
      FMaxLogPalette.palVersion:=$300;
      FMaxLogPalette.palNumEntries:=2;
      FMaxLogPalette.palPalEntry[0].peRed:=0;
      FMaxLogPalette.palPalEntry[0].peGreen:=0;
      FMaxLogPalette.palPalEntry[0].peBlue:=0;
      FMaxLogPalette.palPalEntry[0].peFlags:=0;
      FMaxLogPalette.palPalEntry[1].peRed:=63;
      FMaxLogPalette.palPalEntry[1].peGreen:=63;
      FMaxLogPalette.palPalEntry[1].peBlue:=63;
      FMaxLogPalette.palPalEntry[1].peFlags:=0;
      FBitmap.Palette:=CreatePalette(pLogPalette(@FMaxLogPalette)^);
      FBitMap.IgnorePalette:=False;
    end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			ReadTGALine
//
// -----------------------------------------------------------------------------
Procedure TTGAFile.ReadTGALine(Stream:TStream);
var w:word;
    i,n:Integer;
    offset:Integer;
    tgatriple:TTGATriple;
    b:Byte;
    repeater:Byte;
Begin
  FillChar(scan_line,FTGAHeader.tgaWidth*3,0);
  If FTGAHeader.tgaImageType In [1,2,3] Then
  Begin //uncompressed
     Case FTGAHeader.tgaBits Of
        1:Stream.Read(scan_line, (FTGAHeader.tgaWidth+7) Div 8);
        8:Stream.Read(scan_line, FTGAHeader.tgaWidth);
       16:begin
            offset:=0;
            for i:=0 to FTGAHeader.tgaWidth-1 do
            begin
              Stream.Read(w,2);
              scan_line[offset]:=(w  and $1F) shl 3;         //Blue
              scan_line[offset+1]:=((w shr 5) and $1F) shl 3; //Green
              scan_line[offset+2]:=((w shr 10) and $1F) shl 3;//Red
              Inc(offset,3);
            end;
          end;
       24:begin
            Stream.Read(scan_line,(FTGAHeader.tgaWidth*3));
          end;
       32:begin
            offset:=0;
            for i:=0 to FTGAHeader.tgaWidth-1 do
            begin
              Stream.Read(b,1);
              scan_line[offset]:=b;
              Stream.Read(b,1);
              scan_line[offset+1]:=b;
              Stream.Read(b,1);
              scan_line[offset+1]:=b;
              Inc(offset,3);
              Stream.Position:=Stream.Position+1;
            end;
          end;
     end; //end case
   end else //Compressed


   if FTGAHeader.tgaImageType In [9,10,11] then
   begin
     n:=0;
     offset:=0;
     repeat
       Stream.Read(b,1);
       repeater:=(b And $7F)+1;
       Inc(n,repeater);
       if b and $80 <>0 then //compressed bytes
       begin
         case FTGAHeader.tgaBits of
         1,8:begin
               Stream.Read(b,1);
               FillChar(scan_line[offset],repeater,b);
               Inc(Offset,repeater);
            end;
         16,24,32:begin
               if (FTGAHeader.tgaBits=32) or (FTGAHeader.tgaBits=24) then
               begin
                  Stream.Read(tgatriple,3)
               end
               else if (FTGAHeader.tgaBits=16) then
               begin
                 Stream.Read(w,2);
                 tgatriple.r:=((w shr 10) and $1F) shl 3;
                 tgatriple.g:=((w shr 5) and $1F) shl 3;
                 tgatriple.b:=((w shr 0) and $1F) shl 3;
               end;
                for i:=0 to repeater-1 do
                begin
                  scan_line[offset]:=tgatriple.b;
                  scan_line[offset+1]:=tgatriple.g;
                  scan_line[offset+2]:=tgatriple.r;
                  Inc(Offset,3);
                end;
               if (FTGAHeader.tgaBits=32) then Stream.Read(b,1);
             end;
         end;
       end else
       begin //uncompressed byte
          Case FTGAHeader.tgaBits Of
            1,8:begin
                 Stream.Read(scan_line[offset], repeater);
                 Inc(Offset,repeater);
              end;
            16:begin
                 for i:=0 to repeater -1 do
                 begin
                   Stream.Read(w,2);
                   scan_line[offset]:=(w  and $1F) shl 3;         //Blue
                   scan_line[offset+1]:=((w shr 5) and $1F) shl 3; //Green
                   scan_line[offset+2]:=((w shr 10) and $1F) shl 3;//Red
                   Inc(Offset,3);
                 end;
               end;
            24:begin
                 Stream.Read(scan_line[offset],repeater*3);
                 Inc(Offset,repeater*3);
               end;
            32:begin
                   Stream.Read(scan_line[offset],repeater*3);
                   Inc(Offset,repeater*3);
                   Stream.Position:=Stream.Position+1;
                end;
          end;
       end;
      until n>=FTGAHeader.tgaWidth;
   end; //end compressed

end;

// -----------------------------------------------------------------------------
//
//			ReadTGABody
//
// -----------------------------------------------------------------------------
procedure TTGAFile.ReadTGABody(Stream:TStream);
var i,j,v,w,x:Integer;
    y:pRGBArray;
    y2:pByteArray;
   aStream:TMemoryStream;
   Position,Increment:Integer;
   b1,b2,b3:Byte;
begin
  y:=nil;
  y2:=nil;
  i:=Stream.Position;
  aStream:=TMemoryStream.Create;
  Stream.Position:=0;
  aStream.LoadFromStream(Stream);
  aStream.Position:=i;
  if FTGAHeader.tgaImageType In [1,2,3,9,10,11] Then
  begin

    //check if picture is stored reverse (Horizontal)
    if FTGAHeader.tgaDescriptor and $20 =0
    then begin
       Position:=FBitmap.Height-1;
       Increment:=-1;
    end else
    begin
       Position:=0;
       Increment:=1;
    end;

    i:=0;
    while i< FBitmap.Height  do
    begin
      case FBitmap.Pixelformat of
        pf24Bit:y:=FBitmap.ScanLine[Position];
        pf8Bit: y2:=FBitmap.ScanLine[Position];
      end;
      Position:=Position+Increment;
      ReadTGALine(aStream);

      //check if picture is stored reverse (Vertical)
      if FTGAHeader.tgaDescriptor and $10 <>0 then
      begin
        case FBitmap.PixelFormat of
           pf8Bit:begin
                    x:= FTGAHeader.tgaWidth;
                    v:= 0;
                    for w:=0 to (FTGAHeader.tgaWidth div 2) -1 do
                    begin
                       b1:=scan_line[x];
                       scan_line[x]:=scan_line[v];
                       scan_line[v]:=b1;
                       Inc(v);
                       Dec(x);
                    end;
                  end;
           pf24Bit:Begin
                    x:= (FTGAHeader.tgaWidth*3)-3;
                    v:= 0;
                    for w:=0 to (FTGAHeader.tgaWidth div 2) -1 do
                    begin
                       b1:=scan_line[x];
                       b2:=scan_line[x+1];
                       b3:=scan_line[x+2];
                       scan_line[x]:=scan_line[v];
                       scan_line[x+1]:=scan_line[v+1];
                       scan_line[x+2]:=scan_line[v+2];
                       scan_line[v]:=b1;
                       scan_line[v+1]:=b2;
                       scan_line[v+2]:=b3;
                       v:=v+3;
                       x:=x-3;
                    end;
                   end;
        end;
      end;
      case FBitmap.PixelFormat of
          pf8Bit: Move(scan_line,y2^,FBitmap.Width-1);
          pf24Bit:begin
                    x:=0;
                    for j:=0 to FBitmap.Width-1 do
                    begin
                       y[j].rgbtRed:=scan_line[x+2];
                       y[j].rgbtGreen:=scan_line[x+1];
                       y[j].rgbtBlue:=scan_line[x];
                       x:=x+3;
                    end;
                  end;
       end;
     inc(i);
    end;
  end;
  aStream.Free;
End;

// -----------------------------------------------------------------------------
//
//			WriteTGAHeader
//
// -----------------------------------------------------------------------------
procedure TTGAFile.WriteTGAHeader(Stream:TStream);
var fPalEntries:TPaletteEntries;
    i:Integer;
begin
  FillTGAHeader;
  case Fbitmap.PixelFormat of
  pf8bit: begin
             FTGAHeader.tgaImageType:=1;
             FTGAHeader.tgaColorMapType:=1;
             FTGAHeader.tgaColorMapStart:=0;
             FTGAHeader.tgaColorMapLength:=256;
             FTGAHeader.tgaColorMapBits:=24;
             FTGAHeader.tgaBits:=8;
           end;
  pf24bit:begin
             FTGAHeader.tgaImageType:=2;
             FTGAHeader.tgaColorMapType:=0;
             FTGAHeader.tgaColorMapStart:=0;
             FTGAHeader.tgaColorMapLength:=0;
             FTGAHeader.tgaColorMapBits:=24;
             FTGAHeader.tgaBits:=24;
           end;
  end;
  Stream.Position:=0;
  Stream.Write(FTGAHeader,sizeof(FTGAHeader));
  Stream.Write(MyTGADescriptor[1],Length(MyTGADescriptor));
  if FBitmap.PixelFormat=pf8Bit then
  begin
    GetPaletteEntries(FBitmap.Palette,0,256,fpalEntries);
    for i:=0 to 255 do
    begin
      Stream.Write(fPalEntries[i].peBlue,1);
      Stream.Write(fPalEntries[i].peGreen,1);
      Stream.Write(fPalEntries[i].peRed,1);
    end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			WriteTGABody
//
// -----------------------------------------------------------------------------
procedure TTGAFile.WriteTGABody(Stream:TStream);
var i,j:Integer;
    y:pRGBArray;
    y2:pByteArray;
begin

  y2:=nil;
  y:=nil;
  for i:= 0 to FBitmap.Height -1 do
  begin
    if FBitmap.PixelFormat=pf24Bit then y:=FBitmap.ScanLine[i] else
    y2:=FBitmap.ScanLine[i];
    for j:=0 to FBitmap.Width -1 do
    begin
      case FBitmap.PixelFormat of
        pf8bit: begin
                   Stream.Write(y2[j],1);
                 end;
        pf24bit:begin
                   Stream.Write(y[j].rgbtBlue,1);
                   Stream.Write(y[j].rgbtGreen,1);
                   Stream.Write(y[j].rgbtRed,1);
                 end;
      end;
    end;
  end;

end;

// -----------------------------------------------------------------------------
//
//			LoadBitmap
//
// -----------------------------------------------------------------------------
procedure TTGAFile.LoadBitmap(filename:String);
begin
   try
     FBitmap.LoadFromFile(filename);
     case FBitmap.PixelFormat of
      pf1Bit:FTGASaveColorMode:=tga1Bit;
      pf8Bit:FTGASaveColorMode:=tga8Bit;
      pf24Bit:FTGASaveColorMode:=tga24Bit;
     end;
   except
     if GFXRaiseErrors then raise  EGraphicFormat.Create('Invalid bitmapfile (TGA)');
     GFXFileErrorList.Add('Invalid bitmapfile (TGA)');
   end;
end;

procedure TTGAFile.AssignBitmap(ABitmap:TBitmap);
begin
  FBitmap.Assign(ABitmap);
  case FBitmap.PixelFormat of
    pf1Bit:FTGASaveColorMode:=tga1Bit;
    pf8Bit:FTGASaveColorMode:=tga8Bit;
    pf24Bit:FTGASaveColorMode:=tga24Bit;
  end;
end;

// -----------------------------------------------------------------------------
//
//			FillTGAHeader
//
// -----------------------------------------------------------------------------
procedure TTGAFile.FillTGAHeader;
begin
  with FTGAHeader do
  begin
    tgaIdentSize:=Length(MyTGADescriptor);
    tgaXStart:=0;
    tgaYStart:=0;
    tgaWidth:=FBitmap.Width;
    tgaHeight:=Fbitmap.Height;
    tgaDescriptor:=$20;
  end;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromStream
//
// -----------------------------------------------------------------------------
procedure TTGABitmap.LoadFromStream(Stream: TStream);
var
  aTGA: TTGAFile;
  aStream: TMemoryStream;
begin
  aTGA := TTGAFile.Create;
  try
    aTGA.LoadFromStream(Stream);
    aStream := TMemoryStream.Create;
    try
      aTGA.Bitmap.SaveToStream(aStream);
      aStream.Position:=0;
      inherited LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  finally
    aTGA.Free;
  end;
end;


initialization
  TPicture.RegisterFileFormat('TGA','TGA-Format', TTGABitmap);

finalization
   TPicture.UnRegisterGraphicClass(TTGABitmap);

end.
