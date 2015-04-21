unit gfx_pcd;
{***********************************************************************
Unit gfx_pcd.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi 3 / 4

    gfx_pcd is part of the gfx_library collection

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
//			PCD classes
//
// -----------------------------------------------------------------------------

type
  TPCDBitmap = class(TBitmap)
  public
    procedure LoadFromStream(Stream: TStream);override;
  end;

Type
  TPCDFile = class(TObject)
    FBitmap                :TBitmap;
  public
    constructor Create;
    destructor Destroy;override;
    procedure LoadFromFile(filename: String);
    procedure LoadFromStream(Stream: TStream);
    property Bitmap:TBitmap read FBitmap write FBitmap;
end;

// -----------------------------------------------------------------------------
//
//			vars
//
// -----------------------------------------------------------------------------
var PCDSize: Integer;

implementation

//***********************************************************
//
// TPCDFile
//
//***********************************************************

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
constructor TPCDFile.Create;
begin
  FBitmap:=TBitmap.Create;
  PCDSize:=3;
end;

// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
destructor TPCDFile.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromFile
//
// -----------------------------------------------------------------------------
procedure TPCDFile.LoadFromFile(filename: String);
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
procedure TPCDFile.LoadFromStream(Stream: TStream);
Var y,x:word;
    Y1,Y2,CbCr:TBytearray;
    ycbcr1,ycbcr2:TYCbCrTriple;
    FirstRGB, SecondRGB:TRGBTriple;
    b:integer;
    Row,w,h,OriginH,OriginW:Integer;
    TargetRow:pRGBArray;
    DataByte:Byte;
    Vertical:Boolean;
Begin
    w:=0;
    h:=0;
    Stream.Position:=0;;
    Stream.Seek(72,soFromBeginning);
    Stream.Read(DataByte,1);
    if DataByte And 63<>8 then Vertical:=True else vertical := False;

    Case PCDsize Of
      1: Begin
           W:=192;
           H:=128;
           Stream.Seek($2000,soFromBeginning);
         End;
      2: Begin
           W:=384;
           H:=256;
           Stream.Seek($B800,soFromBeginning);
         End;
      3: Begin
           W:=768;
           H:=512;
           Stream.Seek($30000,soFromBeginning);
         End;
    End;
    OriginH:=h;
    OriginW:=w;
    if Vertical then //change the dimensions of the Bitmap, cause the image is
    begin            // stored in vertical direction
      b:=w;
      w:=h;
      h:=b;
    end;
    FBitmap.PixelFormat:=pf24Bit;
    FBitmap.Width:=w;
    FBitmap.Height:=h;
    Row:=0;
    For y:=0 To (originH div 2) -1 Do
    Begin
      Stream.Read(Y1,OriginW);
      Stream.Read(Y2,OriginW);
      Stream.Read(CbCr,OriginW);
      For x:=0 To OriginW-1 Do
      Begin
        with ycbcr1 do
        begin
          ycbcrY:= Y1[x];
          ycbcrCB:= CbCr[x Div 2];
          ycbcrCR:= CbCr[(OriginW Div 2)+(x Div 2)];
        end;
        with ycbcr2 do
        begin
          ycbcrY:= Y2[x];
          ycbcrCB:= CbCr[x Div 2];
          ycbcrCR:= CbCr[(OriginW Div 2)+(x Div 2)];
        end;

        YCbCrToRGB(ycbcr1,FirstRGB);
        YCbCrToRGB(ycbcr2,SecondRGB);
        if not Vertical then
        begin
          TargetRow:=FBitmap.ScanLine[Row];
          TargetRow[x]:=FirstRGB;
          TargetRow:=FBitmap.ScanLine[Row+1];
          TargetRow[x]:=SecondRGB;
        end else
        begin
          TargetRow:=FBitmap.ScanLine[ORiginW-x-1];
          TargetRow[Row]:=FirstRGB;
          TargetRow:=FBitmap.ScanLine[originW-x-1];
          TargetRow[Row+1]:=SecondRGB;
        end;
      End;
      Inc(Row,2);
    End;
End;

// -----------------------------------------------------------------------------
//
//			LoadFromStream
//
// -----------------------------------------------------------------------------
procedure TPCDBitmap.LoadFromStream(Stream: TStream);
var
  aPCD: TPCDFile;
  aStream: TMemoryStream;
begin
  aPCD := TPCDFile.Create;
  try
    aPCD.LoadFromStream(Stream);
    aStream := TMemoryStream.Create;
    try
      PCDSize:=1;
      aPCD.Bitmap.SaveToStream(aStream);
      aStream.Position:=0;
      inherited LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  finally
    aPCD.Free;
  end;
end;


initialization
  PCDSize:=3;
  TPicture.RegisterFileFormat('PCD','PCD-Format', TPCDBitmap);

finalization
   TPicture.UnRegisterGraphicClass(TPCDBitmap);

end.
