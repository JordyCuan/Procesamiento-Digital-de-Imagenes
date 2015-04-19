unit gfx_tiff;
{***********************************************************************
Unit gfx_tiff.PAS v1.0  0400
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi  4

    gfx_tiff is part of the gfx_library collection

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

    The TIFF converter algorythms are based on source codes by
        Mike Lischke (public@lischke-online.de) (decoder) and
        Wolfgang Krug (krug@sdm.de) (encoder)

********************************************************************************}

interface
uses Windows,
     Graphics,
     Classes,
     SysUtils,
     gfx_errors,
     gfx_compression,
     gfx_basedef;

// -----------------------------------------------------------------------------
//
//			TIFF classes
//
// -----------------------------------------------------------------------------
type
  TTIFFHeader = packed record
    tiffBOrder:        Word;
    tiffVersion:       Word;
    tiffIFD:           Cardinal;
  end;

  TImgFDEntry = packed record
    tiffTag:           Word;
    tiffDataType:      Word;
    tiffDataLength:    Cardinal;
    tiffOffset:        Cardinal;
  end;

  TTiffParams = packed record
    tiffBitsPerPlane         :Byte;
    tiffPhotoMetric          :Byte;
    tiffSamplesPerPixel      :Byte;
    tiffExtraSamples         :Byte;
    tiffRowsPerStrip         :TCardinalArray;
    tiffBitsPerSample        :TCardinalArray;
    tiffByteCount            :TCardinalArray;
    tiffOffsets              :TCardinalArray;
    tiffColorMap             :Cardinal;
    tiffStrip_Count          :Cardinal;
    tiffStripOffsets         :Cardinal;
    tiffStripByteCount       :Cardinal;
    tiffCompressionType      :Word;
    tiffPrediction           :Word;
    tiffColorSpace           :TColorSpace;
  end;

  TTIFFBitmap = class(TBitmap)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

Type
  TTIFFFile = class(TObject)
    fTIFFHeader             :TTIFFHeader;
    fTIFFParams             :TTiffParams;
    fBIG_ENDIAN             :Boolean;
    FBitmap                 :TBitmap;
    FPalette                :TPaletteWordArray;
    FStreamBase             :Integer;
    tiffIFD                 :array of TImgFDEntry;
    fid_c:WORD;
  private
    procedure ReadTIFFHeader(Stream:TStream);
    procedure ReadTIFFPalette(Stream:TStream; Colormap:Cardinal; BitsPerPixel:Byte; PMetric:Cardinal);
    procedure IFD_to_BIGENDIAN;
    function  GetParams(Param:Cardinal):Cardinal;
    procedure GetParamList(Param:Cardinal;Stream:TSTream; var VList: TCardinalArray );
    function  GetParamsLen(Param:Cardinal):Cardinal;

  public
    constructor Create;
    destructor Destroy;override;
    procedure LoadFromFile(filename: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(filename: String);
    procedure SaveToStream(Stream: TStream);
    procedure AssignBitmap(Bitmap:TBitmap);
    property  Bitmap:TBitmap read FBitmap;
  end;

// -----------------------------------------------------------------------------
//
//			const
//
// -----------------------------------------------------------------------------

  const
  TIFF_NOTYPE                   = 0;
  TIFF_BYTE                     = 1;
  TIFF_ASCII                    = 2;
  TIFF_SHORT                    = 3;
  TIFF_LONG                     = 4;
  TIFF_RATIONAL                 = 5;
  TIFF_SBYTE                    = 6;
  TIFF_UNDEFINED                = 7;
  TIFF_SSHORT                   = 8;
  TIFF_SLONG                    = 9;
  TIFF_SRATIONAL                = 10;
  TIFF_FLOAT                    = 11;
  TIFF_DOUBLE                   = 12;

  TIFF_BIGENDIAN                = $4D4D;
  TIFF_LITTLEENDIAN             = $4949;

  TIFF_SUBFILETYPE              = 254;
    FILETYPE_REDUCEDIMAGE       = $1;
    FILETYPE_PAGE               = $2;
    FILETYPE_MASK               = $4;
  TIFF_OSUBFILETYPE             = 255;
    OFILETYPE_IMAGE             = 1;
    OFILETYPE_REDUCEDIMAGE      = 2;
    OFILETYPE_PAGE              = 3;
  TIFF_IMAGEWIDTH               = 256;
  TIFF_IMAGELENGTH              = 257;
  TIFF_BITSPERSAMPLE            = 258;
  TIFF_COMPRESSION              = 259;
    COMPRESSION_NONE            = 1;
    COMPRESSION_CCITTRLE        = 2;
    COMPRESSION_LZW             = 5;
    COMPRESSION_PACKBITS        = 32773;
  TIFF_PHOTOMETRIC              = 262;
    PHOTOMETRIC_MINISWHITE      = 0;
    PHOTOMETRIC_MINISBLACK      = 1;
    PHOTOMETRIC_RGB             = 2;
    PHOTOMETRIC_PALETTE         = 3;
    PHOTOMETRIC_MASK            = 4;
    PHOTOMETRIC_SEPARATED       = 5;
    PHOTOMETRIC_YCBCR           = 6;
    PHOTOMETRIC_CIELAB          = 8;
  TIFF_DOCUMENTNAME             = 269;
  TIFF_IMAGEDESCRIPTION         = 270;
  TIFF_STRIPOFFSETS             = 273;
  TIFF_ORIENTATION              = 274;
    ORIENTATION_TOPLEFT         = 1;
    ORIENTATION_TOPRIGHT        = 2;
    ORIENTATION_BOTRIGHT        = 3;
    ORIENTATION_BOTLEFT         = 4;
    ORIENTATION_LEFTTOP         = 5;
    ORIENTATION_RIGHTTOP        = 6;
    ORIENTATION_RIGHTBOT        = 7;
    ORIENTATION_LEFTBOT         = 8;
  TIFF_SAMPLESPERPIXEL          = 277;
  TIFF_ROWSPERSTRIP             = 278;
  TIFF_STRIPBYTECOUNTS          = 279;
  TIFF_XRESOLUTION              = 282;
  TIFF_YRESOLUTION              = 283;
  TIFF_PLANARCONFIG             = 284;
    PLANARCONFIG_CONTIG         = 1;
    PLANARCONFIG_SEPARATE       = 2;
  TIFF_RESOLUTIONUNIT           = 296;
    RESUNIT_NONE                = 1;
    RESUNIT_INCH                = 2;
    RESUNIT_CENTIMETER          = 3;
  TIFF_SOFTWARE                 = 305;
  TIFF_DATETIME                 = 306;
  TIFF_ARTIST                   = 315;
  TIFF_PREDICTOR                = 317;
    PREDICTION_NONE             = 1;
    PREDICTION_HORZ_DIFFERENCING = 2;
  TIFF_COLORMAP                 = 320;
  TIFF_EXTRASAMPLES             = 338;
    EXTRASAMPLE_UNSPECIFIED     = 0;
    EXTRASAMPLE_ASSOCALPHA      = 1;
    EXTRASAMPLE_UNASSALPHA      = 2;

  TIFF_VERSION                  = 42;

// -----------------------------------------------------------------------------
//
//			vars
//
// -----------------------------------------------------------------------------
var
 tiffIFD_RGB : array[0..15] of TImgFDEntry = (
 ( tiffTag: TIFF_SUBFILETYPE; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_IMAGEWIDTH; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_IMAGELENGTH; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_BITSPERSAMPLE; tiffDataType: $0003; tiffDataLength: $00000003; tiffOffset: $00000008 ),
 ( tiffTag: TIFF_COMPRESSION; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: COMPRESSION_NONE ),
 ( tiffTag: TIFF_PHOTOMETRIC; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: PHOTOMETRIC_RGB),
 ( tiffTag: TIFF_STRIPOFFSETS; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_SAMPLESPERPIXEL; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000003 ),
 ( tiffTag: TIFF_ROWSPERSTRIP; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_STRIPBYTECOUNTS; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_XRESOLUTION; tiffDataType: $0005; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_YRESOLUTION; tiffDataType: $0005; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_PLANARCONFIG; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: PLANARCONFIG_CONTIG ),
 ( tiffTag: TIFF_RESOLUTIONUNIT; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: RESUNIT_INCH),
 ( tiffTag: TIFF_SOFTWARE; tiffDataType: $0002; tiffDataLength: $0000000C; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_EXTRASAMPLES; tiffDataType: $0002; tiffDataLength: $00000001; tiffOffset: EXTRASAMPLE_UNSPECIFIED ));

 tiffIFD_PAL : array[0..14] of TImgFDEntry = (
 ( tiffTag: TIFF_SUBFILETYPE; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_IMAGEWIDTH; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_IMAGELENGTH; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_BITSPERSAMPLE; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000008 ),
 ( tiffTag: TIFF_COMPRESSION; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: COMPRESSION_NONE ),
 ( tiffTag: TIFF_PHOTOMETRIC; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset:  PHOTOMETRIC_PALETTE),
 ( tiffTag: TIFF_STRIPOFFSETS; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_SAMPLESPERPIXEL; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: $00000001 ),
 ( tiffTag: TIFF_ROWSPERSTRIP; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_STRIPBYTECOUNTS; tiffDataType: $0004; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_XRESOLUTION; tiffDataType: $0005; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_YRESOLUTION; tiffDataType: $0005; tiffDataLength: $00000001; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_RESOLUTIONUNIT; tiffDataType: $0003; tiffDataLength: $00000001; tiffOffset: RESUNIT_INCH),
 ( tiffTag: TIFF_SOFTWARE; tiffDataType: $0002; tiffDataLength: $0000000C; tiffOffset: $00000000 ),
 ( tiffTag: TIFF_COLORMAP; tiffDataType: $0003; tiffDataLength: $00000300; tiffOffset: $00000008 ));

  NoOfDirs      : array[0..1] of Byte = ( $0F, $00 );
  NullString    : array[0..3] of Byte = ( $00, $00, $00, $00 );
  XRes   	: array[0..7] of Byte = ( $6D,$03,$00,$00,  $0A,$00,$00,$00 );
  YRes   	: array[0..7] of Byte = ( $6D,$03,$00,$00,  $0A,$00,$00,$00 );
  Software      : array[0..11] of Char = ( 'M', 'O', 'S', '-', 'P', 'R', 'O', 'J', 'E', 'C', 'T', 'S');
  BPS 		: array[0..2] of Word = ( $0008, $0008, $0008 );

var ScanLineArray:Array[0..65535] Of Byte;

implementation

//***********************************************************
//
// TTIFFFile
//
//***********************************************************

// -----------------------------------------------------------------------------
//
//			LoadFromStream
//
// -----------------------------------------------------------------------------
procedure TTIFFBitmap.LoadFromStream(Stream: TStream);
var
  aTIFF: TTIFFFile;
  aStream: TMemoryStream;
begin
  aTIFF := TTIFFFile.Create;
  try
    aTIFF.LoadFromStream(Stream);
    aStream := TMemoryStream.Create;
    try
      aTIFF.Bitmap.SaveToStream(aStream);
      aStream.Position:=0;
      inherited LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  finally
    aTIFF.Free;
  end;
end;

// -----------------------------------------------------------------------------
//
//			LoadFromFile
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.LoadFromFile(FileName: String);
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
procedure TTIFFFile.LoadFromStream(Stream: TStream);
begin
   Stream.Position := 0;
   FStreamBase:=Stream.Position;
   ReadTIFFHeader(Stream);
end;

// -----------------------------------------------------------------------------
//
//			SaveToFile
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.SaveToFile(FileName: String);
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
procedure TTIFFFile.SaveToStream(Stream: TStream);
var cBuffer:Cardinal;
    i,j,divisor, shift ,bs,col_count,bps, width:Integer;
    picStream:TMemoryStream;
    RGB:TRGBTriple;
    BGR:TBGRTripleByte;
    BGRA:TBGRAQuadByte;
    SrcRow:pRGBArray;
    SrcScanLine:pScanLine;
    o_ColMap,o_IFD, o_XRES,o_YRES,o_BPS,o_SOFTWARE, o_STRIP: Integer;
    fPal:TPaletteEntries;
    fPalArray:TPaletteWordArray;
begin
    if not Assigned(FBitmap) then Exit;
    Stream.Write ( fTIFFHeader, sizeof(fTIFFHeader));
    o_XRES := Stream.Position;
    Stream.Write (XRes,sizeof(XRes));
    o_YRES := Stream.Position;
    Stream.Write (YRes,sizeof(YRes));
    o_BPS := Stream.Position;
    Stream.Write (BPS,sizeof(BPS));
    o_SOFTWARE := Stream.Position;
    Stream.Write (Software,sizeof(Software));
    o_STRIP := Stream.Position;



    picStream:=TMemoryStream.Create;
    try
    picStream.Position:=0;

    width:=FBitmap.Width;
    divisor:=1;
    case fTIFFParams.tiffColorSpace of
    cs8Bit,cs4bit,cs1bit:
             begin
               case fTIFFParams.tiffColorSpace of
                 cs1Bit:begin
                        col_count:=2;
                        bps:=1;
                        width := (width div 8) * 8;
                        divisor:=8;
                        end;
                 cs4Bit:begin
                        col_count:=16;
                        bps:=4;
                        shift:=0;
                        width := (width div bps) * bps;
                        divisor:=2;
                        end;
                 cs8Bit:begin
                        col_count:=256;
                        shift:=8;
                        bps:=8;
                        divisor:=1;
                        end;
               end;

               GetPaletteEntries(FBitmap.Palette,0,col_Count,fPal);
               for i:=0 to col_count-1 do
               begin
                 case fTIFFParams.tiffColorSpace of
                   cs1Bit:begin
                     fPalArray[0,i]:=fPal[i].peRed + fPal[i].peRed shl 8 ;
                     fPalArray[1,i]:=fPal[i].peGreen +fPal[i].peGreen shl 8;
                     fPalArray[2,i]:=fPal[i].peBlue + fPal[i].peBlue shl 8;
                     end;
                   cs4Bit:begin
                     fPalArray[0,i]:=fPal[i].peRed+ fPal[i].peRed shl 8;
                     fPalArray[0,i+16]:=fPal[i].peGreen +fPal[i].peGreen shl 8;
                     fPalArray[0,i+32]:=fPal[i].peBlue + fPal[i].peBlue shl 8;
                     end;
                   cs8Bit:begin
                     fPalArray[0,i]:=fPal[i].peRed shl 8;
                     fPalArray[1,i]:=fPal[i].peGreen shl 8;
                     fPalArray[2,i]:=fPal[i].peBlue shl 8;
                     end;
                  end;
               end;
               o_colmap:=Stream.Position;
               Stream.Write ( fPalArray, 2* 3* col_Count);
               o_STRIP:=Stream.Position;
               picStream.Position:=0;

               for i:=0 to FBitmap.Height-1 do
               begin
                 SrcScanLine:=FBitmap.ScanLine[i];
                 picStream.Write(SrcScanLine^,Width div Divisor);
               end;
              picStream.Position:=0;
               Stream.CopyFrom(picStream,picStream.Size);
               tiffIFD_PAL[1].tiffOffset := Cardinal(Width);
               tiffIFD_PAL[2].tiffOffset := Cardinal(FBitmap.Height);
               tiffIFD_PAL[3].tiffOffset := Cardinal(bps);
               if fTIFFParams.tiffColorSpace=cs1bit then tiffIFD_PAL[5].tiffOffset := Cardinal(PHOTOMETRIC_MINISWHITE);
               tiffIFD_PAL[8].tiffOffset := Cardinal(FBitmap.Height);
               tiffIFD_PAL[9].tiffOffset := Cardinal(width  *FBitmap.Height);
               tiffIFD_PAL[14].tiffDataLength := Cardinal(col_count * 3);
               tiffIFD_PAL[14].tiffOffset := o_Colmap;
               tiffIFD_PAL[ 6].tiffOffset := o_STRIP;
               tiffIFD_PAL[10].tiffOffset := o_XRES;
               tiffIFD_PAL[11].tiffOffset := o_YRES;
               tiffIFD_PAL[13].tiffOffset := o_SOFTWARE;
             end;
    csRGB:  begin
              for i:= 0 to fBitmap.Height-1 do
              begin
                SrcRow:=FBitmap.ScanLine[i];
                for j:=0 to Width -1 do
                begin
                  case fTIFFParams.tiffColorSpace of
                  csRGB:begin
                          RGB:=SrcRow[j];
                          BGR.bgrBlue:=RGB.rgbtRed;
                          BGR.bgrGreen:=RGB.rgbtGreen;
                          BGR.bgrRed:=RGB.rgbtBlue;
                          picStream.Write(BGR,SizeOf(BGR));
                        end;
                  end;
                end;
              end;
              picStream.Position:=0;
              Stream.CopyFrom(picStream,picStream.Size);
              tiffIFD_RGB[1].tiffOffset := Cardinal(FBitmap.Width);
              tiffIFD_RGB[2].tiffOffset := Cardinal(FBitmap.Height);
              tiffIFD_RGB[8].tiffOffset := Cardinal(FBitmap.Height);
              tiffIFD_RGB[9].tiffOffset := Cardinal(3*FBitmap.Width*FBitmap.Height);
              tiffIFD_RGB[ 3].tiffOffset := o_BPS;
              tiffIFD_RGB[ 6].tiffOffset := o_STRIP;
              tiffIFD_RGB[10].tiffOffset := o_XRES;
              tiffIFD_RGB[11].tiffOffset := o_YRES;
              tiffIFD_RGB[14].tiffOffset := o_SOFTWARE;
            end;
    end;
    finally
      picStream.Free;
    end;
    case ftiffParams.tiffColorSpace of
    csRGBA:     tiffIFD_RGB[15].tiffOffset := EXTRASAMPLE_ASSOCALPHA;
    else        tiffIFD_RGB[15].tiffOffset := EXTRASAMPLE_UNSPECIFIED;
    end;



    o_IFD := Stream.Position ;
    Stream.Write ( NoOfDirs, sizeof(NoOfDirs));

    case fTIFFParams.tiffColorSpace of
      csRGB,csCMYK,csCIELAB,csYCBCR: Stream.Write ( tiffIFD_RGB, sizeof(tiffIFD_RGB));
      cs1Bit,cs4Bit,cs8Bit:          Stream.Write ( tiffIFD_PAL, sizeof(tiffIFD_PAL));
    end;

    Stream.Write ( NullString, sizeof(NullString));
    Stream.Seek ( 4, soFromBeginning ) ;
    Stream.Write ( o_IFD, sizeof(o_IFD));


end;


// -----------------------------------------------------------------------------
//
//			AssignBitmap
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.AssignBitmap(Bitmap:TBitmap);
begin
 FBitmap.Assign(Bitmap);
 with fTIFFHeader do
 begin
   tiffBOrder:=$4949;
   tiffVersion:=TIFF_VERSION;
   tiffIFD:=$00000000;
 end;

 with fTIFFParams do
 begin
   SetLength(tiffBitsPerSample,1);
   case FBitmap.PixelFormat of
   pf1bit:begin
           tiffPhotoMetric:=PHOTOMETRIC_MINISBLACK;
           tiffSamplesPerPixel:=1;
           tiffBitsPerSample[0]:=1;
           tiffColorSpace:=cs1Bit;
          end;
   pf4bit:begin
           tiffPhotoMetric:=PHOTOMETRIC_PALETTE;
           tiffSamplesPerPixel:=1;
           tiffBitsPerSample[0]:=4;
           tiffColorSpace:=cs4Bit;
          end;
   pf8bit:begin
           tiffPhotoMetric:=PHOTOMETRIC_PALETTE;
           tiffSamplesPerPixel:=1;
           tiffBitsPerSample[0]:=8;
           tiffColorSpace:=cs8Bit;
          end;
   pf24bit,pf15Bit,pf16Bit:begin
           tiffPhotoMetric:=PHOTOMETRIC_RGB;
           tiffSamplesPerPixel:=3;
           tiffBitsPerSample[0]:=8;
           tiffColorSpace:=csRGB;
          end;
   pf32bit:begin
           tiffPhotoMetric:=PHOTOMETRIC_RGB;
           tiffSamplesPerPixel:=3;
           tiffBitsPerSample[0]:=8;
           tiffColorSpace:=csRGB;
           end;
   end;
   tiffCompressionType:=COMPRESSION_NONE;
 end;
end;

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
constructor TTIFFFile.Create;
begin
  FBitmap:=TBitmap.Create;
end;

// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
destructor TTIFFFile.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
//
//			ReadTIFFHeader
//
// -----------------------------------------------------------------------------
procedure TTiffFile.ReadTIFFHeader(Stream:TStream);
var
    pixels, eBuff: Pointer;
    depredict_scheme         :Integer;
    compression_scheme         :Integer;
    line,row_size,ssize,j,i,k: Integer;
    fWidth,fHeight:Word;
    dBuff,xBuff:PChar;
    DestRow :pRGBArray;
    RGB:TRGBTriple;
    BGR:TBGRTriple;
    BGRA:TBGRAQuad;
    RGBA:TRGBAQuad;
    CMYK:TCMYKQuad;
    CIELAB:TCIELABTriple;
begin
  fid_c:=0;
  line:=0;
  with fTIFFParams do
  begin

  tiffColorMap:=0;
  Stream.ReadBuffer(ftiffHeader,sizeof(fTiffHeader));
  if FTIFFHeader.tiffBOrder =TIFF_BIGENDIAN then fBIG_ENDIAN:=True else fBIG_ENDIAN:=False;

  if fBIG_ENDIAN then
  begin
       FTIFFHeader.tiffVersion:=Swap(FTIFFHeader.tiffVersion);
       FTIFFHeader.tiffIFD:=SwapL(FTIFFHeader.tiffIFD);
  end;
  if FTIFFHeader.tiffVersion <> TIFF_VERSION then
  begin
    if GFXRaiseErrors then raise  EGraphicFormat.Create('TIFF: Unsupported version');
    GFXFileErrorList.Add('TIFF: TIFF: Unsupported version');
    exit;
  end;
  Stream.Position:=FStreamBase+FTIFFHeader.tiffIFD;
  Stream.ReadBuffer(fid_c,sizeof(fid_c));
  if fBIG_ENDIAN then fid_c:=swap(fid_c);
  SetLength(tiffIFD,fid_c);
  Stream.ReadBuffer(tiffIFD[0], fid_c* SizeOf(TImgFDEntry));
  if fBIG_ENDIAN then IFD_to_BIGENDIAN;

    //get the tiff-parameters
    tiffPrediction:=GetParams(TIFF_PREDICTOR);
    tiffPhotoMetric:=GetParams(TIFF_PHOTOMETRIC);
    tiffSamplesPerPixel:=GetParams(TIFF_SAMPLESPERPIXEL);
    if tiffSamplesPerPixel=0 then tiffSamplesPerPixel:=1;
    tiffExtraSamples:=GetParams(TIFF_EXTRASAMPLES);
    tiffCompressionType:=GetParams(TIFF_COMPRESSION);
    tiffStripOffsets:=GetParams(TIFF_STRIPOFFSETS);
    tiffStrip_Count:=GetParamsLen(TIFF_STRIPOFFSETS);
    tiffStripByteCount:=GetParams(TIFF_STRIPBYTECOUNTS);
    GetParamList(TIFF_BITSPERSAMPLE,Stream,tiffBitsPerSample);
    if Length(tiffBitsPerSample)=0 then begin SetLength(tiffBitsPerSample,1);tiffBitsPerSample[0]:=1;end;
    GetParamList(TIFF_ROWSPERSTRIP,Stream,tiffRowsPerStrip);
    fWidth := GetParams(TIFF_IMAGEWIDTH);
    fHeight := GetParams(TIFF_IMAGELENGTH);
    tiffColorMap:=GetParams(TIFF_COLORMAP);


    if (fHeight= 0) or (fWidth=0) then
    begin
      if GFXRaiseErrors then raise  EGraphicFormat.Create('TIFF: Illegal image dimensions');
         GFXFileErrorList.Add('TIFF: TIFF: Illegal image dimensions');
         exit;
    end;

    SetLength(tiffOffsets,tiffStrip_Count);
    SetLength(tiffByteCount,tiffStrip_Count);
    tiffBitsPerPlane:=tiffBitsPerSample[0]*tiffSamplesPerPixel;
    row_size:=(tiffBitsPerPlane* fWidth +7 ) div 8;

    if tiffStrip_Count>1 then
    begin
      Stream.Position:=FStreamBase+tiffStripOffsets;
      Stream.ReadBuffer(tiffOffsets[0], tiffStrip_Count*4 );
      if fBIG_ENDIAN then for i:=0 to (tiffStrip_Count *4)-1 do SwapL(tiffOffsets[i]);
      Stream.Position:=FStreamBase+tiffStripByteCount;
      Stream.ReadBuffer(tiffByteCount[0],tiffStrip_Count*4);
      if fBIG_ENDIAN then for i:=0 to (tiffByteCount[0] *4)-1 do SwapL(tiffOffsets[i]);
    end else begin
       if tiffStrip_Count = 1 then
       begin
         SetLength(tiffRowsPerStrip,1);
         tiffRowsPerStrip[0]:=fHeight;
       end;
       tiffOffsets[0]:=tiffStripOffsets;
       tiffByteCount[0]:=tiffStripByteCount;
    end;
    tiffColorSpace:=csNone;

    case tiffSamplesPerPixel of
      1:begin
          case tiffBitsPerSample[0] of
          1: begin
               FBitmap.PixelFormat:=pf1Bit;
               tiffColorSpace:=cs1Bit;
             end;
          4: begin
               FBitmap.PixelFormat:=pf4Bit;
               tiffColorSpace:=cs4Bit;
             end;
          8,16: begin
               FBitmap.PixelFormat:=pf8Bit;
               tiffColorSpace:=cs8Bit;
               end;
          else
             begin
                 if GFXRaiseErrors then raise  EGraphicFormat.Create('TIFF: Unsupported Bits Per Sample');
                    GFXFileErrorList.Add('TIFF: TIFF: Unsupported Bits Per Sample');
                 exit;
             end;

          end;
          //image has a palette...
          ReadTIFFPalette(Stream, tiffColorMap, tiffBitsPerPlane, tiffPhotoMetric);
        end;
      3:begin
          case tiffPhotoMetric of
          PHOTOMETRIC_RGB: begin
                             if tiffExtraSamples>0 then
                             begin
                               FBitmap.PixelFormat:=pf32Bit;
                               tiffColorSpace:=csRGBA;
                             end else
                             begin
                               FBitmap.PixelFormat:=pf24Bit;
                               tiffColorSpace:=csRGB;
                             end;
                           end;
          PHOTOMETRIC_YCBCR: begin
                               FBitmap.PixelFormat:=pf24bit;
                               tiffColorSpace:=csYCBCR;
                             end;
          PHOTOMETRIC_CIELAB: begin
                               FBitmap.PixelFormat:=pf24bit;
                               tiffColorSpace:=csCIELAB;
                             end;
          else
             begin
                 if GFXRaiseErrors then raise  EGraphicFormat.Create('TIFF: Unsupported photometric interpretation');
                    GFXFileErrorList.Add('TIFF: Unsupported photometric interpretation');
                 exit;
             end;

          end;
         end;
      4:begin
          case tiffPhotoMetric of
          PHOTOMETRIC_RGB: begin
                             FBitmap.PixelFormat:=pf24Bit;
                             tiffColorSpace:=csRGB;
                           end;
          PHOTOMETRIC_SEPARATED: begin
                               FBitmap.PixelFormat:=pf24bit;
                               tiffColorSpace:=csCMYK;
                             end;
          else
             begin
                 if GFXRaiseErrors then raise  EGraphicFormat.Create('TIFF: Unsupported photometric interpretation');
                    GFXFileErrorList.Add('TIFF: Unsupported photometric interpretation');
                 exit;
             end;
          end;
        end;
      else FBitmap.PixelFormat:=pfDevice;
    end;{end case}
    FBitmap.Width:=fWidth;
    FBitmap.Height:=fHeight;

    // deprediction
    if tiffPrediction = PREDICTION_HORZ_DIFFERENCING then
    case tiffSamplesPerPixel of
      3:depredict_scheme:=3;
      4:depredict_scheme:=4;
      else depredict_scheme:=1;
    end else depredict_scheme:=0;

    // compression check
    if ( tiffCompressionType <> COMPRESSION_NONE ) and
//    (tiffCompressionType <> COMPRESSION_LZW ) and  // removed to avoid copyright problems
    (tiffCompressionType <> COMPRESSION_PACKBITS)
    then
        begin
          if GFXRaiseErrors then
          raise  EGraphicFormat.Create('TIFF: Unsupported compression scheme');
          GFXFileErrorList.Add('TIFF: Unsupported compression scheme');
          exit;
        end;

    line:=0;
    j:=0;
    for i:=0 to tiffStrip_Count -1 do
    begin
       Stream.Position:=FStreamBase+tiffOffsets[i];
       if i < Length(tiffRowsPerStrip) then
       ssize := row_size * tiffRowsPerStrip[i] else
       ssize := row_size * tiffRowsPerStrip[High(tiffRowsPerStrip)];
       GetMem(dBuff, ssize);
      if tiffCompressionType<> COMPRESSION_NONE then
      begin
        GetMem(eBuff, tiffByteCount[i]);
        Stream.Read(eBuff^, tiffByteCount[i]);
        case tiffCompressionType of
//          COMPRESSION_LZW: DecodeTIFFLZW(eBuff, dBuff, tiffByteCount[i], ssize); // removed to avoid copyright problems
          COMPRESSION_PACKBITS : DecodeTIFFPackBits(eBuff, dBuff, tiffByteCount[i], ssize);
        end;
        FreeMem(eBuff);
      end
      else
      begin
        Stream.Read(dBuff^, ssize);
      end;
      xBuff:=dBuff;

      while (line < fHeight) and  ((xBuff-dBuff) < Integer(ssize)) do
      begin
        if line > fHeight then break;
        DestRow:=FBitmap.ScanLine[line];
        Inc(Line);
        case depredict_scheme of
        1:Deprediction1(xBuff,fWidth-1);
        3:Deprediction3(xBuff,fWidth-1);
        4:Deprediction4(xBuff,fWidth-1);
        end;

        j:=0;
        case tiffColorSpace of
        csRGB,cscmyk, csCIELAB,csRGBA :
           while ((xBuff-dBuff) < ssize-1) and (j<fWidth) do
           begin
             case tiffColorSpace of
                csRGBA:
                   begin
                    BGRA.bgrBlue:=pBGRAQuad(xBuff).bgrBlue;
                    BGRA.bgrGreen:=pBGRAQuad(xBuff).bgrGreen;
                    BGRA.bgrRed:=pBGRAQuad(xBuff).bgrRed;
                    BGRA.bgrAlpha:=pBGRAQuad(xBuff).bgrAlpha;
                    if tiffExtraSamples>0 then
                    RGBAtoBGRA_16(RGBA,BGRA) else
                    RGBAtoBGRA(RGBA,BGRA);
                    RGB.rgbtRed := BGRA.bgrRed;
                    RGB.rgbtGreen := BGRA.bgrGreen;
                    RGB.rgbtBlue := BGRA.bgrBlue;
                    DestRow[j]:=RGB;
                    Inc(pBGRAQuad(xBuff));
                   end;
                csRGB:
                   begin
                    RGB.rgbtBlue:=pRGBTriple(xBuff).rgbtRed;
                    RGB.rgbtRed:=pRGBTriple(xBuff).rgbtBlue;
                    RGB.rgbtGreen:=pRGBTriple(xBuff).rgbtGreen;
                    DestRow[j]:=RGB;
                    Inc(pRGBTriple(xBuff));
                   end;
                csCMYK :
                   begin
                    CMYK.cmykCyan:=pCMYKQuad(xBuff).cmykCyan;
                    CMYK.cmykMagenta:=pCMYKQuad(xBuff).cmykMagenta;
                    CMYK.cmykYellow:=pCMYKQuad(xBuff).cmykYellow;
                    CMYK.cmykK:=pCMYKQuad(xBuff).cmykK;
                    CMYKtoRGB(CMYK,RGB);
                    DestRow[j].rgbtRed :=RGB.rgbtBlue;
                    DestRow[j].rgbtBlue :=RGB.rgbtRed;
                    DestRow[j].rgbtGreen :=RGB.rgbtGreen;
                    Inc(pCMYKQuad(xBuff));
                   end;
                csCIELAB :
                   begin
                    CIELAB.L:=pCIELABTriple(xBuff).L;
                    CIELAB.A:=pCIELABTriple(xBuff).A;
                    CIELAB.B:=pCIELABTriple(xBuff).B;
                    CIELABtoBGR(CIELAB,BGR);
                    DestRow[j].rgbtRed :=BGR.bgrRed ;
                    DestRow[j].rgbtBlue :=BGR.bgrBlue;
                    DestRow[j].rgbtGreen :=BGR.bgrGreen;
                    Inc(pCIELABTriple(xBuff));
                   end;
              end;{end case}
             Inc(j);
           end;{end while}
        cs8Bit, cs1Bit, cs4Bit: begin
                   Move(xBuff^,DestRow[0], row_size);
                   Inc(pChar(xBuff),row_size)
                end;
        end;{end case}
    end;
    FreeMem(dBuff);
   end;
   end;
end;

// -----------------------------------------------------------------------------
//
//			IFD_TO_BIGENDIAN
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.IFD_to_BIGENDIAN;
var i:Integer;
begin
     for i:=0 to High(tiffIFD) do
     begin
       tiffIFD[i].tiffTag:=Swap(tiffIFD[i].tiffTag);
       tiffIFD[i].tiffDataLength:=Swap(tiffIFD[i].tiffDataLength);
       tiffIFD[i].tiffDataType:=Swap(tiffIFD[i].tiffDataType);
       case tiffIFD[i].tiffDataType of
       TIFF_SHORT,TIFF_SSHORT:
         if tiffIFD[i].tiffDataLength >1 then tiffIFD[i].tiffOffset:=SwapL(tiffIFD[i].tiffOffset) else
            tiffIFD[i].tiffOffset:=Swap(tiffIFD[i].tiffOffset);
       TIFF_LONG,TIFF_SLONG:
            tiffIFD[i].tiffOffset:=SwapL(tiffIFD[i].tiffOffset);
       end;
     end;
end;

// -----------------------------------------------------------------------------
//
//			GetParamsLen
//
// -----------------------------------------------------------------------------
function TTIFFFile.GetParamsLen(Param:Cardinal):Cardinal;
var i,p:Integer;
begin
     p:=-1;
     Result:=1;
     for i:=0 to fid_c-1 do
     begin
       if tiffIFD[i].tiffTag=Param then
       begin
          p:=i;
          break;
       end;
     end;
     if p=-1 then exit;
     Result:=tiffIFD[p].tiffDataLength;
          Case tiffIFD[p].tiffDataType of
          TIFF_ASCII,TIFF_BYTE,TIFF_SBYTE: Result:=Byte(Result);
          TIFF_SHORT, TIFF_SSHORT:Result:=Word(Result);
          TIFF_LONG, TIFF_SLONG:
          end;
end;

// -----------------------------------------------------------------------------
//
//			GetParams
//
// -----------------------------------------------------------------------------
function TTIFFFile.GetParams(Param:Cardinal):Cardinal;
var i,p:Integer;
begin
     p:=-1;
     Result:=0;
     for i:=0 to fid_c-1 do
     begin
       if tiffIFD[i].tiffTag=Param then
       begin
          p:=i;
          break;
       end;
     end;
     if p=-1 then exit;
     Result:=tiffIFD[p].tiffOffset;
     if tiffIFD[p].tiffDataLength=1 then
     begin
       case tiffIFD[p].tiffDataType of
         TIFF_BYTE: Result:=Byte(Result);
         TIFF_SHORT, TIFF_SSHORT: Result:=Word(Result);
       end;
   end;
end;

// -----------------------------------------------------------------------------
//
//			GetParamList
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.GetParamList(Param:Cardinal;Stream:TSTream; var VList: TCardinalArray);
var i,p:Integer;
    v:Cardinal;
begin
     p:=-1;
     for i:=0 to fid_c-1 do
     begin
       if tiffIFD[i].tiffTag=Param then
       begin
          p:=i;
          break;
       end;
     end;
     if p=-1 then exit;
     VList:=nil;
     SetLength(VList, tiffIFD[p].tiffDataLength);
     if tiffIFD[p].tiffDataLength=1 then VList[0]:=tiffIFD[p].tiffOffset else
     begin
       Stream.Position:=FStreamBase+tiffIFD[p].tiffOffset;
        for i:=0 to tiffIFD[p].tiffDataLength-1 do
        begin
          Case tiffIFD[p].tiffDataType of
          TIFF_ASCII,TIFF_BYTE,TIFF_SBYTE:
            begin
              Stream.Read(v,1);
              v:=Byte(V);
            end;
          TIFF_SHORT, TIFF_SSHORT:
            begin
              Stream.Read(v,2);
              if fBIG_ENDIAN then v:=Swap(v) else v:=Word(v);
            end;
          TIFF_LONG, TIFF_SLONG:
            begin
             Stream.Read(v,4);
             if fBIG_ENDIAN then v:=SwapL(v);
            end;
          end;
          VList[i]:=v;
        end;
     end;
end;

// -----------------------------------------------------------------------------
//
//			ReadTIFFpalette
//
// -----------------------------------------------------------------------------
procedure TTIFFFile.ReadTIFFPalette(Stream:TStream; Colormap:Cardinal; BitsPerPixel:Byte; PMetric:Cardinal);
var fPal:TMaxLogPalette;
    col_count,i,j,v,o,count:Integer;

begin
   fillchar(fpalette,sizeof(fpalette),0);
   fillchar(fpal,sizeof(fpal),0);

   count:=0;
   case BitsperPixel of
    1: Count := 1;
    4: Count := 15;
    8,16: Count := 255;
   end;

   if (ColorMap >0 ) and (PMetric= PHOTOMETRIC_PALETTE) then
   begin
     Stream.Position:=FStreamBase+Colormap;
     Stream.ReadBuffer(fPalette[0,0],2*3*(count+1)); //SizeOf(fPalette)
   end;
   if fBIG_ENDIAN then for i:=0 to 2 do
   for j:=0 to count do
   SwapS(fPalette[i,j]);

   fPal.palVersion := $300;
   fPal.palNumEntries := 1 + Count;


   for i:=0 to Count do
   begin
     case BitsPerPixel of
     1: begin
          case PMetric of
            PHOTOMETRIC_MINISWHITE:  v:= 255 * (1-i);
            PHOTOMETRIC_MINISBLACK:  v:= 255 *i;
            else v:=i;
          end;
          if pMetric =  PHOTOMETRIC_PALETTE then
          begin
            fPal.palPalEntry[i].peRed := fPalette[0,i];
            fPal.palPalEntry[i].peGreen := fPalette[0,i+1] ;
            fPal.palPalEntry[i].peBlue := fPalette[0,i+2] ;
            fPal.palPalEntry[i].peFlags:=0;
          end else
          begin
            fPal.palPalEntry[i].peRed := v;
            fPal.palPalEntry[i].peGreen := v;
            fPal.palPalEntry[i].peBlue := v;
            fPal.palPalEntry[i].peFlags:=0;
          end;
        end;
     4: begin
          case PMetric of
            PHOTOMETRIC_MINISWHITE:  o:= count-i;
            PHOTOMETRIC_MINISBLACK:  o:= i;
            else o:=i;
          end;
          case PMetric of
            PHOTOMETRIC_MINISWHITE,PHOTOMETRIC_MINISBLACK:
            begin
              v:=16 * i;
              fPal.palPalEntry[o].peRed := v;
              fPal.palPalEntry[o].peGreen := v;
              fPal.palPalEntry[o].peBlue := v;
              fPal.palPalEntry[o].peFlags:=0;
            end;
            PHOTOMETRIC_PALETTE:
            begin
             fPal.palPalEntry[i].peRed := fPalette[0,i];
             fPal.palPalEntry[i].peGreen := fPalette[0,i+16] ;
             fPal.palPalEntry[i].peBlue := fPalette[0,i+32] ;
             fPal.palPalEntry[i].peFlags:=0;
            end;
          end;
        end;
     8:
       case PMetric of
       PHOTOMETRIC_PALETTE:
         begin
           fPal.palPalEntry[i].peRed := fPalette[0,i] shr 8;
           fPal.palPalEntry[i].peGreen := fPalette[1,i] shr 8;
           fPal.palPalEntry[i].peBlue := fPalette[2,i] shr 8;
           fPal.palPalEntry[i].peFlags:=0;
         end;
       PHOTOMETRIC_MINISWHITE,PHOTOMETRIC_MINISBLACK:
          begin
            case PMetric of
              PHOTOMETRIC_MINISWHITE:  o:= count-i;
              PHOTOMETRIC_MINISBLACK:  o:= i;
              else o:= i;
           end;
           fPal.palPalEntry[o].peRed := i;
           fPal.palPalEntry[o].peGreen := i;
           fPal.palPalEntry[o].peBlue := i;
           fPal.palPalEntry[o].peFlags:=0;
          end;
     end;
   end;
  end;

  FBitmap.Palette:=CreatePalette(pLogPalette(@fPal)^);
  FBitMap.IgnorePalette:=False;
end;


initialization
  TPicture.RegisterFileFormat('TIF','TIFF-Format', TTIFFBitmap);

finalization
   TPicture.UnRegisterGraphicClass(TTIFFBitmap);


end.
