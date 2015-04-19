{***********************************************************************
Unit gfx_colors.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de

    gfx_colors is part of the gfx_library collection

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

    Histogram and descriptive statistic algorythms are based on or taken from the sources written by
    Earl F. Glynn, efg's computer lab
********************************************************************************}

unit gfx_colors;

interface
uses
    Wintypes,
    Messages,
    ExtCtrls,
    Windows,
    Graphics,
    gfx_basedef,
    StdCtrls,
    SysUtils;
   {$R-}

// -----------------------------------------------------------------------------
//
//			Statistics
//
// -----------------------------------------------------------------------------

  const
    NAN = MaxDouble / 2.0;

  type
   TDescriptiveStatistics =
   class(TObject)
     protected
       FCount       :  INTEGER;
       FMaxValue    :  DOUBLE;
       FMinValue    :  DOUBLE;
       FSum         :  DOUBLE;
       FSumOfSquares:  DOUBLE;
     public
       property  Count   :  INTEGER   read FCount;
       property  MaxValue:  DOUBLE    read FMaxValue;
       property  MinValue:  DOUBLE    read FMinValue;
       property  Sum     :  DOUBLE    read FSum;
       constructor Create;
       procedure NextValue(const x:  DOUBLE);
       function MeanValue        :  DOUBLE;
       function StandardDeviation:  DOUBLE;
       procedure ResetValues;
   end;

// -----------------------------------------------------------------------------
//
//			ColorQuantizer
//
// -----------------------------------------------------------------------------

  type
    TOctreeNode = class;  // forward definition so TReducibleNodes can be declared

    TReducibleNodes = array[0..7] OF TOctreeNode;

    TOctreeNode     =
      class(TObject)
        IsLeaf      :  BOOLEAN;
        PixelCount  :  INTEGER;
        RedSum      :  INTEGER;
        GreenSum    :  INTEGER;
        BlueSum     :  INTEGER;
        Next        :  TOctreeNode;
        Child       :  TReducibleNodes;

        constructor Create (const Level         :  INTEGER;
                            const ColorBits     :  INTEGER;
                            var   LeafCount     :  INTEGER;
                            var   ReducibleNodes:  TReducibleNodes);
        destructor  Destroy;  override;

      end;

    TColorQuantizer =
      class(TOBject)
        private
          FTree          :  TOctreeNode;
          FLeafCount     :  INTEGER;
          FReducibleNodes:  TReducibleNodes;
          FMaxColors     :  INTEGER;
          FColorBits     :  INTEGER;

        PROTECTED
          procedure   AddColor(var   Node          :  TOctreeNode;
                               const r,g,b         :  BYTE;
                               const ColorBits     :  INTEGER;
                               const Level         :  INTEGER;
                               var   LeafCount     :  INTEGER;
                               var   ReducibleNodes:  TReducibleNodes);
          procedure   DeleteTree(var Node:  TOctreeNode);
          procedure   GetPaletteColors(const Node        :  TOctreeNode;
                                       var   RGBQuadarray:  TRGBQuadarray;
                                       var   Index       :  INTEGER);
          procedure   ReduceTree(const ColorBits:  INTEGER;
                                 var   LeafCount:  INTEGER;
                                 var   ReducibleNodes:  TReducibleNodes);

        public
          constructor Create(const MaxColors:  INTEGER; const ColorBits:  INTEGER);
          destructor  Destroy;  override;

          procedure   GetColorTable(var RGBQuadarray:  TRGBQuadarray);
          function    ProcessImage(const Handle:  THandle):  BOOLEAN;

          property    ColorCount:  INTEGER  READ FLeafCount;

      end;

// -----------------------------------------------------------------------------
//
//			ColorSpace
//
// -----------------------------------------------------------------------------

 type
    THistogram  = array[0..255] of INTEGER;
    TRGBStatistics =
    class(TObject)
      protected
        FStatsRed  :  TDescriptiveStatistics;
        FStatsGreen:  TDescriptiveStatistics;
        FStatsBlue :  TDescriptiveStatistics;

        function PixelCount:  INTEGER;

      public
        property    Count:  INTEGER  read PixelCount;

        constructor Create;
        destructor  Destroy;  override;
        procedure NextRGBTriple(const rgb:  TRGBTriple);
        procedure ResetValues;
      end;

  { ---------------------------------------------------------------- }

  // Bitmaps
  procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
  function GetBitmapDimensionsString(const Bitmap:  TBitmap):  string;
  function GetColorPlaneString(const ColorPlane:  TColorPlane):  string;
  function GetPixelformatString(const Pixelformat:  TPixelformat):  string;

  // Image Processing
  procedure GetImagePlane (const ColorPlane:  TColorPlane;
                           const ColorOutput:  BOOLEAN;
                           const OriginalBitmap:  TBitmap;
                           var   ProcessedBitmap:  TBitmap);

  // Bitmap Statistics
  procedure GetBitmapStatistics(const Bitmap:  TBitmap;
                                const ColorPlane:  TColorPlane;
                                var Statistics:  TDescriptiveStatistics);
  procedure GetHistogram(const Bitmap:  TBitmap;
                         const ColorPlane:  TColorPlane;
                         var   Histogram:  THistogram);
  procedure DrawHistogram(const ColorPlane:  TColorPlane;
                          const Histogram:  THistogram;
                          const aCanvas:  TCanvas;
                          const LineColor:TColor;                          //** Added
                          const TextColor:TColor);                         //** Added


//------------------------------------------------------------
// Palette
//------------------------------------------------------------

  const
    clMoneyGreen = TColor($C0DCC0);   // Color   "8"  RGB:  192 220 192
    clSkyBlue    = TColor($F0CAA6);   // Color   "9"  RGB:  166 202 240
    clCream      = TColor($F0FBFF);   // Color "246"  RGB:  255 251 240
    clMediumGray = TColor($A4A0A0);   // Color "247"  RGB:  160 160 164

    NonDitherColors:  array[0..19] OF TColor =
      (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clSilver,
       clMoneyGreen, clSkyblue, clCream, clMediumGray,
       clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

  const PaletteDataSize256 = SizeOf(TLogPalette)+(255*SizeOf(TPaletteEntry));
  type

    TGetBitmapCallback = procedure (const Counter:  INTEGER;
                                    var OK:  BOOLEAN; var Bitmap:  TBitmap);

  const
    PaletteVersion = $0300;

  function  GetColorDepth:  INTEGER;
  function  GetPixelDepth:  INTEGER;

  function  GetDesktopPalette:  hPalette;
  function  GetGradientPalette(const red, green, blue:  BOOLEAN):  hPalette;
  function  CreateOptimizedPaletteforSingleBitmap(const Bitmap:  TBitmap;
                                                  const ColorBits:  INTEGER): hPalette;
  function  CreateOptimizedPaletteforManyBitmaps(const CallBack:  TGetBitmapCallback;
                                                 const ColorBits:  INTEGER): hPalette;

  function  IsPaletteDevice:  BOOLEAN;

  procedure LogicalPaletteToRGBQuad(const Start         :  WORD;
                                    const Count         :  WORD;
                                    var   LogicalPalette:  TPaletteEntries;
                                    var   RGBQuad       :  TRGBQuadarray);

  function  ReadAndCreatePaletteFromFractIntFile(const Filename:  string):  hPalette;

  procedure WMQueryNewPalette(formcanvashandle:THandle; PaletteHandle:  hPalette;
                              var Msg:  TWMQueryNewPalette);

  procedure WMPaletteChanged(formcanvashandle:THandle;formhandle:THandle; PaletteHandle:  hPalette;
                            var Msg:  TWMPaletteChanged);

  function ReadPaletteFromFile(FileName:String):HPalette;
  procedure ApplyPaletteToGraphic(Palette:Hpalette;Graphic:TGraphic;ChangePixelformat:Boolean;NewPixelformat:TPixelformat);
  procedure ApplyPaletteToDC(DC:HDC;Palette:HPalette);
  function LoadOptimizedPalette:HPalette;
  procedure PaletteDefaults(PaletteEntries:TPaletteEntries);
  procedure MakeGreyPalette(MaxLogPalette:TMaxLogPalette);
  procedure MakeStandardPalette(MaxLogPalette:TMaxLogPalette);
  var
    PaletteColorCount:  INTEGER;

const      stdPalette:array[0..15,1..3] Of Byte = (
{Black}                                       (0  ,0  ,0 ),
{Blue}                                        (0  ,0  ,32),
{Green}                                       (0  ,32 ,0 ),
{Cyan}                                        (0  ,32 ,32),
{Red}                                         (32 ,0  ,0 ),
{Magenta}                                     (32 ,0  ,32),
{Brown}                                       (32 ,32 ,0 ),
{Light Gray}                                  (42 ,42 ,42),
{Dark Gray}                                   (21 ,21 ,21),
{Light Blue}                                  (0  ,0  ,63),
{Light Green}                                 (0  ,63 ,0 ),
{Light Cyan}                                  (0  ,63 ,63),
{Light Red}                                   (63 ,0  ,0 ),
{Light Magenta}                               (63 ,0  ,63),
{Yellow}                                      (63 ,63 ,0 ),
{Bright White}                                (63 ,63 ,63));

  const optPal:array[0..255, 0..2] of byte =
    ((0,0,0      ),     (128,0,0    ),     (0,128,0    ),     (128,128,0  ),     (0,0,128    ),
     (128,0,128  ),     (0,128,128  ),     (192,192,192),     (52,66,82   ),     (79,101,125 ),
     (6,7,6      ),     (7,21,8     ),     (12,18,17   ),     (24,11,10   ),     (21,14,18   ),
     (17,23,13   ),     (21,18,16   ),     (5,19,46    ),     (20,38,17   ),     (21,41,53   ),
     (41,10,9    ),     (39,23,12   ),     (40,20,21   ),     (55,11,12   ),     (56,22,11   ),
     (59,27,22   ),     (49,43,19   ),     (46,48,44   ),     (4,26,80    ),     (8,41,85    ),
     (18,53,112  ),     (40,52,75   ),     (39,60,98   ),     (51,69,13   ),     (55,72,38   ),
     (58,100,49  ),     (23,72,81   ),     (20,74,101  ),     (56,75,80   ),     (41,73,114  ),
     (59,108,75  ),     (51,100,122 ),     (70,10,11   ),     (72,24,10   ),     (71,26,29   ),
     (86,7,14    ),     (87,11,16   ),     (85,26,13   ),     (87,26,20   ),     (81,45,20   ),
     (75,42,37   ),     (71,44,57   ),     (72,55,36   ),     (74,58,54   ),     (88,44,37   ),
     (88,56,38   ),     (88,60,56   ),     (110,13,12  ),     (119,26,39  ),     (102,43,15  ),
     (102,41,26  ),     (100,55,15  ),     (104,53,24  ),     (121,39,17  ),     (126,63,13  ),
     (105,44,37  ),     (107,40,53  ),     (106,57,39  ),     (104,58,52  ),     (118,43,36  ),
     (120,42,58  ),     (121,52,39  ),     (118,58,56  ),     (72,55,65   ),     (76,78,23   ),
     (83,72,44   ),     (79,104,44  ),     (105,69,19  ),     (108,70,38  ),     (102,69,50  ),
     (103,88,39  ),     (109,82,52  ),     (119,70,43  ),     (121,74,55  ),     (123,85,43  ),
     (121,83,51  ),     (115,96,27  ),     (112,104,46 ),     (84,76,71   ),     (77,87,114  ),
     (85,111,77  ),     (82,108,116 ),     (108,84,69  ),     (109,89,111 ),     (105,115,86 ),
     (106,115,118),     (38,59,128  ),     (31,73,148  ),     (48,84,144  ),     (55,110,144 ),
     (78,104,143 ),     (83,112,164 ),     (108,119,143),     (99,121,173 ),     (112,137,37 ),
     (90,132,74  ),     (83,137,100 ),     (103,135,89 ),     (112,141,112),     (111,163,111),
     (86,132,137 ),     (80,134,171 ),     (119,139,139),     (109,140,176),     (122,162,134),
     (127,164,187),     (126,140,194),     (144,22,14  ),     (142,28,41  ),     (146,41,24  ),
     (133,44,45  ),     (136,37,62  ),     (130,59,43  ),     (136,57,52  ),     (157,44,57  ),
     (152,55,32  ),     (152,60,60  ),     (168,28,19  ),     (169,40,28  ),     (176,52,41  ),
     (142,56,69  ),     (171,57,71  ),     (156,77,4   ),     (139,79,54  ),     (143,110,21 ),
     (141,102,52 ),     (163,85,14  ),     (171,82,54  ),     (171,113,13 ),     (169,111,47 ),
     (137,77,77  ),     (135,74,85  ),     (138,89,72  ),     (130,95,92  ),     (152,75,69  ),
     (150,88,68  ),     (144,103,73 ),     (139,115,109),     (170,87,75  ),     (174,109,78 ),
     (177,112,105),     (198,26,24  ),     (204,62,4   ),     (204,41,37  ),     (245,56,57  ),
     (207,75,61  ),     (205,122,12 ),     (211,124,47 ),     (224,94,24  ),     (203,82,85  ),
     (206,87,102 ),     (199,115,86 ),     (208,115,103),     (246,80,77  ),     (236,119,105),
     (147,118,142),     (143,122,167),     (168,123,152),     (231,118,130),     (152,131,49 ),
     (181,134,25 ),     (182,144,35 ),     (151,133,84 ),     (149,140,115),     (143,169,71 ),
     (149,166,112),     (174,137,87 ),     (173,143,115),     (179,170,111),     (208,144,18 ),
     (202,135,51 ),     (215,168,27 ),     (218,183,44 ),     (237,153,10 ),     (229,145,43 ),
     (248,180,9  ),     (203,134,89 ),     (209,138,105),     (208,168,83 ),     (214,180,109),
     (228,146,84 ),     (231,146,117),     (239,173,75 ),     (240,170,124),     (215,217,17 ),
     (222,199,52 ),     (251,205,6  ),     (246,203,46 ),     (234,230,10 ),     (251,228,8  ),
     (251,230,48 ),     (251,246,43 ),     (204,198,92 ),     (214,197,107),     (241,205,78 ),
     (240,204,111),     (251,231,80 ),     (250,238,110),     (143,147,144),     (148,164,156),
     (149,174,178),     (177,146,142),     (170,140,165),     (177,168,148),     (178,177,174),
     (133,150,202),     (145,176,210),     (169,186,206),     (168,183,226),     (181,199,145),
     (172,197,176),     (180,201,205),     (177,206,236),     (208,142,138),     (194,152,186),
     (209,171,144),     (210,177,173),     (236,152,141),     (239,171,139),     (241,180,175),
     (204,179,202),     (224,182,212),     (210,196,150),     (213,201,176),     (248,203,143),
     (247,203,170),     (248,232,143),     (250,247,176),     (210,203,205),     (207,213,231),
     (221,230,200),     (202,229,240),     (243,208,201),     (228,215,232),     (244,238,209),
     (247,246,238),     (162,177,196),     (160,160,164),     (128,128,128),     (255,0,0    ),
     (0,255,0    ),     (255,255,0  ),     (0,0,255    ),     (255,0,255  ),     (0,255,255  ),
     (0,0,102    ));

//------------------------------------------------------------
// Splitting
//------------------------------------------------------------

 procedure GetRedPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetGreenPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetBluePlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetHuePlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetSatPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetValPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
 procedure GetLigPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);

implementation

//------------------------------------------------------------
// Statistics
//------------------------------------------------------------


// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
  constructor TDescriptiveStatistics.Create;
  begin
    ResetValues
  end {Create};


// -----------------------------------------------------------------------------
//
//			NextValue
//
// -----------------------------------------------------------------------------
  procedure TDescriptiveStatistics.NextValue (const x:  DOUBLE);
  begin
    INC(FCount);
    FSum := FSum + x;
    FSumOfSquares := FSumOfSquares + x*x;

    if   x > FMaxValue
    then FmaxValue := x;

    if  x < FMinValue
    then FMinValue := x
  end {NextNumber};


// -----------------------------------------------------------------------------
//
//			MeanValue
//
// -----------------------------------------------------------------------------
  function TDescriptiveStatistics.MeanValue:  DOUBLE;
  begin
    if   FCount = 0
    then RESULT := NAN
    else RESULT := FSum / FCount
  end {MeanValue};


// -----------------------------------------------------------------------------
//
//			StandardDeviation
//
// -----------------------------------------------------------------------------
  function TDescriptiveStatistics.StandardDeviation:  DOUBLE;
  begin
    if   FCount < 2
    then RESULT := NAN
    else RESULT := SQRT( (FSumOfSquares - (FSum*FSum / FCount)) /
                         (FCount - 1) )
  end {StandardDeviation};


// -----------------------------------------------------------------------------
//
//			ResetValue
//
// -----------------------------------------------------------------------------
  procedure TDescriptiveStatistics.ResetValues;
  begin
    FCount := 0;

    FMinValue :=  MaxDouble;
    FMaxValue := -MaxDouble;

    FSum          := 0.0;
    FSumOfSquares := 0.0
  end {ResetValues};

//------------------------------------------------------------
// Color Quantization
//------------------------------------------------------------

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
  constructor TOctreeNode.Create (const Level         :  INTEGER;
                                  const ColorBits     :  INTEGER;
                                  var   LeafCount     :  INTEGER;
                                  var   ReducibleNodes:  TReducibleNodes);
    var
      i:  INTEGER;
  begin
    PixelCount  := 0;
    RedSum      := 0;
    GreenSum    := 0;
    BlueSum     := 0;
    for i := Low(Child) to High(Child) do
      Child[i] := nil;

    IsLeaf := (Level = ColorBits);
    if   IsLeaf
    then begin
      Next := nil;
      INC(LeafCount);
    end
    else begin
      Next := ReducibleNodes[Level];
      ReducibleNodes[Level] := SELF
    end
  end {Create};


// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
  destructor TOctreeNode.Destroy;
    var
      i:  INTEGER;
  begin
      for i := Low(Child) to High(Child) do
        Child[i].Free
  end {Destroy};



// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
  constructor TColorQuantizer.Create(const MaxColors:  INTEGER; const ColorBits:  INTEGER);
    var
      i:  INTEGER;
  begin
    ASSERT (ColorBits <= 8);

    FTree := nil;
    FLeafCount := 0;

    // Initialize all nodes even though only ColorBits+1 of them are needed
    for i := Low(FReducibleNodes) to High(FReducibleNodes) do
      FReducibleNodes[i] := nil;

    FMaxColors := MaxColors;
    FColorBits := ColorBits
  end {Create};


// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
  destructor  TColorQuantizer.Destroy;
  begin
    if   FTree <> nil
    then DeleteTree(FTree)
  end {Destroy};


// -----------------------------------------------------------------------------
//
//			GetColorTable
//
// -----------------------------------------------------------------------------
procedure TColorQuantizer.GetColorTable(var RGBQuadarray:  TRGBQuadarray);
var
      Index:  INTEGER;
begin
    Index := 0;
    GetPaletteColors(FTree, RGBQuadarray, Index)
end {GetColorTable};


// -----------------------------------------------------------------------------
//
//			ProcessImage
//
// -----------------------------------------------------------------------------
  // Handles passed to ProcessImage should refer to DIB sections, not DDBs.
  // In certain cases, specifically when it's called upon to process 1, 4, or
  // 8-bit per pixel images on systems with palettized display adapters,
  // ProcessImage can produce incorrect results if it's passed a handle to a
  // DDB.
function TColorQuantizer.ProcessImage(const Handle:  THandle):  BOOLEAN;
const
      MaxPixelCount = 1048576;  // 2^20 shouldn't be much of a limit here

var
      Bytes     :  INTEGER;
      DIBSection:  TDIBSection;

    // Process 1, 4, or 8-bit DIB:
    // The strategy here is to use GetDIBits to convert the image into
    // a 24-bit DIB one scan line at a time.  A pleasant side effect
    // of using GetDIBits in this manner is that RLE-encoded 4-bit and
    // 8-bit DIBs will be uncompressed.

    // Implemented as in article, but doesn't work (yet) as I would expect.
    procedure ProcessLowBitDIB;
      var
        BitmapInfo   :  TBitmapInfo;
        DeviceContext:  hDC;
        i            :  INTEGER;
        j            :  INTEGER;
        ScanLine     :  pRGBarray;
    begin
      GetMem(ScanLine, 3*DIBSection.dsBmih.biWidth);
      try
        ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
        with BitmapInfo do
        begin
          bmiHeader.biSize        := SizeOf(TBitmapInfo);
          bmiHeader.biWidth       := DIBSection.dsBmih.biWidth;
          bmiHeader.biHeight      := DIBSection.dsBmih.biHeight;
          bmiHeader.biPlanes      := 1;
          bmiHeader.biBitCount    := 24;
          bmiHeader.biCompression := BI_RGB;

        end;

        DeviceContext := GetDC(0);
        try
          for j := 0 to DIBSection.dsBmih.biHeight-1 do
          begin
            GetDIBits (DeviceContext, Handle, j, 1, ScanLine, BitmapInfo, DIB_RGB_COLORS);
            for i := 0 to DIBSection.dsBmih.biWidth-1 do
            begin
              with Scanline[i] do
                AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                         FColorBits, 0, FLeafCount, FReducibleNodes);

              while FLeafCount > FMaxColors do
                ReduceTree(FColorbits, FLeafCount, FReducibleNodes)

            end
          end

        finally
          ReleaseDC(0, DeviceContext);
        end

      finally
        FreeMem(ScanLine)
      end
    end {ProcessLowBitDIB};


    procedure Process16BitDIB;
    begin
      // Not yet implemented
    end {Process16BitDIB};


    procedure Process24BitDIB;
      var
        i       :  INTEGER;
        j       :  INTEGER;
        ScanLine:  pRGBarray;
    begin
      Scanline := pRGBarray(DIBSection.dsBm.bmBits);
      for j := 0 to DIBSection.dsBmih.biHeight-1 do
      begin

        for i := 0 to DIBSection.dsBmih.biWidth-1 do
        begin
          with Scanline[i] DO
            AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                     FColorBits, 0, FLeafCount, FReducibleNodes);

          WHILE FLeafCount > FMaxColors DO
            ReduceTree(FColorbits, FLeafCount, FReducibleNodes)

        end;

        ScanLine := pRGBarray(INTEGER(Scanline) + DIBSection.dsBm.bmWidthBytes);
      end
    end {Process24BitDIB};


    procedure Process32BitDIB;
    begin
      // Not yet implemented
    end {Process32BitDIB};

begin
    RESULT := FALSE;

    Bytes := GetObject(Handle, SizeOF(DIBSection), @DIBSection);

    IF   Bytes > 0   // Invalid Bitmap if Bytes = 0
    THEN begin
//    PadBytes := DIBSECTION.dsBm.bmWidthBytes -
//                (((DIBSection.dsBmih.biWidth * DIBSection.dsBmih.biBitCount) + 7) DIV 8);
      ASSERT (DIBSection.dsBmih.biHeight < MaxPixelCount);
      ASSERT (DIBSection.dsBmih.biWidth  < MaxPixelCount);

      CASE  DIBSection.dsBmih.biBitCount OF
         1:  ProcessLowBitDIB;
         4:  ProcessLowBitDIB;
         8:  ProcessLowBitDIB;
        16:  Process16bitDIB;
        24:  Process24bitDIB;
        32:  Process32bitDIB
        else
          // Do nothing.  Default RESULT is already FALSE
      end

    end
end ;


// -----------------------------------------------------------------------------
//
//			AddColor
//
// -----------------------------------------------------------------------------
procedure TColorQuantizer.AddColor(var   Node          :  TOctreeNode;
                                     const r,g,b         :  BYTE;
                                     const ColorBits     :  INTEGER;
                                     const Level         :  INTEGER;
                                     var   LeafCount     :  INTEGER;
                                     var   ReducibleNodes:  TReducibleNodes);
const
      Mask:  array[0..7] OF BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);

var
      Index    :  INTEGER;
      Shift    :  INTEGER;
begin
    // If the node doesn't exist, create it.
    IF   Node = NIL
    THEN Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

    IF   Node.IsLeaf
    THEN begin
      INC(Node.PixelCount);
      INC(Node.RedSum,   r);
      INC(Node.GreenSum, g);
      INC(Node.BlueSum,  b)
    end
    else begin
      // Recurse a level deeper if the node is not a leaf.
      Shift := 7 - Level;

      Index := (((r AND mask[Level]) SHR Shift) SHL 2)  OR
               (((g AND mask[Level]) SHR Shift) SHL 1)  OR
                ((b AND mask[Level]) SHR Shift);
      AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1,
               LeafCount, ReducibleNodes)
    end

end ;



// -----------------------------------------------------------------------------
//
//			DeleteTree
//
// -----------------------------------------------------------------------------
procedure TColorQuantizer.DeleteTree(var Node:  TOctreeNode);
var
      i        :  INTEGER;
begin
    for i := Low(TReducibleNodes) TO High(TReducibleNodes) DO
    begin
      IF   Node.Child[i] <> NIL
      THEN DeleteTree(Node.Child[i]);
    end;

    Node.Free;

    Node := NIL;
end;


// -----------------------------------------------------------------------------
//
//			GetPaletteColors
//
// -----------------------------------------------------------------------------
procedure TColorQuantizer.GetPaletteColors(const Node        :  TOctreeNode;
                                             var   RGBQuadarray:  TRGBQuadarray;
                                             var   Index       :  INTEGER);
var
      i:  INTEGER;
begin
    if Assigned(Node) then IF   Node.IsLeaf
    THEN begin
      with RGBQuadarray[Index] DO
      begin
        TRY
          rgbRed   := BYTE(Node.RedSum   DIV Node.PixelCount);
          rgbGreen := BYTE(Node.GreenSum DIV Node.PixelCount);
          rgbBlue  := BYTE(Node.BlueSum  DIV Node.PixelCount)
        EXCEPT
          rgbRed   := 0;
          rgbGreen := 0;
          rgbBlue  := 0
        end;

        rgbReserved := 0
      end;
      INC(Index)
    end
    else begin
      for i := Low(Node.Child) TO High(Node.Child) DO
      begin
        IF   Node.Child[i] <> NIL
        THEN GetPaletteColors(Node.Child[i], RGBQuadarray, Index)
      end
    end
end;


// -----------------------------------------------------------------------------
//
//			ReduceTree
//
// -----------------------------------------------------------------------------
procedure TColorQuantizer.ReduceTree(const ColorBits:  INTEGER;
                                       var   LeafCount:  INTEGER;
                                       var   ReducibleNodes:  TReducibleNodes);
var
      BlueSum :  INTEGER;
      Children:  INTEGER;
      GreenSum:  INTEGER;
      i       :  INTEGER;
      Node    :  TOctreeNode;
      RedSum  :  INTEGER;
begin
    // Find the deepest level containing at least one reducible node
    i := Colorbits - 1;
    WHILE (i > 0) AND (ReducibleNodes[i] = NIL) DO
      DEC(i);

    // Reduce the node most recently added to the list at level i.
    Node := ReducibleNodes[i];
    ReducibleNodes[i] := Node.Next;

    RedSum   := 0;
    GreenSum := 0;
    BlueSum  := 0;
    Children := 0;

    for i := Low(ReducibleNodes) TO High(ReducibleNodes) DO
    begin
      IF   Node.Child[i] <> NIL
      THEN begin
        INC(RedSum,          Node.Child[i].RedSum);
        INC(GreenSum,        Node.Child[i].GreenSum);
        INC(BlueSum,         Node.Child[i].BlueSum);
        INC(Node.PixelCount, Node.Child[i].PixelCount);
        Node.Child[i].Free;
        Node.Child[i] := NIL;
        INC(Children)
      end
    end;

    Node.IsLeaf   := TRUE;
    Node.RedSum   := RedSum;
    Node.GreenSum := GreenSum;
    Node.BlueSum  := BlueSum;
    DEC(LeafCount, Children-1)
end;

//------------------------------------------------------------
// Colorspace
//------------------------------------------------------------

// -----------------------------------------------------------------------------
//
//			Create
//
// -----------------------------------------------------------------------------
constructor TRGBStatistics.Create;
begin
    FStatsRed   := TDescriptiveStatistics.Create;
    FStatsGreen := TDescriptiveStatistics.Create;
    FStatsBlue  := TDescriptiveStatistics.Create;

    FStatsRed.ResetValues;
    FStatsGreen.ResetValues;
    FStatsBlue.ResetValues
end;


// -----------------------------------------------------------------------------
//
//			Destroy
//
// -----------------------------------------------------------------------------
destructor TRGBStatistics.Destroy;
begin
    FStatsRed.Free;
    FStatsGreen.Free;
    FStatsBlue.Free
end;


// -----------------------------------------------------------------------------
//
//			PixelCount
//
// -----------------------------------------------------------------------------
function TRGBStatistics.PixelCount;
begin
    RESULT := FStatsRed.Count
end;


// -----------------------------------------------------------------------------
//
//			NextRGBTriple
//
// -----------------------------------------------------------------------------
procedure TRGBStatistics.NextRGBTriple(const rgb:  TRGBTriple);
begin
    with rgb DO
    begin
      FStatsRed.NextValue(rgbtRed);
      FStatsGreen.NextValue(rgbtGreen);
      FStatsBlue.NextValue(rgbtBlue);
    end
end;

// -----------------------------------------------------------------------------
//
//			ResetValues
//
// -----------------------------------------------------------------------------
procedure TRGBStatistics.ResetValues;
begin
    FStatsRed.ResetValues;
    FStatsGreen.ResetValues;
    FStatsBlue.ResetValues
end;


{==  Bitmaps  =======================================================}

// -----------------------------------------------------------------------------
//
//			PrintBitmap
//
// -----------------------------------------------------------------------------
  {Based on posting to borland.public.delphi.winapi by Rodney E Geraghty, 8/8/97.
   Used to print bitmap on any Windows printer.}
procedure PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
var
      BitmapHeader:  pBitmapInfo;
      BitmapImage :  POINTER;
      HeaderSize  :  DWOrd;
      ImageSize   :  DWord;
begin
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage,  ImageSize);
    TRY
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     {Destination Origin}
                    DestRect.Right  - DestRect.Left, {Destination Width}
                    DestRect.Bottom - DestRect.Top,  {Destination Height}
                    0, 0,                            {Source Origin}
                    Bitmap.Width, Bitmap.Height,     {Source Width & Height}
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY)
    FINALLY
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    end
end;


// -----------------------------------------------------------------------------
//
//			GetBitmapDimensionsString
//
// -----------------------------------------------------------------------------
function GetBitmapDimensionsString(const Bitmap:  TBitmap):  STRING;
begin
    RESULT := IntToStr(Bitmap.Width)  + ' by ' +
              IntToStr(Bitmap.Height) + ' pixels by ' +
              GetPixelformatString(Bitmap.Pixelformat) + ' color';
end;

// -----------------------------------------------------------------------------
//
//			GetColorPlaneString
//
// -----------------------------------------------------------------------------
function GetColorPlaneString(const ColorPlane:  TColorPlane):  STRING;
begin
    CASE ColorPlane OF
      cpRGB:           RESULT := 'RGB Composite';

      cpRed:           RESULT := 'Red';
      cpGreen:         RESULT := 'Green';
      cpBlue:          RESULT := 'Blue';

      cpHueHSV:        RESULT := 'Hue (HSV)';
      cpSaturationHSV: RESULT := 'Saturation (HSV)';
      cpValue:         RESULT := 'Value (HSV)';

      cpCyan:          RESULT := 'Cyan';
      cpMagenta:       RESULT := 'Magenta';
      cpYellow:        RESULT := 'Yellow';
      cpBlack:         RESULT := 'Black (CMYK)';

      cpIntensity:     RESULT := 'RGB Intensity';
    end

end;


// -----------------------------------------------------------------------------
//
//			GetPixelFormatString
//
// -----------------------------------------------------------------------------
function GetPixelformatString(const Pixelformat:  TPixelformat):  STRING;
    var
      format:  STRING;
begin
    CASE Pixelformat OF
      pfDevice:  format := 'Device';
      pf1bit:    format := '1 bit';
      pf4bit:    format := '4 bit';
      pf8bit:    format := '8 bit';
      pf15bit:   format := '15 bit';
      pf16bit:   format := '16 bit';
      pf24bit:   format := '24 bit';
      pf32bit:   format := '32 bit'
      else
        format := 'Unknown';
    end;
    RESULT := format;
end;


 {==  Image Processing  ==============================================}

// -----------------------------------------------------------------------------
//
//			GetImagePlane
//
// -----------------------------------------------------------------------------
procedure GetImagePlane (const ColorPlane:  TColorPlane;
                           const ColorOutput:  BOOLEAN;
                           const OriginalBitmap:  TBitmap;
                           var   ProcessedBitmap:  TBitmap);
var
    HSV:THSVTriple;
    CMYK:TCMYKQuad;
    i           :  INTEGER;
    Intensity   :  INTEGER;
    j           :  INTEGER;
  //  L           :  INTEGER;
    RowOriginal :  pRGBarray;
    RowProcessed:  pRGBarray;
begin
    IF   OriginalBitmap.Pixelformat <> pf24bit
    THEN RAISE  EGraphicformat.Create('GetImageSpace:  ' +
               'Bitmap must be 24-bit color.');

    {Step through each row of image.}
    for j := OriginalBitmap.Height-1 downto 0 DO
    begin
      RowOriginal  := pRGBarray(OriginalBitmap.Scanline[j]);
      RowProcessed := pRGBarray(ProcessedBitmap.Scanline[j]);

      for i := OriginalBitmap.Width-1 downto 0 DO
      begin
        CASE ColorPlane OF
          cpRGB:    IF   ColorOutput
                    THEN AssignRGBTriple(RowProcessed[i], RowOriginal[i])
                    else begin
                      Intensity := RGBIntensity(RowOriginal[i]);
                      AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(Intensity, Intensity, Intensity))
                    end;

          cpRed:    IF   ColorOutput
                    THEN AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(RowOriginal[i].rgbtRed, 0, 0))
                    else begin
                      Intensity := RowOriginal[i].rgbtRed;
                      AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(Intensity, Intensity, Intensity))
                    end;

          cpGreen:  IF   ColorOutput
                    THEN AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(0, RowOriginal[i].rgbtGreen, 0))
                    else begin
                      Intensity := RowOriginal[i].rgbtGreen;
                      AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(Intensity, Intensity, Intensity))
                    end;

          cpBlue:   IF   ColorOutput
                    THEN AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(0, 0, RowOriginal[i].rgbtBlue))
                    else begin
                      Intensity := RowOriginal[i].rgbtBlue;
                      AssignRGBTriple(RowProcessed[i],
                                    RGBTriple(Intensity, Intensity, Intensity))
                    end;

          cpHueHSV:
            begin
              RGBtoHSV(RowOriginal[i], HSV);
              HSV.hsvHue := (255 * HSV.hsvHue) DIV 359;

              {Shades of Hue}
              AssignRGBTriple(RowProcessed[i], RGBTriple(HSV.hsvHue,HSV.hsvHue,HSV.hsvHue))
            end;

          cpSaturationHSV:
            begin
              RGBtoHSV(RowOriginal[i], HSV);

              {Shades of Saturation}
              AssignRGBTriple(RowProcessed[i], RGBTriple(HSV.hsvSaturation,HSV.hsvSaturation,HSV.hsvSaturation))
            end;

          cpValue:
            begin
              RGBtoHSV(RowOriginal[i], HSV);

              {Shades of Value}
              AssignRGBTriple(RowProcessed[i], RGBTriple(HSV.hsvValue,HSV.hsvValue,HSV.hsvValue))
            end;

          cpCyan:
            begin
              RGBtoCMYK(RowOriginal[i], CMYK);

              {Shades of Value}
              AssignRGBTriple(RowProcessed[i], RGBTriple(CMYK.cmykCyan,CMYK.cmykCyan,CMYK.cmykCyan))
            end;

          cpMagenta:
            begin
              RGBtoCMYK(RowOriginal[i], CMYK);

              {Shades of Value}
              AssignRGBTriple(RowProcessed[i], RGBTriple(CMYK.cmykMagenta,CMYK.cmykMagenta,CMYK.cmykMagenta))
            end;

          cpYellow:
            begin
              RGBtoCMYK(RowOriginal[i], CMYK);

              {Shades of Value}
              AssignRGBTriple(RowProcessed[i], RGBTriple(CMYK.cmykYellow,CMYK.cmykYellow,CMYK.cmykYellow))
            end;

          cpBlack:
            begin
              RGBtoCMYK(RowOriginal[i], CMYK);

              {Shades of Value}
              AssignRGBTriple(RowProcessed[i], RGBTriple(CMYK.cmykK,CMYK.cmykK,CMYK.cmykK))
            end;

          cpIntensity:
            begin
              Intensity := RGBIntensity(RowOriginal[i]);

              {Shades of Saturation}
              AssignRGBTriple(RowProcessed[i],
                              RGBTriple(Intensity, Intensity, Intensity))
            end

        end


      end

    end
end;


{==  Image Statistics  ==============================================}

// -----------------------------------------------------------------------------
//
//			GetBitmapStatistics
//
// -----------------------------------------------------------------------------
procedure GetBitmapStatistics(const Bitmap:  TBitmap;const ColorPlane:  TColorPlane;
          var Statistics:  TDescriptiveStatistics);
var
      CMYK:  TCMYKQuad;
      HSV  :  THSVTriple;      {color coordinates}
      i      :  INTEGER;
      j      :  INTEGER;
      Row    :  pRGBarray;
      Value  :  INTEGER;
begin
    IF   Bitmap.Pixelformat <> pf24bit
    THEN RAISE EGraphicformat.Create('GetBitmapStatistics:  ' +
               'Bitmap must be 24-bit color.');

    value := 0;  {avoid compiler warning about initialization}

    {Step through each row of image.}
    for j := Bitmap.Height-1 downto 0 DO
    begin
      Row  := pRGBarray(Bitmap.Scanline[j]);

      for i := Bitmap.Width-1 downto 0 DO
      begin
        CASE ColorPlane OF
          cpRGB,
          cpIntensity:   value := RGBIntensity(Row[i]);

          cpRed:        value := Row[i].rgbtRed;
          cpGreen:      value := Row[i].rgbtGreen;
          cpBlue:       value := Row[i].rgbtBlue;

          cpHueHSV:
            begin
              RGBtoHSV(Row[i], HSV);
              value := HSV.hsvHue
            end;

          cpSaturationHSV:
           begin
              RGBtoHSV(Row[i], HSV);
              value := HSV.hsvSaturation
            end;

          cpValue:
            begin
              RGBtoHSV(Row[i], HSV);
              value := HSV.hsvValue
            end;

          cpCyan:
            begin
              RGBtoCMYK(Row[i], CMYK);
              value := CMYK.cmykCyan
            end

        end;

        Statistics.NextValue(value)

      end

    end

end;

// -----------------------------------------------------------------------------
//
//			GetHistogram
//
// -----------------------------------------------------------------------------
procedure GetHistogram(const Bitmap    :  TBitmap;
                         const ColorPlane:  TColorPlane;
                         var Histogram   :  THistogram);
var
      CMYK:  TCMYKQuad;
      HSV  :  THSVTriple;      {color coordinates}
      i      :  INTEGER;
      index  :  INTEGER;
      j      :  INTEGER;
      Row    :  pRGBarray;
      Row8bit:  pBytearray;
begin
    IF   (Bitmap.Pixelformat <> pf24bit) AND
         (Bitmap.Pixelformat <> pf8bit)
    THEN RAISE EGraphicformat.Create('GetHistogram:  ' +
                   'Bitmap must be 8-bit or 24-bit.');

    index := 0;  {avoid compiler warning about initialization}

    for i := Low(THistogram) TO High(THistogram) DO
      Histogram[i] := 0;

    IF  Bitmap.Pixelformat = pf8bit
    THEN begin
      IF   ColorPlane <> cp8bit
      THEN RAISE EGraphicformat.Create('GetHistogram:  ' +
               'Attempting to extract color plane from 8-bit bitmap.');

      for j := Bitmap.Height-1 downto 0 DO
      begin
        Row8bit  := pBytearray(Bitmap.Scanline[j]);
        for i := Bitmap.Width-1 downto 0 DO
        begin
          INC (Histogram[Row8bit[i]])
        end
      end

    end
    else begin
      {Step through each row of image.}
      for j := Bitmap.Height-1 downto 0 DO
      begin
        Row  := pRGBarray(Bitmap.Scanline[j]);

        for i := Bitmap.Width-1 downto 0 DO
        begin
          CASE ColorPlane OF
            cpRGB,
            cpIntensity:   index := RGBIntensity(Row[i]);

            cpRed:        index := Row[i].rgbtRed;
            cpGreen:      index := Row[i].rgbtGreen;
            cpBlue:       index := Row[i].rgbtBlue;

            cpHueHSV:
              begin
                RGBtoHSV(Row[i], HSV);
                index := (255 * HSV.hsvHue) DIV 359
              end;

            cpSaturationHSV:
              begin
                RGBtoHSV(Row[i], HSV);
                index := HSV.hsvSaturation
              end;

            cpValue:
              begin
                RGBtoHSV(Row[i], HSV);
                index := HSV.hsvValue
              end;

            cpCyan:
              begin
                RGBtoCMYK(Row[i], CMYK);
                index := CMYK.cmykCyan
              end;

            cpMagenta:
              begin
                RGBtoCMYK(Row[i], CMYK);
                index := CMYK.cmykMagenta
              end;

            cpYellow:
              begin
                RGBtoCMYK(Row[i], CMYK);
                index := CMYK.cmykYellow
              end;

            cpBlack:
              begin
                RGBtoCMYK(Row[i], CMYK);
                index := CMYK.cmykK
              end;

          end;

          INC (Histogram[index]);
        end

      end

    end
end;


// -----------------------------------------------------------------------------
//
//			DrawHistogram
//
// -----------------------------------------------------------------------------
procedure DrawHistogram(const ColorPlane:  TColorPlane;
                          const Histogram :  THistogram;
                          const aCanvas    :  TCanvas;
                          const LineColor:TColor;                     //<- LineColor added 19.04.98
                          const TextColor:TColor);                     //<- TextColor added 19.04.98

const
      MajorTickSize = 8;

var
      BarLength:  INTEGER;
      Delta    :  INTEGER;
      dheight  :  Integer;                                                  //** Added
      Color    :  TColor;
      i        :  INTEGER;
      j        :  INTEGER;
      MaxValue :  INTEGER;
      MinValue :  Integer;                                                  //** Added
      RGBTriple:  TRGBTRiple;
      Width    :  INTEGER;
      lMargin  :  Integer;                                                  //** Added
      HSV:      THSVTriple;
begin
    Color := clBlack;    {avoid compiler warning about initialization}
    aCanvas.Pen.Style:=psSolid;

    lMargin:=44;                                                            //** Added
    dHeight   := aCanvas.ClipRect.Bottom-(4+MajorTickSize+aCanvas.TextHeight('X'));   //** Added
    Width    := aCanvas.ClipRect.Right;

    MaxValue := xMaxIntValue(Histogram);
    MinValue := xMinIntValue(Histogram);

    {for now only paint on a canvas exactly 256 pixels wide.  If
    MaxValue is zero, array was not filled in correctly and is ignored.}
    IF   (Width = 256+lMargin) AND (MaxValue > 0)
    THEN begin
      for i := Low(THistogram) TO High(THistogram) DO
      begin
        CASE ColorPlane OF
          cpRGB,
          cpIntensity:   Color := RGB(i, i, i);

          cpRed:         Color := RGB(i, 0, 0);
          cpGreen:       Color := RGB(0, i, 0);
          cpBlue:        Color := RGB(0, 0, i);
          cpHueHSV:
            begin
              HSV.hsvHue:=(i*359) DIV 255;
              HSV.hsvSaturation:=255;
              HSV.hsvValue:=255;
              HSVtoRGB(HSV, RGBTriple);
              with RGBTriple DO
                Color := RGB(rgbtRed, rgbtGreen, rgbtBlue)
            end;
          cpSaturationHSV:
                         Color := clBlack;
          cpValue:       Color := RGB(i, i, i);

        end;

        aCanvas.Pen.Color := Color;
        BarLength := ROUND(dHeight*Histogram[i] / MaxValue);
        aCanvas.MoveTo(i+lMargin, dHeight-1);
        aCanvas.LineTo(i+lMargin, dHeight-1-BarLength)
//          aCanvas.Pixels[i+lMargin, dHeight-1-BarLength]:=Color;
      end;

      aCanvas.Pen.Color := LineColor;
      {Vertical Lines for visual estimation}
      for i := 0 TO 25 DO
      begin
        aCanvas.MoveTo((10*i)+lMargin, dHeight-1);
        IF   i MOD 5 = 0
        THEN Delta := MajorTickSize
        else Delta := MajorTickSize DIV 2;
        aCanvas.LineTo((10*i)+lMargin, dHeight+1+Delta);
      end;

      {Horizontal Lines}
      for j := 0 TO 4 DO
      begin
        aCanvas.MoveTo(0+lMargin, j*dHeight DIV 5);
        aCanvas.LineTo(Width-1, j*dHeight DIV 5);
      end;

      //** Added +
      aCanvas.Font.Color:=TextColor;
      aCanvas.Brush.Style := bsClear;
      aCanvas.TextOut(lMargin-aCanvas.TextWidth(IntToStr(MaxValue))-2, 0, IntToStr(MaxValue));
      aCanvas.TextOut(lMargin-aCanvas.TextWidth(IntToStr(MinValue))-2, dHeight, IntToStr(MinValue));
      //** Added -

      aCanvas.TextOut(2+lMargin, dHeight+aCanvas.TextHeight('X') - MajorTickSize+3, '0');
      aCanvas.TextOut(Width-aCanvas.TextWidth('250 '),
                     dHeight+aCanvas.TextHeight('X') - MajorTickSize+3, '250')
    end
end;

//------------------------------------------------------------
// Palette
//------------------------------------------------------------

// -----------------------------------------------------------------------------
//
//			GetColorDepth
//
// -----------------------------------------------------------------------------
  // Adapted from Tommy Andersen's ColorDepth post to
  // comp.lang.pascal.delphi.components.misc, 5 Oct 1997.
  //
  // According to Tim Robert's post "Bitmaps with > 256 Colors" to
  // compl.lang.pascal.delphi.misc, 17 Apr 1997, "in a windows bitmap,
  // one of either the bits-per-plane or the number of planes must be 1.  In
  // fact, the planar form is only supported at 4 bits per pixel.  A 24-bit
  // true color bitmap must have bits-per-pixel set to 24 and planes set to 1.
function GetColorDepth:  INTEGER;
var
      DeviceContext:  hDC;
begin
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := 1 SHL (GetDeviceCaps(DeviceContext, PLANES) *
                       GetDeviceCaps(DeviceContext, BITSPIXEL))
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    end;
end;


// -----------------------------------------------------------------------------
//
//			GetPixelDepth
//
// -----------------------------------------------------------------------------
function  GetPixelDepth:  INTEGER;
    var
      DeviceContext:  hDC;
begin
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := GetDeviceCaps(DeviceContext, BITSPIXEL)
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    end;
end;


// -----------------------------------------------------------------------------
//
//			GetDesktopPalette
//
// -----------------------------------------------------------------------------
  // Adapted from PaletteFromDesktop function in "Delphi 2 Multimedia," Jarol,
  // et al, 1996, Coriolis Group Books, p. 307.  Unlike book version, use
  // TMaxLogPalette and avoid allocating and freeing a pLogPalette area.
function GetDesktopPalette:  hPalette;
var
      LogicalPalette     :  TMaxLogPalette;
      ScreendeviceContext:  hDC;
      ReturnCode         :  INTEGER;
begin
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreendeviceContext := GetDC(0);
    TRY
      // Get all 256 entries
      ReturnCode :=  GetSystemPaletteEntries(ScreendeviceContext,
                                             0, 255, LogicalPalette.palPalEntry)
    FINALLY
      ReleaseDC(0, ScreendeviceContext)
    end;

    IF   ReturnCode >0
    THEN RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    else RESULT := 0
end;


// -----------------------------------------------------------------------------
//
//			GetGradientPalette
//
// -----------------------------------------------------------------------------
  // Parameters identify which planes participate.
  // Use all TRUE for shades of gray.
function  GetGradientPalette(const red, green, blue:  BOOLEAN):  hPalette;
var
      ScreendeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;

    function ScaleValue(const flag:  BOOLEAN; const i:  INTEGER):  INTEGER;
    begin
      IF   flag
      THEN RESULT := MulDiv(i, 255, 235)
      else RESULT := 0
    end;

begin
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreendeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreendeviceContext,
                                0, 10, LogicalPalette.palPalEntry[0]);   // Maintain first 10
      GetSystemPaletteEntries(ScreendeviceContext,
                              246, 10, LogicalPalette.palPalEntry[246]); // Maintain last  10
    FINALLY
      ReleaseDC(0, ScreendeviceContext)
    end;

    // Skip over first 10 and last 10 "fixed" entries
    for i := 0 TO 255-20 DO
    begin
      // Skip over first 10 "fixed" entries in system palette
      LogicalPalette.palPalEntry[10+i].peRed   := ScaleValue(Red,   i);
      LogicalPalette.palPalEntry[10+i].peGreen := ScaleValue(Green, i);
      LogicalPalette.palPalEntry[10+i].peBlue  := ScaleValue(Blue,  i);
      LogicalPalette.palPalEntry[10+i].peFlags := pC_RESERVED;
    end;

    RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
end;


// -----------------------------------------------------------------------------
//
//			CreateOptimizedPaletteForSingleBitmap
//
// -----------------------------------------------------------------------------
  // Adapted from MSJ "Wicked Code" article, Oct 97, pp. 79-84
  // Bitmap must be a DIB.
function  CreateOptimizedPaletteforSingleBitmap(const Bitmap:  TBitmap; const ColorBits:  INTEGER): hPalette;
var
      ColorQuantizer     :  TColorQuantizer;
      ScreendeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      RGBQuadarray       :  TRGBQuadarray;
begin
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreendeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreendeviceContext,
                                0,256, LogicalPalette.palPalEntry[0]);   // Need first 10 and last 10
    FINALLY
      ReleaseDC(0, ScreendeviceContext)
    end;

    // Normally for 24-bit images, use ColorBits of 5 or 6.  for 8-bit images
    // use ColorBits = 8.
    ColorQuantizer := TColorQuantizer.Create(236, ColorBits);
    TRY
      ColorQuantizer.ProcessImage(Bitmap.Handle);
      ColorQuantizer.GetColorTable(RGBQuadarray);

      // Skip over first 10 and last 10 "fixed" entries
      for i := 0 TO 255-20 DO
      begin
        // Skip over first 10 "fixed" entries in system palette
        LogicalPalette.palPalEntry[10+i].peRed   := RGBQuadarray[i].rgbRed;
        LogicalPalette.palPalEntry[10+i].peGreen := RGBQuadarray[i].rgbGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := RGBQuadarray[i].rgbBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := RGBQuadarray[i].rgbReserved
      end;
      RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    FINALLY
      ColorQuantizer.Free
    end
end;


// -----------------------------------------------------------------------------
//
//			CreateOptimizedPaletteForManyBitmaps
//
// -----------------------------------------------------------------------------
  // This separate function is for convenience in processing many bitmaps to
  // obtain an optimized palette.  The CallBack is called until a NIL pointer
  // is returned.
function  CreateOptimizedPaletteforManyBitmaps(const CallBack:  TGetBitmapCallback;
                                                 const ColorBits:  INTEGER): hPalette;
var
      Bitmap             :  TBitmap;
      ColorQuantizer     :  TColorQuantizer;
      Counter            :  INTEGER;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      OK                 :  BOOLEAN;
      RGBQuadarray       :  TRGBQuadarray;
      ScreendeviceContext:  hDC;
begin
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreendeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreendeviceContext,
                                0,256, LogicalPalette.palPalEntry[0]);   // Need first 10 and last 10
    FINALLY
      ReleaseDC(0, ScreendeviceContext)
    end;

    // Normally for 24-bit images, use ColorBits of 5 or 6.  for 8-bit images
    // use ColorBits = 8.
    ColorQuantizer := TColorQuantizer.Create(236, ColorBits);
    TRY

      // Keep calling the callback for more bitmaps until a NIL pointer is
      // returned.  Use Counter and OK parameters is for convenience.
      Counter := 1;
      CallBack(Counter, OK, Bitmap);
      WHILE (Bitmap <> NIL) DO
      begin
        IF   OK
        THEN ColorQuantizer.ProcessImage(Bitmap.Handle);

        INC(Counter);
        CallBack(Counter, OK, Bitmap)
      end;

      ColorQuantizer.GetColorTable(RGBQuadarray);

      // Skip over first 10 and last 10 "fixed" entries
      for i := 0 TO 255-20 DO
      begin
        // Skip over first 10 "fixed" entries in system palette
        LogicalPalette.palPalEntry[10+i].peRed   := RGBQuadarray[i].rgbRed;
        LogicalPalette.palPalEntry[10+i].peGreen := RGBQuadarray[i].rgbGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := RGBQuadarray[i].rgbBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := RGBQuadarray[i].rgbReserved
      end;
      RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    FINALLY
      ColorQuantizer.Free
    end
end;


// -----------------------------------------------------------------------------
//
//			IsPaletteDevice
//
// -----------------------------------------------------------------------------
  //  Adapted from Joe C. Hecht's BitTBitmapAsDIB post to
  //  borland.public.delphi.winapi, 12 Oct 1997.
function IsPaletteDevice:  BOOLEAN;
    var
      DeviceContext:  hDC;
begin
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := GetDeviceCaps(DeviceContext, RASTERCAPS) AND RC_PALETTE = RC_PALETTE
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    end
end;


// -----------------------------------------------------------------------------
//
//			LogicalPaletteToRGBQuad
//
// -----------------------------------------------------------------------------
procedure LogicalPaletteToRGBQuad(const Start       :  WORD;
                                    const Count       :  WORD;
                                    var LogicalPalette:  TPaletteEntries;
                                    var RGBQuad       :  TRGBQuadarray);
var
      i:  INTEGER;
begin
    for i := Start TO Start+Count-1 DO
    begin
      RGBQuad[i].rgbRed      := LogicalPalette[i].peRed;
      RGBQuad[i].rgbGreen    := LogicalPalette[i].peGreen;
      RGBQuad[i].rgbBlue     := LogicalPalette[i].peBlue;
      RGBQuad[i].rgbReserved := LogicalPalette[i].peFlags
    end
end;


// -----------------------------------------------------------------------------
//
//			ReadAndCreatePaletteFromFractIntFile
//
// -----------------------------------------------------------------------------
  //  Adapted from "DibDemo" by John Biddiscombe, J.Biddiscombe@r1.ac.uk
function ReadAndCreatePaletteFromFractIntFile(const Filename      :  STRING):  hPalette;
var
      Blue               :  INTEGER;
      Green              :  INTEGER;
      ScreendeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      FractIntPalFile    :  TextFile;
      Red                :  INTEGER;
begin
    AssignFile(FractIntPalFile, Filename);
    RESET(FractIntPalFile);

    READLN(FractIntPalFile, PaletteColorCount);
    IF   PaletteColorCount >  236
    THEN Palettecolorcount := 236;

    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreendeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreendeviceContext,
                                0, 10, LogicalPalette.palPalEntry[0]);   // Maintain first 10
      GetSystemPaletteEntries(ScreendeviceContext,
                              246, 10, LogicalPalette.palPalEntry[246]); // Maintain last  10
    FINALLY
      ReleaseDC(0, ScreendeviceContext)
    end;

    for i := 0 TO PaletteColorCount-1 DO
    begin
      READLN(FractIntPalFile, Red, Green, Blue);

      // Skip over first 10 "fixed" entries in system palette
      LogicalPalette.palPalEntry[10+i].peRed   := Red;
      LogicalPalette.palPalEntry[10+i].peGreen := Green;
      LogicalPalette.palPalEntry[10+i].peBlue  := Blue;

      // Must be PC_RESERVED if AnimatePalette is to be used
      LogicalPalette.palPalEntry[10+i].peFlags := PC_RESERVED;
    end;

    IF   PaletteColorCount-1 < 235
    THEN begin
      for i := PaletteColorCount TO 235
      DO begin
        LogicalPalette.palPalEntry[10+i].peRed   := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peRed;
        LogicalPalette.palPalEntry[10+i].peGreen := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := pc_Reserved;
      end
    end;

    RESULT := CreatePalette(pLogPalette(@LogicalPalette)^);

    CloseFile(FractIntPalFile);
end;

// -----------------------------------------------------------------------------
//
//			WMQuereyNewPalette
//
// -----------------------------------------------------------------------------
 //  This message informs a window that it is about to receive the
  //  keyboard focus, giving the window the opportunity to realize its
  //  logical palette when it receives the focus.
procedure WMQueryNewPalette(formcanvashandle:THandle; PaletteHandle:  hPalette;
                              var Msg:  TWMQueryNewPalette);
var
      DeviceContext:  hDC;
      Palette      :  hPalette;
      ReturnCode   :  INTEGER;
begin
    DeviceContext := formCanvasHandle;

    // Select the specified palette into a device context.
    // FALSE parameter forces logical palette to be copied into the device palette
    // when the application is in the foreground.
    Palette := SelectPalette(DeviceContext, PaletteHandle, FALSE);

    // Map palette entries from the current logical palette to the system palette.
    // Returned value is the number of entries in the logical palette mapped to
    // the system palette.
    ReturnCode := RealizePalette(DeviceContext);

    // Restore the old palette into the device context.
    SelectPalette(DeviceContext, Palette, FALSE);

    // If new entries were mapped to the system palette, then invalidate the
    // image so it gets repainted.
  //  IF   ReturnCode > 0
  //  THEN form.Invalidate;

    Msg.Result := ReturnCode
end;


// -----------------------------------------------------------------------------
//
//			WMPaletteChanged
//
// -----------------------------------------------------------------------------
  // This message is sent to all top-level and overlapped windows after the
  // window with keyboard focus has realized its logical palette, thereby
  // changing the system palette.  This message enables a window that uses a
  // color palette but does not have the keyboard focus to realize its logical
  // palette and update its client area.
procedure WMPaletteChanged(formcanvashandle:THandle;formhandle:THandle; PaletteHandle:  hPalette;
                            var Msg:  TWMPaletteChanged);
var
      DeviceContext:  hDC;
      Palette      :  hPalette;
begin
    IF  Msg.PalChg = formHandle
    THEN begin
      // To avoid creating an infinite loop, a window that receives this message
      // must not realize its palette (unless it determines the window that
      // caused the palette to change is not its own)
      Msg.Result := 0
    end
    else begin
      DeviceContext := formCanvasHandle;

      // Select the specified palette into a device context.
      // A TRUE parameter causes the logical palette to be mapped to the colors
      // already in the physical palette in the best possible way.
      Palette := SelectPalette(DeviceContext, PaletteHandle, TRUE);

      // Map palette entries from the current logical palette to the system palette.
      // Returned value is the number of entries in the logical palette mapped to
      // the system palette.
      RealizePalette(DeviceContext);

      // UpdateColors is Microsoft's preferred way to refresh an on-screen Window
      // if the system palette changes.  UpdateColors updates the clinet area
      // of the specified device contenxt by remapping the current colors in the
      // client area to the currently realized logical palette.  An inactive window
      // with a realized logical palette may call UpdateColors as an alternative
      // to redrawing its client area when the system palette changes.
      UpdateColors(DeviceContext);

      // Restore the old palette into the device context.
      SelectPalette(DeviceContext, Palette, FALSE);

      Msg.Result := 1;
    end
end;

// -----------------------------------------------------------------------------
//
//			ReadPaletteFromFile
//
// -----------------------------------------------------------------------------
function ReadPaletteFromFile(FileName:String):HPalette;
var F:TextFile;
    s:String;
    i:Integer;
    fLogicalPalette:TMaxLogPalette;
    fRGBQuadarray:TRGBQuadarray;
begin
  AssignFile(F,FileName);
  Reset(F);
  ReadLn(F,s);
  ReadLn(f,s);
  ReadLn(f,s);
  i:=0;
  while not EOF(f) do
  begin
   ReadLn(f,s);
    fRGBQuadarray[i].rgbRed:=StrToInt(Copy(s,1,Pos(' ',s)-1));
    Delete(s,1,Pos(' ',s));
    fRGBQuadarray[i].rgbGreen:=StrToInt(Copy(s,1,Pos(' ',s)-1));
    Delete(s,1,Pos(' ',s));
    fRGBQuadarray[i].rgbBlue:=StrToInt(s);
    Inc(i,1);
    fRGBQuadarray[i].rgbReserved:=0;
   end;

    fLogicalPalette.palVersion    := PaletteVersion;
    fLogicalPalette.palNumEntries := 256;

      for i := 0 TO 255 DO
      begin
        fLogicalPalette.palPalEntry[i].peRed   := fRGBQuadarray[i].rgbRed;
        fLogicalPalette.palPalEntry[i].peGreen := fRGBQuadarray[i].rgbGreen;
        fLogicalPalette.palPalEntry[i].peBlue  := fRGBQuadarray[i].rgbBlue;
        fLogicalPalette.palPalEntry[i].peFlags := fRGBQuadarray[i].rgbReserved
      end;
      Result := CreatePalette(pLogPalette(@fLogicalPalette)^);
end;

// -----------------------------------------------------------------------------
//
//			LoadOptimizedPalette
//
// -----------------------------------------------------------------------------
function LoadOptimizedPalette:HPalette;
var i:Integer;
    fLogicalPalette:TMaxLogPalette;
    fRGBQuadarray:TRGBQuadarray;
begin
  for i:=0 to 255 do
  begin
    fRGBQuadarray[i].rgbRed:=Optpal[i,0];
    fRGBQuadarray[i].rgbGreen:=Optpal[i,1];
    fRGBQuadarray[i].rgbBlue:=Optpal[i,2];
    fRGBQuadarray[i].rgbReserved:=0;
   end;

    fLogicalPalette.palVersion    := PaletteVersion;
    fLogicalPalette.palNumEntries := 256;

      for i := 10 TO 255-10 DO
      begin
        fLogicalPalette.palPalEntry[i].peRed   := fRGBQuadarray[i].rgbRed;
        fLogicalPalette.palPalEntry[i].peGreen := fRGBQuadarray[i].rgbGreen;
        fLogicalPalette.palPalEntry[i].peBlue  := fRGBQuadarray[i].rgbBlue;
        fLogicalPalette.palPalEntry[i].peFlags := fRGBQuadarray[i].rgbReserved
      end;
      Result := CreatePalette(pLogPalette(@fLogicalPalette)^);
end;

// -----------------------------------------------------------------------------
//
//			ApplyPaletteToGraphic
//
// -----------------------------------------------------------------------------
procedure ApplyPaletteToGraphic(Palette:Hpalette;Graphic:TGraphic;ChangePixelformat:Boolean;NewPixelformat:TPixelformat);
begin
  if (Palette<>0) then
  begin
{    if Graphic is TJPEGImage then
    begin
      TJPEGImage(Graphic).Pixelformat:=jf8Bit;
      DeleteObject(TJPEGImage(Graphic).palette);
      TJPEGImage(Graphic).Palette:=Palette;;

    end else if Graphic is TBitmap then}
    begin
      TBitmap(Graphic).Pixelformat:=NewPixelformat;
      DeleteObject(TBitmap(Graphic).ReleasePalette);
//      TBitmap(Graphic).Palette:=Palette;
      TBitmap(Graphic).Palette:=CopyPalette(Palette);
      TBitmap(Graphic).Canvas.Refresh;
    end;
  end;
end;

// -----------------------------------------------------------------------------
//
//			ApplyPaletteToDC
//
// -----------------------------------------------------------------------------
procedure ApplyPaletteToDC(DC:HDC;Palette:HPalette);
begin
  if Palette<>0 then
  begin
    SelectPalette(DC, Palette, False);
    RealizePalette(DC);
  end;
end;

// -----------------------------------------------------------------------------
//
//			PaletteDefaults
//
// -----------------------------------------------------------------------------
procedure PaletteDefaults(PaletteEntries:TPaletteEntries);
var
  J:Integer;
begin
for J:=0 To 15 Do
    begin
        PaletteEntries[J].peRed:=StdPalette[J,1];
        PaletteEntries[J].peGreen:=StdPalette[J,2];
        PaletteEntries[J].peBlue:=StdPalette[J,3];
    end;
end;

// -----------------------------------------------------------------------------
//
//			MakeGreyPalette
//
// -----------------------------------------------------------------------------
procedure MakeGreyPalette(MaxLogPalette:TMaxLogPalette);
var
  i:Integer;
begin
MaxLogPalette.palVersion:=PaletteVersion;
MaxLogPalette.palNumEntries:=256;
for i:=0 To 255 Do
    begin
      MaxLogPalette.palPalEntry[i].peRed    :=i*63 Div 255;
      MaxLogPalette.palPalEntry[i].peGreen  :=i*63 Div 255;
      MaxLogPalette.palPalEntry[i].peBlue   :=i*63 Div 255;
      MaxLogPalette.palPalEntry[i].peFlags  :=0;
    end;
end;

// -----------------------------------------------------------------------------
//
//			MakeStandardPalette
//
// -----------------------------------------------------------------------------
procedure MakeStandardPalette(MaxLogPalette:TMaxLogPalette);
var
  i:integer;
  r,g,b:Word;
begin
  MaxLogPalette.palVersion:=PaletteVersion;
  MaxLogPalette.palNumEntries:=256;
  i:=0;
  for R:=0 to 7 do
  begin
    for G:=0 to 7 do
    begin
      for B:=0 to 3 do
      begin
         MaxLogPalette.palPalEntry[i].peRed    :=(R+1)*8-1;
         MaxLogPalette.palPalEntry[i].peGreen  :=(G+1)*8-1;
         MaxLogPalette.palPalEntry[i].peBlue   :=(B+1)*16-1;
         MaxLogPalette.palPalEntry[i].peFlags  :=0;
         Inc(i);
      end;
    end;
  end;
end;

//------------------------------------------------------------
// Spliting
//------------------------------------------------------------

// -----------------------------------------------------------------------------
//
//			GetPlane
//
// -----------------------------------------------------------------------------
procedure GetPlane(SrcBitmap,DestBitmap:TBitmap;Plane:Integer);
 var
   lig  :  Integer;
   i        :  INTEGER;
   j        :  INTEGER;
   rowRGB   :  pRGBarray;
   rowD     :  pRGBarray;
   HSV      :  THSVTriple;
   HSL      :  THSLTriple;
begin

  SetBitmapsEql(SrcBitmap,DestBitmap);

    for   j := SrcBitmap.Height-1 downto 0 DO
    begin
      rowRGB := SrcBitmap.Scanline[j];
      rowD   := DestBitmap.Scanline[j];
      for i := SrcBitmap.Width-1 downto 0 DO
      begin
        case Plane of
        1:begin
            rowD[i].rgbtRed   := rowRGB[i].rgbtRed;
            rowD[i].rgbtGreen := 0;
            rowD[i].rgbtBlue  := 0
          end;
        2:begin
            rowD[i].rgbtRed   := 0;
            rowD[i].rgbtGreen := rowRGB[i].rgbtGreen;
            rowD[i].rgbtBlue  := 0
          end;
        3:begin
            rowD[i].rgbtRed   := 0;
            rowD[i].rgbtGreen := 0;
            rowD[i].rgbtBlue  := rowRGB[i].rgbtBlue;
          end;
        4:begin
            RGBtoHSV(rowRGB[i],  HSV);
            HSV.hsvHue := (HSV.hsvHue * 255) DIV 359;
            rowD[i].rgbtRed   := HSV.hsvHue;
            rowD[i].rgbtGreen := HSV.hsvHue;
            rowD[i].rgbtBlue  := HSV.hsvHue;
          end;
        5:begin
            RGBtoHSV(rowRGB[i], HSV);
            rowD[i].rgbtRed   := HSV.hsvSaturation;
            rowD[i].rgbtGreen := HSV.hsvSaturation;
            rowD[i].rgbtBlue  := HSV.hsvSaturation;
          end;
        6:begin
            RGBtoHSV(rowRGB[i], HSV);
            rowD[i].rgbtRed   := HSV.hsvValue;
            rowD[i].rgbtGreen := HSV.hsvValue;
            rowD[i].rgbtBlue  := HSV.hsvValue;
          end;
        7:begin
            RGBtoHSL(rowRGB[i], HSL);
            rowD[i].rgbtRed   := HSL.hslLightness ;
            rowD[i].rgbtGreen := HSL.hslLightness;
            rowD[i].rgbtBlue  := HSL.hslLightness;
          end;
        end;
      end;

    end
end;

// -----------------------------------------------------------------------------
//
//			GetRedPlane
//
// -----------------------------------------------------------------------------
procedure GetRedPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,1);
end;

// -----------------------------------------------------------------------------
//
//			GetGreenPlane
//
// -----------------------------------------------------------------------------
procedure GetGreenPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,2);
end;

// -----------------------------------------------------------------------------
//
//			GetBluePlane
//
// -----------------------------------------------------------------------------
procedure GetBluePlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,3);
end;

// -----------------------------------------------------------------------------
//
//			GetHuePlane
//
// -----------------------------------------------------------------------------
procedure GetHuePlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,4);
end;

// -----------------------------------------------------------------------------
//
//			GetSatPlane
//
// -----------------------------------------------------------------------------
procedure GetSatPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,5);
end;

// -----------------------------------------------------------------------------
//
//			GetValPlane
//
// -----------------------------------------------------------------------------
procedure GetValPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,6);
end;

// -----------------------------------------------------------------------------
//
//			GetLigPlane
//
// -----------------------------------------------------------------------------
procedure GetLigPlane(SrcBitmap:TBitmap;DestBitmap:TBitmap);
begin
  GetPlane(SrcBitmap,DestBitmap,7);
end;

end.
