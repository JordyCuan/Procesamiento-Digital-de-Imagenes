// Mini Sistema para calculr la Transformada de Fourier de una Imagen
// mediante el método de la FFT-2D
//
// Autor. Manuel Martin Ortiz
// FCC - BUAP
//
// Curso de "Procesamiento Digital de Imágenes"
//
// Ultima Revisión : 6 noviembre 2007
// Versión : 0.0.3

unit UFourier;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ExtDlgs, ToolWin, ComCtrls, StdCtrls, Math, UFTipos, JPEG,
  ImgList, Buttons, UBase;

type

  TFFourier = class(TForm)
    MainMenu1: TMainMenu;
    OpenPD: TOpenPictureDialog;
    Archivo1: TMenuItem;
    Transformada1: TMenuItem;
    Filtros1: TMenuItem;
    Abrir1: TMenuItem;
    Salvar1: TMenuItem;
    Directa1: TMenuItem;
    Inversa1: TMenuItem;
    PasaBaja1: TMenuItem;
    PasaAlta1: TMenuItem;
    PasaBanda1: TMenuItem;
    N1: TMenuItem;
    Salida1: TMenuItem;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    SB1: TStatusBar;
    SavePictureDialog1: TSavePictureDialog;
    DF1: TMenuItem;
    Salvar2: TMenuItem;
    Abrir2: TMenuItem;
    SalvarImTF1: TMenuItem;
    OpenD_TDF: TOpenDialog;
    SaveD_TDF: TSaveDialog;
    Panel2: TPanel;
    Label5: TLabel;
    TrackBar2: TTrackBar;
    Label7: TLabel;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    Bevel1: TBevel;
    ImageList1: TImageList;
    ToolButton14: TToolButton;
    ToolButton6: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    h1Butterworth1: TMenuItem;
    Panel3: TPanel;
    ToolButton18: TToolButton;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    Label8: TLabel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    TrackBar3: TTrackBar;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label10: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    TrackBar4: TTrackBar;
    Label15: TLabel;
    Patrones1: TMenuItem;
    CheckBox1: TCheckBox;
    RuidoPeriodico1: TMenuItem;
    BitBtn1: TBitBtn;
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RuidoPeriodico1Click(Sender: TObject);
    procedure Patrones1Click(Sender: TObject);
    procedure Propiedades1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure Image3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Abrir1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Salida1Click(Sender: TObject);
    procedure Directa1Click(Sender: TObject);
    procedure Inversa1Click(Sender: TObject);
    procedure PasaBaja1Click(Sender: TObject);
    procedure Salvar2Click(Sender: TObject);
    procedure Abrir2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SalvarImTF1Click(Sender: TObject);
    procedure Salvar1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure PasaAlta1Click(Sender: TObject);
    procedure PasaBanda1Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }
    BM      : TBitMap;
    jBM     : TJPegImage;

    nom,ext : string;
    ta      : char;
    Nx,Ny   : integer;
    Mx,My   : integer;
    pf      : TPixelFormat;
    mat0,
    mat1    : Bmatrix2D;
    matcd,
    matci   : TComplexmatrix;

    Bimag,
    Btdf,
    Btdfi   : boolean;

    gamma   : real;

    xc,yc   : integer;

    procedure Conv (B       : TBitMap    ;
                    var mat : Bmatrix2D  ;
                    Nx,Ny   : integer   ); overload;

    procedure Conv (mat     : Bmatrix2D  ;
                    var B   : TBitMap    ;
                    Nx,Ny   : integer   ); overload;

//    procedure ConvGris (var B :TBitMap);

    procedure TDFInversa(filtro : TFil);

    procedure MuestraTDF( v : Tvista; p : integer = 1);

    procedure AjustaLimites;

    procedure FButterworth(Sender : TObject; k : byte);


    public
    { Public declarations }
    p_ruido : byte;
    MT : MatImg;
    procedure Elimina_ruido();
  end;

var
  FFourier: TFFourier;

implementation

uses UFourierBase, UFPatron, UFCapZonas;

{$R *.DFM}

// Usar imagen en el main
procedure TFFourier.BitBtn1Click(Sender: TObject);
begin
  BMP2Mat(Image3.Picture.Bitmap, MT);
end;

// Evento Onshow, agregamos la imagen de trabajo
procedure TFFourier.FormShow(Sender: TObject);
var
  j    : integer;
  bim  : boolean;
begin
  // Tenemos la matriz de trabajo copiada, la asignamos al Bitmap
  Mat2BMP(MT, BM);
  BM.PixelFormat := pf24bit;
  // Realizamos el proceso de apertura
  bim := true;
  ta := 'b';
  Nx := BM.Width;
  Ny := BM.Height;
  SB1.Panels[1].text := IntToStr(Nx)+'x'+IntToStr(Ny);
  pf := BM.PixelFormat;

  if pf = pf24bit then begin
    // completa columnas y renglones a múltiplos de 2^n
    j := 2;
    while ((Nx div j) > 0 ) do j := 2*j;
    if (Nx = (j div 2)) then j := j div 2;
    Mx := j;

    j := 2;
    while ((Ny div j) > 0 ) do j := 2*j;
    if (Ny = (j div 2)) then j := j div 2;
    My := j;

    // centro de las imágenes ajustadas
    xc := Mx div 2;
    yc := My div 2;

    // reporta nuevas dimensiones
    SB1.Panels[3].text := IntToStr(Mx)+'x'+IntToStr(My);

    // Convierte y muestra Imagen en tonos de gris
    //ConvGris(BM);

    // pasa el BitMap a su forma matricial
    SetLength(Mat0,Mx,My);
    Conv(BM,Mat0,Mx,My);

    // Muestra imagen con bordes negros en caso que haya crecido
    Conv(Mat0,BM,Mx,My);

    // Muestra Imagen de entrada
    Image1.Picture.Assign(BM);

    // Oculta opciones
    Salvar1.Visible     := false;
    Filtros1.Visible    := false;
    Inversa1.Visible    := false;
    ToolBar2.Visible    := false;

    // Muestra opciones
    Transformada1.Visible    := true;
    Directa1.Visible         := true;
    ToolButton7.visible      := true;
    Panel2.Visible           := false;
    Patrones1.Visible        := true;
    RuidoPeriodico1.Visible  := false;

    // Imagen cargada
    Bimag := true;
  end
  else ShowMessage('Profundidad en bits no soportada para el Proceso...SIC');
end;

// Asumimos que la imagen esta en tonos de gris,
// se toma el canal rojo como base

// Convierte Bitmap en Matriz
procedure TFFourier.Conv(B :TBitMap; var mat : Bmatrix2D; Nx,Ny : integer);
var
  i,j,k  : integer;
  p      : PByteArray;
  kx,ky,
  sx,sy  : integer;
begin
  kx := MIN(Nx,  B.Width);
  ky := MIN(Ny, B.Height);

  sx := (Nx-kx) shr 1;
  sy := (Ny-ky) shr 1;

  // limpia la matriz
  for j := 0 to Ny-1 do
    for i := 0 to Nx-1 do
      mat[i,j] := 0;

  // Llena la matriz con los datos de la imagen
  for j := 0 to ky-1 do begin
    p := B.ScanLine[j];
    for i := 0 to kx-1 do begin
      k := 3*i;
      mat[i+sx,j+sy] := p[k];
    end;
  end;
end;

// Convierte matriz en Bitmap
procedure TFFourier.Conv (mat : Bmatrix2D ; var B :TBitMap; Nx,Ny : integer);
var
  i,j,k,q,c : integer;
//  p       : PByteArray;
  pix     : byte;
  ton     : integer;

begin
  Nx := MAX(Nx,B.Width);
  Ny := MAX(Ny,B.Height);
  B.Width  := Nx;
  B.Height := Ny;

//  SetLength(mat,Nx,Ny);  // aguas

  for j := 0 to Ny-1 do begin
//    p := B.ScanLine[j];
    for i := 0 to Nx-1 do begin
//      k   := 3*i;
      pix := ajusta255(mat[i,j]);
      B.Canvas.Pixels[i,j] := RGB(pix,pix,pix);
//      for q := 0 to 2 do p[k+q] := pix;
    end;
  end;
end;

// Convierte a Grises
{procedure TFFourier.ConvGris(var B :TBitMap);
var
  i,j,k,s : integer;
  p       : PByteArray;
  kx,ky   : integer;
begin
  kx := B.Width;
  ky := B.Height;

  for j := 0 to ky-1 do begin
    p := B.ScanLine[j];
    for i := 0 to kx-1 do begin
      k := 3*i;
      s := (p[k]+p[k+1]+p[k+2]) div 3;
      p[k]   := s;
      p[k+1] := s;
      p[k+2] := s;
    end;
  end;
end;

{ -------------------------  Vistas de la TDF ------------------------------- }

// Reflejo de la selección del parámetro k para Ln(k*z+1)
procedure TFFourier.TrackBar3Change(Sender: TObject);
begin
  label9.Caption := 'k = '+IntToSTr(TrackBar3.Position);
end;

procedure TFFourier.TrackBar4Change(Sender: TObject);
var
  p,q : integer;
begin
  p := TrackBar4.Position;
  q :=  p+1;
  Label15.caption := '1/'+ IntToStr(q);
  gamma := 1/q;
end;

// Muestra la TDF
procedure TFFourier.MuestraTDF( v : Tvista; p : integer);
var
  Bx,Rx,By,Ry: integer;
begin
  // Convierte Matcd a su forma escalar real
  MatrizEsc(Matcd,mat1,Mx,My,v,p);

  // Dimensionamos el BitMap de Salida / panel 2
  BM.Width  := Mx;
  BM.Height := My;
  BM.PixelFormat := pf24bit;

  // Limpiamos la zona
  BM.canvas.brush.Color:= clwhite;
  BM.canvas.pen.Color:= clwhite;
  BM.Canvas.Rectangle(0,0,Mx-1,My-1);

  // Convertimos la matriz a un BitMap
  Conv (mat1,BM,Mx,My);

  // Mostramos en el Image2
  Image2.Picture.Assign(BM);

  // centra el espectro en el scrollBox
  Bx := ScrollBox2.Width - ScrollBox2.HorzScrollBar.ButtonSize div 2;
  Rx := ScrollBox2.HorzScrollBar.Range;
  if Rx > Bx then ScrollBox2.HorzScrollBar.Position := (Rx-Bx) div 2;

  By := ScrollBox2.Width - ScrollBox2.VertScrollBar.ButtonSize div 2;
  Ry := ScrollBox2.VertScrollBar.Range;
  if Ry > By then ScrollBox2.VertScrollBar.Position := (Ry-By) div 2;
end;

// Ln(z+1)
procedure TFFourier.Button1Click(Sender: TObject);
begin
  MuestraTDF(VLn);
end;

// Ln(k*z+1)
procedure TFFourier.Button2Click(Sender: TObject);
var
  k : integer;
begin
  // Lee el parámetro k
  k := TrackBar3.Position;
  MuestraTDF(VLnk,k);
end;

// Negativo de la Imagen Reconstruida
procedure TFFourier.Button3Click(Sender: TObject);
var
  x,y : integer;
begin
  if btdfi then begin

    for y := 0 to My-1 do
      for x := 0 to Mx - 1 do
        mat1[x][y] := 255 - mat1[x][y];

    Conv (mat1,BM,Mx,My);
    Image3.Picture.Assign(BM);
  end;
end;

// Aclarado SIN la Imagen Reconstruida
procedure TFFourier.Button4Click(Sender: TObject);
var
  x,y,p : integer;
  C1,t  : Real;
  TD    : array[0..255] of byte;
begin
  if btdfi then begin
    // Llenamos LUT
    C1 := PI/(2*255);
    for x := 0 to 255 do begin
      t := 255*sin(C1*x);
      TD[x] := round(t) and $FF;
    end;

    // Aplicamos LUT
    for y := 0 to My-1 do
      for x := 0 to Mx - 1 do begin
        p := mat1[x][y];
        mat1[x][y] := TD[p];
      end;

    Conv (mat1,BM,Mx,My);
    Image3.Picture.Assign(BM);
  end;
end;

// Aclarado Log(Z+1)  -- Rango Dinámico Simple
procedure TFFourier.Button5Click(Sender: TObject);
var
  x,y,p   : integer;
  C1,t    : real;
  TD      : array[0..255] of byte;
begin
  if Btdfi then begin
    C1 := 255/LnXP1(255);
    // Llenamos LUT
    for x := 0 to 255 do begin
      t := C1*LnXP1(x);
      TD[x] := round(t) and $FF;
    end;

    // Aplicamos LUT
    for y := 0 to My-1 do
      for x := 0 to Mx - 1 do begin
        p := mat1[x][y];
        mat1[x][y] := TD[p];
      end;

    Conv (mat1,BM,Mx,My);
    Image3.Picture.Assign(BM);
  end;
end;

// Corrección Gamma de aclarado 1/2 - 1/8
procedure TFFourier.Button6Click(Sender: TObject);
var
  x,y,p   : integer;
  t       : real;
  TD      : array[0..255] of byte;
begin
  if Btdfi then begin
    // Llenamos LUT
    for x := 0 to 255 do begin
      t := 255*power(x/255,gamma);
      TD[x] := round(t) and $FF;
    end;
    // Aplicamos LUT
    for y := 0 to My-1 do
      for x := 0 to Mx - 1 do begin
        p := mat1[x][y];
        mat1[x][y] := TD[p];
      end;

    Conv (mat1,BM,Mx,My);
    Image3.Picture.Assign(BM);
  end;
end;

// -----------------------------------------------------------------
//                   Procedimientos de la interfase
// -----------------------------------------------------------------

procedure TFFourier.FormCreate(Sender: TObject);
begin
  BM  := TBitMap.Create;
  jBM := TjpegImage.Create;

  Salvar1.Visible     := false;

  Transformada1.Visible    := false;
  Filtros1.Visible         := false;
  Salvar2.Visible          := false;
  SalvarImTF1.Visible      := false;
  Patrones1.Visible        := false;
  RuidoPeriodico1.Visible  := false;

  Panel2.Hide;
  Panel3.Hide;
  Panel4.Hide;
  CoolBar1.Bands[1].Visible := false;

  ToolButton7.visible := false;
  Bimag := false;
  Btdf  := false;
  Btdfi := false;

  gamma   := 1/2;
  p_ruido := 1;

end;

// Salida del programa
procedure TFFourier.Salida1Click(Sender: TObject);
begin
  Close;
end;

// Abre imagen para su procesamiento
procedure TFFourier.Abrir1Click(Sender: TObject);
var
  j    : integer;
  bim  : boolean;
begin
  if OpenPD.Execute then begin
    nom := OpenPD.FileName;
    ext := ExtractFileExt(nom);
    bim := false;

    if CompareStr(LowerCase(ext),'.jpg') = 0 then begin
      jBM.LoadFromFile(nom);
      BM.Assign(jBM);
      bim := true;
      ta := 'j';
    end;
    if (CompareStr(LowerCase(ext),'.bmp') = 0) then begin
       BM.LoadFromFile(nom);
       bim := true;
       ta := 'b';
    end;

    if bim then begin
      Nx := BM.Width;
      Ny := BM.Height;
      SB1.Panels[1].text := IntToStr(Nx)+'x'+IntToStr(Ny);
      pf := BM.PixelFormat;
      if pf = pf24bit then begin
        // completa columnas y renglones a múltiplos de 2^n
        j := 2;
        while ((Nx div j) > 0 ) do j := 2*j;
        if (Nx = (j div 2)) then j := j div 2;
        Mx := j;

        j := 2;
        while ((Ny div j) > 0 ) do j := 2*j;
        if (Ny = (j div 2)) then j := j div 2;
        My := j;

        // centro de las imágenes ajustadas
        xc := Mx div 2;
        yc := My div 2;

        // reporta nuevas dimensiones
        SB1.Panels[3].text := IntToStr(Mx)+'x'+IntToStr(My);

        // Convierte y muestra Imagen en tonos de gris
        //ConvGris(BM);

        // pasa el BitMap a su forma matricial
        SetLength(Mat0,Mx,My);
        Conv(BM,Mat0,Mx,My);

        // Muestra imagen con bordes negros en caso que haya crecido
        Conv(Mat0,BM,Mx,My);

        // Muestra Imagen de entrada
        Image1.Picture.Assign(BM);

        // Oculta opciones
        Salvar1.Visible     := false;
        Filtros1.Visible    := false;
        Inversa1.Visible    := false;
        ToolBar2.Visible    := false;

        // Muestra opciones
        Transformada1.Visible    := true;
        Directa1.Visible         := true;
        ToolButton7.visible      := true;
        Panel2.Visible           := false;
        Patrones1.Visible        := true;
        RuidoPeriodico1.Visible  := false;

        // Imagen cargada
        Bimag := true;
      end
      else ShowMessage('Profundidad en bits no soportada para el Proceso...SIC');
    end
    else  ShowMessage('Formato no soportado...SIC');
  end;
end;

// Salva Matriz con la TDF de la imagen procesada
procedure TFFourier.Salvar2Click(Sender: TObject);
var
  id           : file of real;
  Ny           : integer;
  rr,rMx,rMy   : real;
  nomt         : String;
  i,j          : integer;
begin
 if SaveD_TDF.Execute then begin
   nomt := SaveD_TDF.FileName;
   AssignFile(id,nomt);
   rewrite(id);

   // control del header
   rr := 2*PI;
   write(id,rr);

   rMx := Mx+0.0;
   rMy := My+0.0;
   write(id,rMx,rMy);

   Ny := My div 2 ;

   for i := 0 to Mx-1 do
     for j := 0 to Ny do
       write(id,Matcd[i][j].r, Matcd[i][j].i);

   closeFile(id);
 end;
end;

// Abre Matriz con la TDF de una imagen
procedure TFFourier.Abrir2Click(Sender: TObject);
var
  id              : file of real;
  rr,rc,rMx,rMy   : real;
  nomt            : String;
  i,j,Ny,i1,j1    : integer;
begin
  if OpenD_TDF.Execute then begin
    nomt := OpenD_TDF.FileName;
    AssignFile(id,nomt);
    reset(id);

    // control del header
    rr := 2*PI;
    read(id,rc);
    if (rc <> rr) then begin
     ShowMessage('Archivo con TDF corrupto ... SIC');
     closeFile(id);
     exit;
    end;

    // lectura de datos
    read(id,rMx,rMy);
    Mx := trunc(rMx);
    My := trunc(rMy);

    // dimensiona matriz Compleja con la TDF
    SetLength(Matcd,Mx,My);

    Ny := My div 2;

    for i := 0 to Mx-1 do
      for j := 0 to Ny do
        read(id,Matcd[i][j].r,Matcd[i][j].i);

    closeFile(id);

    // Completa la matriz por las simetrias
    // de almacenamiento en cada dirección

    // banda izquierda inferior
    for j := 1 to Ny-1 do begin
        j1 := My - j;
        Matcd[0][j1].r :=  Matcd[0][j].r;
        Matcd[0][j1].i := -Matcd[0][j].i;  // conjugado
    end;

    // parte inferior
    for j := 1 to Ny-1 do begin
      j1 := My - j;
      for i := 1 to Mx-1 do begin
        i1 := Mx - i;
        Matcd[i1][j1].r :=  Matcd[i][j].r;
        Matcd[i1][j1].i := -Matcd[i][j].i;   // conjugado
      end;
    end;

    // dimensiona el resto de matrices de trabajo
    SetLength(Mat1 ,Mx,My);
    SetLength(Matci,Mx,My);

    // Reporta resultados en la interfase
    SB1.Panels[1].text := IntToStr(Mx)+'x'+IntToStr(My);
    SB1.Panels[3].text := IntToStr(Mx)+'x'+IntToStr(My);

    // Muestra la imagen de la TDF
    MuestraTDF(VLn);

    // Ajusta barras para el filtrado
    AjustaLimites;

    // Muestra opciones
    Transformada1.Visible := true;
    Inversa1.Visible      := true;
    Filtros1.Visible      := true;
    Salvar2.Visible       := true;
    SalvarImTF1.Visible   := true;
    AjustaLimites;
    Panel2.Show;
    Panel4.Show;
    CoolBar1.Bands[1].Visible := true;

    // oculta opciones
    Directa1.Visible      := false;
  end;
end;

// Salva Imagen de la TDF
procedure TFFourier.SalvarImTF1Click(Sender: TObject);
var
  nom : String;
  bb  : boolean;
begin
  SavePictureDialog1.Title := 'Salvar Imagen de la TDF';
  if SavePictureDialog1.Execute then begin
    nom := SavePictureDialog1.FileName;
    bb := true;
    if FileExists(nom) then
       if MessageDlg('Desea Reescribir el Archivo ' + nom +' ?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes
         then bb := true
         else bb := false;

    if bb then Image2.Picture.SaveToFile(nom);
  end;
end;

// Salva Imagen Filtrada
procedure TFFourier.Salvar1Click(Sender: TObject);
var
  nom : String;
  bb  : boolean;
begin
  SavePictureDialog1.Title := 'Salvar Imagen Filtrada';
  if SavePictureDialog1.Execute then begin
    nom := SavePictureDialog1.FileName;
    bb := true;
    if FileExists(nom) then
       if MessageDlg('Desea Reescribir el Archivo ' + nom +' ?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes
         then bb := true
         else bb := false;

    if bb then Image3.Picture.SaveToFile(nom);
  end;
end;

{ ---------------------------- |.......................| -------------------- }
{ ---------------------------- | FFT Directa e Inversa | -------------------- }
{ ---------------------------- |.......................| -------------------- }

// ---------------------------- FFT Directa ---------------------------------

procedure TFFourier.Directa1Click(Sender: TObject);
var
  t1,t2      : TDateTime;
  tt1,tt2    : TTimeStamp;
  h1,h2,hh   : real;
begin
  // TDF Evaluada
  Btdf := true;

  // Recalcula tamaño de la imagen cargada por si se abrió una TDF
  // En el Image1 está la imagen a procesar

  Mx := Image1.Width;
  My := Image1.Height;

  // dimensionamiento de matrices de salida para visualización y procesamiento
  setLength(Mat1 ,Mx,My);
  setLength(Matcd,Mx,My);
  setLength(Matci,Mx,My);

  CopyMat(Mat0,Mat1,Mx,My);

  // Construye matriz compleja
  MatrizCompleja(Mat1,Matcd,Mx,My);

  // Reorganiza la matriz de datos para centrar la TDF
  ReOrdena(Matcd,Mx,My);

  // Toma la hora actual
  t1 := Now;

  // Evalua la transformada directa de Fourier de la imagen
  FFT2D(Matcd,Mx,My,1);

  t2 := Now;

  tt1 := DateTimeToTimeStamp(t1);
  tt2 := DateTimeToTimeStamp(t2);

  h1 := tt1.Time;
  h2 := tt2.Time;

  hh := (h2 - h1)/1000.0;
  SB1.Panels[5].text := FloatToStr(hh)+' seg' ;

  // Muestra la imagen de la TDF
  MuestraTDF(VLn);

  // muestra opciones
  Inversa1.Visible      := true;
  Filtros1.Visible      := true;
  Salvar2.Visible       := true;
  SalvarImTF1.Visible   := true;
  AjustaLimites;
  Panel2.Show;
  Panel4.Show;
  CoolBar1.Bands[1].Visible := true;
end;

// --------------------------- FFT Inversa ---------------------------------

procedure TFFourier.TDFInversa(Filtro : TFil);
var
  t1,t2      : TDateTime;
  tt1,tt2    : TTimeStamp;
  h1,h2,hh   : real;
  potencia   : Tpot;
begin
  t1 := Now;

  // Evalua la transformada inversa de Fourier de la imagen

  // Copia Matriz con la TDF de Matcd a Matci
  setLength(Matci,Mx,My);
  if Filtro = FNull then CopyMat(Matcd,Matci,Mx,My);

  FFT2D(Matci,Mx,My,-1);

  t2 := Now;

  tt1 := DateTimeToTimeStamp(t1);
  tt2 := DateTimeToTimeStamp(t2);

  h1 := tt1.Time;
  h2 := tt2.Time;

  hh := (h2 - h1)/1000.0;
  SB1.Panels[7].text := FloatToStr(hh)+' seg';

  // Grafica la image resultante en el tercer panel

  // elige forma de calcular la Mat. real
  potencia := PCompleja;
  if RadioButton2.Checked then potencia := PReal;

  MatrizNor(Matci, mat1, Mx,My, potencia);

  if potencia = Preal then ReOrdena(mat1,Mx,My);

  BM.Width  := Mx;
  BM.Height := My;
  BM.PixelFormat := pf24bit;

  BM.canvas.brush.Color:= clwhite;
  BM.canvas.pen.Color:= clwhite;
  BM.Canvas.Rectangle(0,0,Mx-1,My-1);

  Conv (mat1,BM,Mx,My);
  Image3.Picture.Assign(BM);

  Salvar1.Visible := true;
  Btdfi := true;
end;

// inversa simple
procedure TFFourier.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dentro : boolean;
begin
  if Bimag then begin
    dentro := (X>=0) and (X<=Mx) and (Y>=0) and (Y<=My);
    if dentro
      then Panel5.Caption := Format('( %d , %d )',[X,Y])
      else Panel5.Caption := '(X,Y)';
  end;
end;

procedure TFFourier.Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  PP     : real;
  dentro : boolean;
begin
  if Btdf then begin
    dentro := (X>=0) and (X<=Mx) and (Y>=0) and (Y<=My);
    if dentro then begin
      PP := sqr(matcd[X][Y].r) + sqr(matcd[X][Y].i);
      if CheckBox1.Checked then begin
        X := X-xc;
        Y := yc-Y;
      end;
      Panel6.Caption := Format('(%d , %d) : %10.6f',[X,Y,PP]);
    end
    else Panel6.Caption := '(X,Y) : P';
  end;
end;

procedure TFFourier.Image2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbright then begin
    FFCapZonas.StringGrid1.Cells[1,p_ruido] := IntToStr(X);
    FFCapZonas.StringGrid1.Cells[2,p_ruido] := IntToStr(Y);
    FFCapZonas.StringGrid1.Cells[3,p_ruido] := '1';
    FFCapZonas.StringGrid1.Cells[4,p_ruido] := 'S';
    inc(p_ruido);
    if (p_ruido>FFCapZonas.StringGrid1.RowCount)
      then p_ruido:=FFCapZonas.StringGrid1.RowCount;
    
  end;
end;

procedure TFFourier.Image3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dentro : boolean;
  TT     : byte;
begin
  if Btdfi then begin
    dentro := (X>=0) and (X<=Mx) and (Y>=0) and (Y<=My);
    if dentro then begin
      TT := mat1[X][Y];
      Panel7.Caption := Format('(%d , %d) : %d',[X,Y,TT]);
    end
    else Panel6.Caption := '(X,Y) : P';
  end;
end;

procedure TFFourier.Inversa1Click(Sender: TObject);
begin
  TDFInversa(FNull);
end;

{ --------------------------- FILTROS --------------------------------------- }

// Ajusta Limites de los TrackBar
procedure TFFourier.AjustaLimites;
var
  hip : integer;
begin
  hip := trunc (sqrt(sqr(Mx/2)+sqr(My/2)));

  TrackBar1.Max := hip;
  TrackBar2.Max := hip;
  TrackBar1.Position := 2;
  TrackBar2.Position := hip;

  Label6.Caption := '2:'+IntTostr(hip);
  Label7.Caption := IntToStr(hip)+':'+IntTostr(hip);
end;

// Cambio en TrackBar1
procedure TFFourier.TrackBar1Change(Sender: TObject);
var
  li,ls : integer;
begin
  li := TrackBar1.position;
  ls := TrackBar2.position;

  if li >= ls then begin
    li := ls-1;
    TrackBar1.position := li;
  end;

  Label6.Caption := IntToStr(li)+':'+IntToStr(TrackBar1.max);
end;

// Cambio en TrackBar2
procedure TFFourier.TrackBar2Change(Sender: TObject);
var
  li,ls : integer;
begin
  li := TrackBar1.position;
  ls := TrackBar2.position;

  if ls <= li then begin
    ls := li+1;
    TrackBar2.position := ls;
  end;

  Label7.Caption := IntToStr(ls)+':'+IntToStr(TrackBar2.max);
end;

{ -------------------------  FILTROS --------------------------- }
// Filtro Pasa Baja
procedure TFFourier.PasaBaja1Click(Sender: TObject);
var
  i,j,cx,cy,tt : integer;
  R2           : real;
  H            : RMatrix2D;
begin
  SetLength(H,Mx,My);

  R2 := sqr(TrackBar1.Position);
  cx := Mx shr 1;
  cy := My shr 1;

  for j := 0 to pred(My) do begin
    tt := sqr(j-cy);
    for i := 0 to pred(Mx) do
      if (sqr(i-cx) + tt) <= R2
        then H[i][j] := 1.0
        else H[i][j] := 0.0;
  end;

  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
        Matci[i][j] := ComplexProd(Matcd[i][j],H[i][j]);

  Salvar1.Visible := true;
  TDFInversa(FPBaja);
end;

// Filtro Pasa Alta
procedure TFFourier.PasaAlta1Click(Sender: TObject);
var
  i,j,cx,cy,tt : integer;
  R2           : real;
  H            : RMatrix2D;
begin
  SetLength(H,Mx,My);

  R2 := sqr(TrackBar2.Position);
  cx := Mx shr 1;
  cy := My shr 1;

  for j := 0 to pred(My) do begin
    tt := sqr(j-cy);
    for i := 0 to pred(Mx) do
      if (sqr(i-cx) + tt) < R2
        then H[i][j] := 0.0
        else H[i][j] := 1.0;
  end;

  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
        Matci[i][j] := ComplexProd(Matcd[i][j],H[i][j]);

  Salvar1.Visible := true;
  TDFInversa(FPAlta);
end;

// Filtro Pasa Banda
procedure TFFourier.PasaBanda1Click(Sender: TObject);
var
  i,j,cx,cy,tt : integer;
  R12,R22,R    : real;
  H            : RMatrix2D;
begin
  SetLength(H,Mx,My);

  R12 := sqr(TrackBar1.Position);
  R22 := sqr(TrackBar2.Position);
  cx := Mx shr 1;
  cy := My shr 1;

  for j := 0 to pred(My) do begin
    tt := sqr(j-cy);
    for i := 0 to pred(Mx) do begin
      R := sqr(i-cx) + tt;
      if ( R >= R12 ) and (R <= R22)
        then H[i][j] := 1.0
        else H[i][j] := 0.0;
    end;
  end;

  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
        Matci[i][j] := ComplexProd(Matcd[i][j],H[i][j]);

  Salvar1.Visible := true;
  TDFInversa(FPBanda);
end;

// Elimina Ruido en base a una serie de puntos
procedure TFFourier.Elimina_ruido();
var
  i,j          : integer;
  H            : RMatrix2D;

  k            : byte;
  ss           : string;
  lonss,err    : integer;
  op           : char;
  xk,yk,Lk     : integer;
  ii,jj        : integer;
begin
  // Dimensionamos Máscara
  SetLength(H,Mx,My);

  //  Llenamos la máscara con 1's
  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
      H[i][j] := 1.0;

  // Recogemos puntos a eliminar
  for k := 1 to FFCapZonas.StringGrid1.RowCount - 1 do begin
    ss := LowerCase(FFCapZonas.StringGrid1.Cells[4,k]);
    lonss := length(ss);
    if (lonss <> 0) then begin
      op:= ss[1];
      if (op='s') then begin
        val(FFCapZonas.StringGrid1.Cells[1,k],xk,err);
        if (err<>0) then continue;
        val(FFCapZonas.StringGrid1.Cells[2,k],yk,err);
        if (err<>0) then continue;
        val(FFCapZonas.StringGrid1.Cells[3,k],Lk,err);
        if (err<>0) then continue;

        for j := -Lk to Lk do begin
          jj := yk + j;
          for i := -Lk to Lk do begin
            ii := xk+i;
            H[ii][jj] := 0;
          end;
        end;

      end;
    end;
  end;

  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
        Matci[i][j] := ComplexProd(Matcd[i][j],H[i][j]);

  Salvar1.Visible := true;
  TDFInversa(FRuiPe);
end;

procedure TFFourier.Patrones1Click(Sender: TObject);
begin

end;

// Define propiedades del patrón
procedure TFFourier.Propiedades1Click(Sender: TObject);
begin
  FFPatron.ShowModal;
  if FFPatron.ModalResult=mrOK then begin
    BM.Assign(FFPatron.Image2.Picture.Bitmap);
    Image1.Picture.Assign(BM);
    Conv(BM,Mat0,Mx,My);

    RuidoPeriodico1.Visible  := true;
  end;
end;

procedure TFFourier.RuidoPeriodico1Click(Sender: TObject);
begin
  FFCapZonas.Show;
end;

// Filtro Pasa Baja de Butterworth
procedure TFFourier.ToolButton6Click(Sender: TObject);
begin
  Panel3.Show;

  Label8.Caption := 'Butterworth LP';

  with ComboBox1 do begin
    Items.Clear;
    Style := csOwnerDrawFixed;

    Items.Add('n = 1');
    Items.Add('n = 2');
    Items.Add('n = 3');
    Items.Add('n = 4');
    ItemIndex := 1;
  end;
end;

procedure TFFourier.SpeedButton1Click(Sender: TObject);
var
  n : byte;
begin
   n := ComboBox1.ItemIndex;
   FButterworth(sender, n);
end;

// Filtro de Butterworth
procedure TFFourier.FButterworth(Sender : TObject; k : byte);
var
  i,j,cx,cy,
  R0,p            : integer;
  x2,y2,z,t       : real;
  H               : RMatrix2D;
begin
  SetLength(H,Mx,My);

  R0 := TrackBar1.Position;
  cx := Mx shr 1;
  cy := My shr 1;
  p  := 2*k;

  for j := 0 to pred(My) do begin
    y2 := sqr(j - cy + 0.5);
    for i := 0 to pred(Mx) do begin
      x2 := sqr(i - cx + 0.5);
      z  := sqrt(x2+y2)/R0;
      t := power(z,p);
      H[i][j] := 1.0/(1+t);
    end;
  end;

  for j := 0 to pred(My) do
    for i := 0 to pred(Mx) do
        Matci[i][j] := ComplexProd(Matcd[i][j],H[i][j]);

  TDFInversa(FButter);
  Salvar1.Visible := true;
end;

{ ------------------------------------------------------------------------- }
{ ------------------------------ FIN -------------------------------------- }
{ ------------------------------------------------------------------------- }

end.
