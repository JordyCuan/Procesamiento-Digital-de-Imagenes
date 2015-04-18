// Sistema de desarrollo para la implementacion de
// Procesamiento Digital de iMgenes
// V 0.5.11
// 21 - 01 - 2015
// FCC BUAP

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants,System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.ExtDlgs,
  Vcl.ComCtrls, math, Vcl.StdCtrls,
  Jpeg, PNGImage, GIFImg, Vcl.ImgList, Vcl.ToolWin,

  UBase,UnitZoom, UParBin, UHisto, UPuntuales, URegionales, UGeometricos, UIntRotacion, UCalc,
  Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnColorMaps;


type
  TAppPDI = class(TForm)
    MainMenu1: TMainMenu;
    Archivos1: TMenuItem;
    Abrir1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    StatusBar3: TStatusBar;
    Editar1: TMenuItem;
    FiltrosPuntuales1: TMenuItem;
    Negativo1: TMenuItem;
    Gamma1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;

    // barra de botones
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ImageList2: TImageList;
    Interface1: TMenuItem;
    EstiloFCC1: TMenuItem;
    EstiloPunk1: TMenuItem;
    Varios1: TMenuItem;
    Histograma1: TMenuItem;
    Logaritmo1: TMenuItem;
    HacerDeshacer1: TMenuItem;
    Constante50501: TMenuItem;
    Porcentual50501: TMenuItem;

    FuncionSeno1: TMenuItem;
    FuncionExponencial1: TMenuItem;

    SavePictureDialog1: TSavePictureDialog;
    Guardar1: TMenuItem;
    Guardarcomo1: TMenuItem;
    CheckBox1_ROJO: TCheckBox;
    CheckBox2_VERDE: TCheckBox;
    CheckBox3_AZUL: TCheckBox;
    FuncionCoseno1: TMenuItem;
    ClaroOscuro1: TMenuItem;

    OscurecimientoFuerte1: TMenuItem;
    SenoidaInvertida1: TMenuItem;


    Image2Selec: TImage;
    Activarseleccion1: TMenuItem;
    ActivarSeleccionCir1: TMenuItem;
    Desactivarseleccion1: TMenuItem;
    FiltrosRegionales1: TMenuItem;
    FiltrosGeometricos1: TMenuItem;
    BlancoyNegro1: TMenuItem;
    BordesX1: TMenuItem;
    BordesY1: TMenuItem;
    BordesXY1: TMenuItem;
    BordesConvolucion1: TMenuItem;
    N1: TMenuItem;
    MediasConvulucion1: TMenuItem;
    N2: TMenuItem;
    MedianasConvolucion1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    RotacionIBL1: TMenuItem;
    FlipX1: TMenuItem;
    FlipY1: TMenuItem;
    Zoom2xF1: TMenuItem;
    Zoom2xP1: TMenuItem;
    ZoomVMC1: TMenuItem;
    ZoomIBL1: TMenuItem;
    RotacionVMC1: TMenuItem;
    Rotacion901: TMenuItem;
    Rotacion902: TMenuItem;
    Rotacion1801: TMenuItem;
    Mapeo1: TMenuItem;
    FalsoColor1: TMenuItem;
    AbrirPaleta1: TMenuItem;
    AplicarPaleta1: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    N5: TMenuItem;
    N6: TMenuItem;
    ActivarCalculadora1: TMenuItem;
    ToolButton7: TToolButton;
    ScrollBox2: TScrollBox;
    Image2: TImage;
    Image3calcu: TImage;
    PopupMenu1: TPopupMenu;
    CargarImagen1: TMenuItem;
    deArchivo1: TMenuItem;
    deCamara1: TMenuItem;
    ReglaAplicacion1: TMenuItem;
    EsquinaIS1: TMenuItem;
    SeleccPunto1: TMenuItem;
    N001: TMenuItem;
    Aritmeticas1: TMenuItem;
    Suma1: TMenuItem;
    Resta1: TMenuItem;
    Logicas1: TMenuItem;
    AND1: TMenuItem;
    OR1: TMenuItem;
    RadioGroup1: TRadioGroup;
    N7: TMenuItem;
    AbrirfotodesdeWebCam1: TMenuItem;
    Luminancia1: TMenuItem;
    Binarizacion1: TMenuItem;
    BinarizacionParametro1: TMenuItem;
    CPerfilTringular1: TMenuItem;
    ReAbrirOriginal1: TMenuItem;
    Recortar1: TMenuItem;
    N8: TMenuItem;
    BSCY1: TMenuItem;


    // Metodos
    procedure Abrir1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Negativo1Click(Sender: TObject);
    procedure Gamma1Click(Sender: TObject);
    procedure EstiloFCC1Click(Sender: TObject);
    procedure EstiloPunk1Click(Sender: TObject);
    procedure Histograma1Click(Sender: TObject);
    procedure Logaritmo1Click(Sender: TObject);
    procedure HacerDeshacer1Click(Sender: TObject);
    procedure Constante50501Click(Sender: TObject);
    procedure Porcentual50501Click(Sender: TObject);
    procedure FuncionSeno1Click(Sender: TObject);
    procedure FuncionExponencial1Click(Sender: TObject);
    procedure Guardar1Click(Sender: TObject);
    procedure Guardarcomo1Click(Sender: TObject);
    procedure CheckBox1_ROJOClick(Sender: TObject);
    procedure CheckBox2_VERDEClick(Sender: TObject);
    procedure CheckBox3_AZULClick(Sender: TObject);
    procedure FuncionCoseno1Click(Sender: TObject);
    procedure ClaroOscuro1Click(Sender: TObject);
    procedure OscurecimientoFuerte1Click(Sender: TObject);
    procedure SenoidaInvertida1Click(Sender: TObject);

    procedure Image2SelecMouseLeave(Sender: TObject);
    procedure Image2SelecMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Activarseleccion1Click(Sender: TObject);
    procedure ActivarSeleccionCir1Click(Sender: TObject);
    procedure Desactivarseleccion1Click(Sender: TObject);
    procedure Image2SelecMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftstate; X,Y: Integer);
    procedure Image2SelecMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BlancoyNegro1Click(Sender: TObject);
    procedure BordesX1Click(Sender: TObject);
    procedure BordesY1Click(Sender: TObject);
    procedure RotacionIBL1Click(Sender: TObject);

    procedure Rotacion901Click(Sender: TObject);
    procedure Rotacion902Click(Sender: TObject);
    procedure FlipX1Click(Sender: TObject);
    procedure FlipY1Click(Sender: TObject);
    procedure Rotacion1801Click(Sender: TObject);
    procedure Zoom2xF1Click(Sender: TObject);
    procedure Zoom2xP1Click(Sender: TObject);
    procedure MedianaSimple1Click(Sender: TObject);
    procedure BordesXY1Click(Sender: TObject);
    procedure AbrirPaleta1Click(Sender: TObject);
    procedure AplicarPaleta1Click(Sender: TObject);
    procedure ZoomIBL1Click(Sender: TObject);
    procedure ZoomVMC1Click(Sender: TObject);
    procedure ActivarCalculadora1Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure deArchivo1Click(Sender: TObject);
    procedure Suma1Click(Sender: TObject);
    procedure AND1Click(Sender: TObject);
    procedure OR1Click(Sender: TObject);
    procedure Resta1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);

    procedure Luminancia1Click(Sender: TObject);
    procedure Binarizacion1Click(Sender: TObject);
    procedure BinarizacionParametro1Click(Sender: TObject);
    procedure CPerfilTringular1Click(Sender: TObject);

    procedure ReAbrirOriginal1Click(Sender: TObject);
    procedure Recortar1Click(Sender: TObject);
    procedure BSCY1Click(Sender: TObject);

    procedure leer_Bordes(nom : string);
    procedure BordeXClick(Sender: TObject);

    procedure leer_Medias(nom : string);
    procedure MediaXClick(Sender: TObject);

    procedure leer_Medianas(nom : string);
    procedure MedianaXClick(Sender: TObject);





    // Añadidos por Jordy
    //procedure MatrizYB1Click(Sender: TObject);
    //procedure MetMediasGen(Sender: TObject);



  private
    { Private declarations }


    procedure Prepara();
    procedure Presenta();
  public
    { Public declarations }
    nomIma        : string;
    BM1, BMS      : TBitMap;
    valor         : single;
    Im1,Im2,ImC   : MatImg;
    nc, nr        : integer;
    MC1           : MatConv;

    MatBordes     : array of MatConv;
    MatMedias     : array of MatConvNM;
    MatMedianas   : array of MatConvNM;

  end;

var
  AppPDI: TAppPDI;

implementation

{$R *.dfm}

// Inicializacion de la Ventana
procedure TAppPDI.FormCreate(Sender: TObject);
var
  i : integer;
begin
  DecimalSeparator := '.'; // Nos ayudará a evitar problemas con el idioma

  StatusBar3.Panels[0].Text := 'Archivo';

  StatusBar2.Panels[0].Text := 'X';
  StatusBar2.Panels[1].Text := 'Y';

  StatusBar2.Panels[2].Text := 'R';
  StatusBar2.Panels[3].Text := 'G';
  StatusBar2.Panels[4].Text := 'B';

  Application.Icon.LoadFromFile('alien.ico');

  // Variables: Saleccion
  BM1 := TBitMap.Create;

  _banCir := false;
  _banRect := false;
  BMSel    := TBitmap.Create;
  _boolSeleccionando := false;

  // Llenado del canal
  for i := 0 to 2 do
    _kan[i] := true;

  // Inicialización de los Tabs
  TabSheet2.TabVisible := false;
  PageControl1.ActivePageIndex := 0;

  leer_Bordes('mBordes.dat');
  leer_medias('mMedias.dat');
  leer_Medianas('mMedianas.dat');

end;

// Leer matrices de Bordes
procedure TAppPDI.leer_Bordes(nom : string);
var
  id     : TextFile;
  nomF   : String;
  NF,kk,
  n,m,
  i,j    : integer;

  hijo   : TMenuItem;

begin
  AssignFile(id,nom);
  reset(id);

  // leer en número de Máscaras
  readln(id,NF);
  SetLength(MatBordes,NF);

  for kk := 0 to NF - 1 do begin

    hijo := TMenuItem.Create(self);
    readln(id,nomF);
    hijo.Name    := nomF;
    hijo.Caption := nomF;
    hijo.OnClick := BordeXClick;

    BordesConvolucion1.Add(hijo);

    readln(id,n,m);
    SetLength(MatBordes[kk].dat, n, m);
    MatBordes[kk].nc := n;
    MatBordes[kk].nr := m;

    for j := 0 to 2 do begin
      for i := 0 to 2 do
        read(id, MatBordes[kk].dat[i][j]);
      readln(id);
    end;

  end;

  closeFile(id);
end;

// Auxiliar para definir el tipo de Borde a usar cuando se hace click
procedure TAppPDI.BordeXClick(Sender: TObject);
var
  opc   : integer;
  item  : TMenuItem;

begin
  opc  := BordesConvolucion1.IndexOf(Sender as TMenuItem);

  if CanalPrendido then begin
    Prepara();
    fr_BordeConX(Im1, MatBordes[opc], Im2);
    Presenta();
  end;
end;

// Leer Matrices de medias
procedure TAppPDI.leer_Medias(nom : string);
var
  id     : TextFile;
  nomF   : String;
  NF,kk,
  n,m,
  i,j    : integer;
  hijo   : TMenuItem;
begin
  AssignFile(id,nom);
  reset(id);

  // leer en número de Máscaras
  readln(id,NF);
  SetLength(MatMedias,NF);

  for kk := 0 to NF - 1 do begin

    hijo := TMenuItem.Create(self);
    readln(id,nomF);
    hijo.Name    := nomF;
    hijo.Caption := nomF;
    hijo.OnClick := MediaXClick;

    MediasConvulucion1.Add(hijo);

    readln(id,n,m);
    MatMedias[kk].nc := n;
    MatMedias[kk].nr := m;
    SetLength(MatMedias[kk].dat, n,m);

    for j := 0 to m-1 do begin
      for i := 0 to n-1 do
        read(id, MatMedias[kk].dat[i][j]);
      readln(id);
    end;
    readln(id,MatMedias[kk].fac);

  end;

  closeFile(id);
end;

// Auxiliar para definir el tipo de Media a usar cuando se hace click
procedure TAppPDI.MediaXClick(Sender: TObject);
var
  opc   : integer;
  item  : TMenuItem;

begin
  opc  := MediasConvulucion1.IndexOf(Sender as TMenuItem);

  if CanalPrendido then begin
    Prepara();
    fr_MediaConX(Im1,MatMedias[opc],Im2);
    Presenta();
  end;
end;

// Leer matrices de Medianas
procedure TAppPDI.leer_Medianas(nom : string);
var
  id     : TextFile;
  nomF   : String;
  totFilt,kk,
  n,m,
  i,j    : integer;

  hijo   : TMenuItem;
begin
  AssignFile(id,nom);
  reset(id);

  // leer el número total
  readln(id,totFilt);
  SetLength(MatMedianas,totFilt);

  for kk := 0 to totFilt - 1 do begin

    hijo := TMenuItem.Create(self);
    readln(id,nomF);
    hijo.Name    := nomF;
    hijo.Caption := nomF;
    hijo.OnClick := MedianaXClick;

    MedianasConvolucion1.Add(hijo);

    readln(id,n,m);
    MatMedianas[kk].nc := n;
    MatMedianas[kk].nr := m;
    SetLength(MatMedianas[kk].dat, n,m);

    for i := 0 to n-1 do begin
      for j := 0 to m-1 do
        read(id, MatMedianas[kk].dat[i][j]);
      readln(id);
    end;
    readln(id,MatMedianas[kk].fac);
  end;

  closeFile(id);
end;

// Auxiliar para definir el tipo de Mediana a usar cuando se hace click
procedure TAppPDI.MedianaXClick(Sender: TObject);
var
  opc   : integer;
begin
  opc  := MedianasConvolucion1.IndexOf(Sender as TMenuItem);

  if CanalPrendido then begin
    Prepara();
    fr_MedianaX(Im1, MatMedianas[opc], Im2);
    Presenta();
  end;
end;




// ************ Ajuste de las banderas de seleccion segun el cambio
procedure TAppPDI.Activarseleccion1Click(Sender: TObject);
begin
  if _banRect = False then begin // Con esto parchamos un error de clonar imagen
    _banRect := true;
    _banCir  := false;
    StatusBar1.Panels[6].Text := 'Activa';

    // Si lo primero que hace el usuario despues de abierta la imagen es la seleccion
    Mat2Mat(Im1,Im2);
  end;
end;
procedure TAppPDI.ActivarSeleccionCir1Click(Sender: TObject);
begin
  if _banCir = False then begin // Con esto parchamos un error de clonar imagen
    _banCir := true;
    _banRect:= false;
    StatusBar1.Panels[6].Text := 'Activa';

    // Si lo primero que hace el usuario despues de abierta la imagen es la seleccion
    Mat2Mat(Im1,Im2);
  end;
end;
procedure TAppPDI.Desactivarseleccion1Click(Sender: TObject);
begin
  _banRect := false;
  _banCir  := false;
  StatusBar1.Panels[6].Text := 'No activa';

  _x1 := 0      ; _y1 := 0;
  _x2 := Im1.nc ; _y2 := Im1.nr;

  BMSel.Canvas.Pen.Color := clWhite;
  BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
  Image2Selec.Picture.Assign(BMSel);
end;




// *****************************************************
// ******************** SELECCIONES ********************
// *****************************************************
procedure TAppPDI.Image2SelecMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if _banRect and (Button = mbLeft) then begin
    _x1 := X;
    _y1 := Y;

    _boolSeleccionando := true;
  end;

  // Circular
  if _banCir and (Button = mbLeft) then begin
    _x1 := X;
    _y1 := Y;

    _boolSeleccionando := true;
  end;
end;

procedure TAppPDI.Image2SelecMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (_banRect and (Button = mbLeft)) then begin
    if X < _x1 then begin
      _x2 := _x1;
      _x1 := X
    end
    else begin
      _x2 := X;
    end;

    if Y < _y1 then begin
      _y2 := _y1;
      _y1 := Y
    end
    else begin
      _y2 := Y;
    end;


    // Si el usuario se sale del margen de la imagen mientras selecciona
    // se puede sobre pasar el valor de la imagen, ENTONCES:
    if BM1.Width < _x2 then
      _x2 := BM1.Width;
    if BM1.Height < _y2 then
      _y2 := BM1.Height;
    if 0 > _x1 then
      _x1 := 0;
    if 0 > _y1 then
      _y1 := 0;


    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Rectangle(_x1,_y1,_x2,_y2);
    Image2Selec.Picture.Assign(BMSel);

    _boolSeleccionando := false;
  end;


  // Circular
  if (_banCir and (Button = mbLeft)) then begin
    if X < _x1 then begin
      _x2 := _x1;
      _x1 := X
    end
    else begin
      _x2 := X;
    end;

    if Y < _y1 then begin
      _y2 := _y1;
      _y1 := Y
    end
    else begin
      _y2 := Y;
    end;

    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Ellipse(_x1,_y1,_x2,_y2);
    Image2Selec.Picture.Assign(BMSel);

    _boolSeleccionando := false;
  end;
end;
// *****************************************************
// ************************ FIN ************************
// ******************** SELECCIONES ********************
// *****************************************************



// *****************************************************
// ********************** RECORTAR *********************
// *****************************************************
procedure TAppPDI.Recortar1Click(Sender: TObject);
var
  Mt : MatImg;
  x, y, c, xi, yi,
  nnx,nny : integer;
begin
  if _banRect = True then begin
    Mat2Mat(Im2,Im1);

    nnx := _x2 - _x1;
    nny := _y2 - _y1;


    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
    Image2Selec.Picture.Assign(BMSel);

    //Cambio de tamaño
    BMSel.Width := nnx;
    BMSel.Height := nny;

    Mt.nc := nnx;
    Mt.nr := nny;
    SetLength(Mt.dat, nnx, nny, 3);
  
    for x := 0 to nnx - 1 do
      for y := 0 to nny - 1 do begin
        for c := 0 to 2 do begin
          Mt.dat[x, y, c] := Im2.dat[x + _x1, y + _y1, c];
        end;
      end;

    Im2.nc := nnx;
    Im2.nr := nny;
    SetLength(Im2.dat, nnx, nny, 3);
    
    _x1 := 0;     _y1 := 0;
    _x2 := nnx;    _y2 := nny;

    Mat2Mat(Mt, Im2);

    Mt.nc := 0;
    Mt.nr := 0;
    SetLength(Mt.dat, 0,0,0);
  
    
    Presenta();
  end;
end;




// *****************************************************
// ********************** CANALES **********************
// *****************************************************
procedure TAppPDI.CheckBox1_ROJOClick(Sender: TObject);
var ROJO : integer;
begin
  ROJO := 0;
  _kan[ROJO] := CheckBox1_ROJO.Checked;
end;

procedure TAppPDI.CheckBox2_VERDEClick(Sender: TObject);
var VERDE : integer;
begin
  VERDE := 1;
  _kan[VERDE] := CheckBox2_VERDE.Checked;
end;

procedure TAppPDI.CheckBox3_AZULClick(Sender: TObject);
var AZUL : integer;
begin
  AZUL := 2;
  _kan[AZUL] := CheckBox3_AZUL.Checked;
end;
// *****************************************************
// ************************ FIN ************************
// ********************** CANALES **********************
// *****************************************************



// *****************************************************
// ******************** CAMBIAR TEMA *******************
// *****************************************************
procedure TAppPDI.EstiloFCC1Click(Sender: TObject);
begin
  ToolBar1.Images := ImageList1;
end;

procedure TAppPDI.EstiloPunk1Click(Sender: TObject);
begin
  ToolBar1.Images := ImageList2;
end;

// *****************************************************
// ******************** HISTOGRAMA *********************
// *****************************************************
procedure TAppPDI.Histograma1Click(Sender: TObject);
begin
  // Llamar a la interface del Histograma
  FormHisto.show;
end;



// *****************************************************
// ******************** ABRIR IMAGEN *******************
// *****************************************************
procedure TAppPDI.Abrir1Click(Sender: TObject);
var
  pic : TPicture;
begin
  // Invocar a un Manejador de Archivos
  if OpenPictureDialog1.Execute then begin
    nomIma := OpenPictureDialog1.FileName;
    pic := TPicture.Create;

    _banRect := false;
    _banCir  := false;

    StatusBar1.Panels[6].Text := 'No activa';

    try
      pic.LoadFromFile(nomIma);

      BM1.Width  := pic.Width;
      BM1.Height := pic.Height;
      BM1.Canvas.Draw(0,0,pic.Graphic);
      Image1.Picture.Assign(BM1);

      BMP2Mat(BM1,Im1);
      Im2.nc := 0; Im2.nr := 0;

      _x1 := 0      ; _y1 := 0;
      _x2 := Im1.nc ; _y2 := Im1.nr;

      StatusBar3.Panels[1].Text := nomIma;


      // Actualizamos el tam del ImageSelec
      Image2Selec.Width := _x2;
      Image2Selec.Height := _y2;

      // dimensionamos el BItMap de Seleccion
      BMSel.Width  := Image1.Width;
      BMSel.Height := Image1.Height;

      BMSel.Transparent := true;
      BMSel.TransparentColor := clWhite;
      BMSel.Canvas.Pen.Color := clWhite;
      BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
      //BMSel.Canvas.Pen.Color := clBlack;
      //PlumaSel := clBlack;

      // Al ImageSel lo hacemos autoSize , en diseño
      // y también Transparente

      // Le asignamos el BMSel
      Image2Selec.Picture.Assign(BMSel);
    finally
      pic.Free;
    end;

  end;
end;


// *****************************************************
// ********************** RE ABRIR *********************
// *****************************************************
procedure TAppPDI.ReAbrirOriginal1Click(Sender: TObject);
var
  pic : TPicture;
begin
  nomIma := OpenPictureDialog1.FileName;

  // Verifiquemos que ya se haya abierto algo
  if nomIma='' then
    exit;
  
  pic := TPicture.Create;

  _banRect := false;
  _banCir  := false;

  StatusBar1.Panels[6].Text := 'No activa';

  try
    pic.LoadFromFile(nomIma);

    BM1.Width  := pic.Width;
    BM1.Height := pic.Height;
    BM1.Canvas.Draw(0,0,pic.Graphic);
    Image1.Picture.Assign(BM1);

    BMP2Mat(BM1,Im1);
    Im2.nc := 0; Im2.nr := 0;

    _x1 := 0      ; _y1 := 0;
    _x2 := Im1.nc ; _y2 := Im1.nr;

    StatusBar3.Panels[1].Text := nomIma;


    // Actualizamos el tam del ImageSelec
    Image2Selec.Width := _x2;
    Image2Selec.Height := _y2;

    // dimensionamos el BItMap de Seleccion
    BMSel.Width  := Image1.Width;
    BMSel.Height := Image1.Height;

    BMSel.Transparent := true;
    BMSel.TransparentColor := clWhite;
    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);

    Image2Selec.Picture.Assign(BMSel);
  finally
    pic.Free;
  end;
end;


// *****************************************************
// ********************** GUARDAR **********************
// *****************************************************
procedure salvar_Imagen(BB : TBitmap; nombre : string);
var
  ext  : string;
  fjpg : TJPEGImage;
  fgif : TGIFImage;
  fpng : TPngImage;
begin

  // ********* Extension no dada *********
  if not (pos('.', nombre) > 1) then begin
    nombre := nombre + '.jpg';
  end;


  // ********* JPEG *********
  if (pos('.jpg', nombre) > 1) or (pos('.jpeg', nombre) > 1) then begin
    // create the jpeg-graphic
    fjpg := TJPEGImage.Create;
    // assign the bitmap to the jpeg, this converts the bitmap
    fjpg.Assign(BB);
    // and save it to file
    fjpg.SaveToFile(nombre);
  end;


  // ********* GIF *********
  if (pos('.gif', nombre) > 1) then begin
    // create the jpeg-graphic
    fgif := TGIFImage.Create;
    // assign the bitmap to the jpeg, this converts the bitmap
    fgif.Assign(BB);
    // and save it to file
    fgif.SaveToFile(nombre);
  end;

  // ********* PNG *********
  if (pos('.png', nombre) > 1) then begin
    // create the jpeg-graphic
    fpng := TPNGImage.Create;
    // assign the bitmap to the jpeg, this converts the bitmap
    fpng.Assign(BB);
    // and save it to file
    fpng.SaveToFile(nombre);
  end;

end;

// Guardar la imagen
procedure TAppPDI.Guardar1Click(Sender: TObject);
begin
  salvar_Imagen(bm1, nomIma);
end;


// *****************************************************
// ******************** GUARDAR COMO *******************
// *****************************************************
procedure TAppPDI.Guardarcomo1Click(Sender: TObject);
begin
  SavePictureDialog1.FileName := nomIma;
  if SavePictureDialog1.Execute then begin
    nomIma := SavePictureDialog1.FileName;
    salvar_Imagen(BM1, nomIma);
  end;
end;

// Aviso fuera de imagen
procedure TAppPDI.Image2SelecMouseLeave(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '??';
  StatusBar1.Panels[1].Text := '??';

  StatusBar1.Panels[2].Text := '??';
  StatusBar1.Panels[3].Text := '??';
  StatusBar1.Panels[4].Text := '??';
end;

// Informa (X,Y) y RGB del pixel dentro de la Imagen
procedure TAppPDI.Image2SelecMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  r,g,b : byte;
  pix   : integer;
begin
  StatusBar1.Panels[0].Text := IntToStr(X);
  StatusBar1.Panels[1].Text := IntToStr(Y);

  pix := BM1.Canvas.Pixels[X,Y];
  r := GetRValue(pix);
  g := GetGValue(pix);
  b := GetBValue(pix);

  StatusBar1.Panels[2].Text := IntToStr(r);
  StatusBar1.Panels[3].Text := IntToStr(g);
  StatusBar1.Panels[4].Text := IntToStr(b);

  if ((ssLeft in Shift) and _boolSeleccionando) and _banRect then begin
    // Borramos el anterior
    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
    Image2Selec.Picture.Assign(BMSel);

    // Dibujamos el temporal
    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Rectangle(_x1,_y1,X,Y);
    Image2Selec.Picture.Assign(BMSel);
  end;

  // Circular
  if ((ssLeft in Shift) and _boolSeleccionando) and _banCir then begin
    // Borramos el anterior
    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
    Image2Selec.Picture.Assign(BMSel);

    // Dibujamos el temporal
    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Ellipse(_x1,_y1,X,Y);
    Image2Selec.Picture.Assign(BMSel);
  end;
end;


// *****************************************************
// ******************* HACER-DESHACER ******************
// *****************************************************
// Siempre que haya un proceso previo una imagen
procedure TAppPDI.HacerDeshacer1Click(Sender: TObject);
var
 Mtemp : MatImg;
begin
  // validamos si hubo proceso
  if (Im2.nc+Im2.nr) <> 0 then begin
    Mat2Mat (Im1  , Mtemp);
    Mat2Mat (Im2  , Im1);
    Mat2Mat (Mtemp, Im2);
    SetLength(Mtemp.dat,1,1,1);

    Presenta();

    // Hay un bug muy escondido que surge cuando se selecciona una imagen,
    // luego se recorta, luego se hace UNDO y luego se aplica un filtro
    _x1 := 0;
    _y1 := 0;
    _x2 := BM1.Width;
    _y2 := BM1.Height;
    _banCir := False;
    _banRect := False;
    StatusBar1.Panels[6].Text := 'No activa';

    // Limpia
    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
    Image2Selec.Picture.Assign(BMSel);
  end;
end;

// Prepara para procesar una imagen en forma de matriz
procedure TAppPDI.Prepara();
begin
  // Si Im2 no esta vacia = si ha habido un proceso previo
  // Copiamos Im2 en Im1
  if (Im2.nc+Im2.nr) <> 0 then
    Mat2Mat(Im2,Im1);

  // descomponer pixel  pixel e invertir
  // en cada canal 255 - z

  nc := Im1.nc;
  nr := Im1.nr;

  // inicializar Im2 al mismo tamaño de Im1
  Im2.nc := nc;
  Im2.nr := nr;
  Setlength(Im2.dat,nc,nr,3);
end;

// Presentan el resultado del proceso en pantalla
procedure TAppPDI.Presenta();
begin
  Mat2BMP(Im2,BM1);
  Image1.Picture.Assign(BM1);
end;



// *****************************************************
// ******************** F PUNTUALES ********************
// *****************************************************

// Negativo usando el BitMap: BM1
procedure TAppPDI.Negativo1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_negativo(Im1,Im2);
    Presenta();
  end;
end;



// Correccion Gamma
procedure TAppPDI.Gamma1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    valor := StrToFloat(Edit1.Text);

    Prepara();
    fp_gamma(Im1,Im2,valor);
    Presenta();
  end;
end;

// amplificacion logaritmica
procedure TAppPDI.Logaritmo1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_logaritmo(Im1,Im2);
    Presenta();
  end;
end;

procedure TAppPDI.Luminancia1Click(Sender: TObject);
begin
  Prepara();
  fp_Luminancia(Im1,Im2);
  Presenta();

end;

procedure TAppPDI.MedianaSimple1Click(Sender: TObject);
begin
                              //
end;

//Funcion Seno
procedure TAppPDI.FuncionSeno1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_seno(Im1,Im2);
    Presenta();
  end;
end;

//Funcion Coseno
procedure TAppPDI.FuncionCoseno1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_coseno(Im1,Im2);
    Presenta();
  end;
end;

//Funcion Oscurecimiento Fuerte
procedure TAppPDI.OscurecimientoFuerte1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_OscFuerte(Im1,Im2,StrToFloat(Edit1.Text));
    Presenta();
  end;
end;

//Funcion Exponencial
procedure TAppPDI.FuncionExponencial1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_exponencial(Im1,Im2,StrToFloat(Edit1.Text));
    Presenta();
  end;
end;

//Funcion Senoidal Invertida para  Contraste
procedure TAppPDI.SenoidaInvertida1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fp_Senoidal(Im1,Im2,StrToFloat(Edit1.Text));
    Presenta();
  end;
end;


//Funcion TangenteHiperbolica Claro-Oscuro
procedure TAppPDI.ClaroOscuro1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    valor := StrToFloat(Edit1.Text);
    Prepara();
    fp_claroOscuro(Im1,Im2,valor);
    Presenta();
  end;
end;

procedure TAppPDI.Binarizacion1Click(Sender: TObject);
begin
  Prepara();
  fp_Binarizacion(Im1,Im2);
  Presenta();

end;

procedure TAppPDI.BinarizacionParametro1Click(Sender: TObject);
var
u,var_Error:integer;
begin
  Form3.ShowModal;
    if Form3.ModalResult <> mrOk then
      Exit;

      //Validacion

      val(Form3.Edit1.Text,u,var_Error);

        if var_Error <> 0 then begin
          ShowMessage('Error de Entrada de Datos');
          Exit;
        end;
        Prepara();
        fp_BinarizacionPar(Im1,Im2,u);
        Presenta();
end;

procedure TAppPDI.CPerfilTringular1Click(Sender: TObject);
//Debe de Recibir un Valor del TexBox
begin
valor := StrToFloat(Edit1.Text);
  Prepara();
  fp_PerfilTriangular(Im1,Im2,valor);
  Presenta();

end;

procedure TAppPDI.BlancoyNegro1Click(Sender: TObject);
begin
  // No es necesaria la parte del CANAL del color
  Prepara();
  fp_blancoNegro(Im1,Im2);
  Presenta();
end;

// aditivo
procedure TAppPDI.Constante50501Click(Sender: TObject);
begin
  if CanalPrendido then begin
    valor := StrToFloat(Edit1.Text);

    if abs(valor)>=50 then begin
      ShowMessage('Fuera de rango, lea !!!');
      exit;
    end;

    Prepara();
    fp_constante(Im1,Im2, valor);
    Presenta();
  end;
end;


// porcentual
procedure TAppPDI.Porcentual50501Click(Sender: TObject);
begin
  if CanalPrendido then begin
    valor := StrToFloat(Edit1.Text);

    if abs(valor)>=50 then begin
      ShowMessage('Fuera de rango, lea !!!');
      exit;
    end;

    Prepara();
    fp_porcentual (Im1,Im2, valor);
    Presenta();
  end;
end;




// Borde Simple Y con convoluciones
(*procedure TAppPDI.MatrizYB1Click(Sender : TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fr_BSCY(im1, _MC1, im2);
    Presenta();
  end;
end;
*)


(*procedure TAppPDI.MetMediasGen(Sender: TObject);
var
  opc: integer;
begin
  opc := MediasConvolucion1.IndexOf(Sender as TMenuItem);


  if CanalPrendido then begin
    Prepara();
    fr_MediasC(Im1, _MCGM[opc], Im2);
    Presenta();
  end;

end;
*)


// *****************************************************
// ******************** F REGIONALES *******************
// *****************************************************

// Obserbador de tipo de borde (Val Absoluto / Repujado)
procedure TAppPDI.RadioGroup1Click(Sender: TObject);
begin
  _Norma := RadioGroup1.ItemIndex;
end;

// Borde simple en X
procedure TAppPDI.BordesX1Click(Sender: TObject);
begin
  //if CanalPrendido then begin
    Prepara();
    fr_BSX(im1, im2);
    Presenta();
  //end;
end;

// Borde simple en Y
procedure TAppPDI.BordesY1Click(Sender: TObject);
begin
  //if CanalPrendido then begin
    Prepara();
    fr_BSY(im1, im2);
    Presenta();
  //end;
end;


// Borde Simple en XY
procedure TAppPDI.BordesXY1Click(Sender: TObject);
begin
  //if CanalPrendido then begin
    Prepara();
    fr_BSXY(im1, im2);
    Presenta();
  //end;
end;


// Bordes Simples con Convolucion en Y
procedure TAppPDI.BSCY1Click(Sender: TObject);
begin
  //if CanalPrendido then begin
    Prepara();
    fr_BSCY(im1, _MC1Y, im2);
    Presenta();
  //end;
end;




// *****************************************************
// ******************* F GEOMETRICOS *******************
// *****************************************************
procedure TAppPDI.RotacionIBL1Click(Sender: TObject);
var
  ang : single;
begin
  FormRot.ShowModal;

  if FormRot.ModalResult = mrOK then begin
    ang := FormRot.angRot;

    Prepara();
    fg_rotaIBL(im1, im2, ang);
    Presenta();
  end;
end;

procedure TAppPDI.ZoomVMC1Click(Sender: TObject);
var
  nx,ny: integer;
begin
  Form2.Edit1.Text:=IntToStr(Im1.nc);
  Form2.Edit2.Text:=IntToStr(Im1.nr);
  Form2.Label5.Caption:=Format('%d x %d',[Im1.nc,Im1.nr]);
  Form2.ShowModal;
  if(Form2.ModalResult=mrOk)then begin
    nx := StrToInt(Form2.Edit1.Text);
    ny := StrToInt(Form2.Edit2.Text);

    Prepara();
    fg_zoomVC(Im1,Im2,nx,ny);
    Presenta();
  end;
end;

procedure TAppPDI.ZoomIBL1Click(Sender: TObject);
var
  nx,ny : integer;
begin
  Form2.Edit1.Text:=IntToStr(Im1.nc);
  Form2.Edit2.Text:=IntToStr(Im1.nr);
  Form2.Label5.Caption:=Format('%d x %d',[Im1.nc,Im1.nr]);
  Form2.ShowModal;
    if(Form2.ModalResult=mrOk) then begin
       nx:=StrToInt(Form2.Edit1.Text);
       ny:=StrToInt(Form2.Edit2.Text);
      //Los parametros que se le pasan, son lo que se recibiran de la Interfaz
        Prepara();
        fg_zoomIBL(Im1,Im2,nx,ny);
        Presenta();
end;
end;

//Reflexion en X
procedure TAppPDI.FlipX1Click(Sender: TObject);
begin
  Prepara();
  fg_flipX(Im1,Im2);
  Presenta();
end;


//Reflexion en Y
procedure TAppPDI.FlipY1Click(Sender: TObject);
begin
  Prepara();
  fg_flipY(Im1,Im2);
  Presenta();
end;



procedure TAppPDI.Rotacion901Click(Sender: TObject);
begin
  Prepara();
  fg_rotaMas90(im1,im2);
  Presenta();
end;



procedure TAppPDI.Rotacion902Click(Sender: TObject);
begin
  Prepara();
  fg_rotaMenos90(im1,im2);
  Presenta();
end;

procedure TAppPDI.Rotacion1801Click(Sender: TObject);
begin
  Prepara();
  fg_rota180(im1,im2);
  Presenta();

end;

//Zoom del Flojo
procedure TAppPDI.Zoom2xF1Click(Sender: TObject);
begin
  Prepara();
  fg_zoomF2x(Im1,Im2);
  Presenta();
end;

procedure TAppPDI.Zoom2xP1Click(Sender: TObject);
begin
  Prepara();
  fg_zoom2x(Im1,Im2);
  Presenta();
end;







// *****************************************************
// ******************** FALSO COLOR ********************
// *****************************************************

// Abrir una paleta de colores
procedure TAppPDI.AbrirPaleta1Click(Sender: TObject);
var
  k   : integer;
  nom : string;
  fid : TextFile;
begin
  if OpenDialog1.Execute then begin
    nom := OpenDialog1.FileName;
    AssignFile(fid, nom);
    Reset(fid);

    // Saltar 6 renglones
    for k := 1 to 6 do
      readln(fid);

    // leer las 256 tercias RGB
    for k := 0 to 255 do
      readln(fid, _Paleta[k][0], _Paleta[k][1], _Paleta[k][2]);

    closeFile(fid);
  end;
end;


// Aplicar una paleta de colores
procedure TAppPDI.AplicarPaleta1Click(Sender: TObject);
var
  x,y,v,tono : integer;
  c: Integer;
begin
  Prepara();

  for y := _y1 to _y2-1 do begin
    for x := _x1 to _x2-1 do begin
      tono := ajusta255(Im1.dat[x][y][0]);
      for c := 0 to 2 do begin
        Im2.dat[x][y][c] := _Paleta[tono][c];
      end;
    end;
  end;

  Presenta();
end;



// *****************************************************
// ******************** CALCULADORA ********************
// *****************************************************
procedure TAppPDI.ActivarCalculadora1Click(Sender: TObject);
begin
  if not ActivarCalculadora1.Checked then begin
    TabSheet2.TabVisible := true;
    ActivarCalculadora1.Checked := true;
    PageControl1.ActivePageIndex := 1;
  end
  else begin
    TabSheet2.TabVisible := false;
    ActivarCalculadora1.Checked := false;
  end;
end;

procedure TAppPDI.ToolButton7Click(Sender: TObject);
begin
  ActivarCalculadora1.Click;
end;

// Abrir archivo para la calculadora
procedure TAppPDI.deArchivo1Click(Sender: TObject);
var
  pic     : TPicture;
  nomIma  : String;
begin
  if OpenPictureDialog1.Execute then begin
    nomIma := OpenPictureDialog1.FileName;
    pic := TPicture.Create;

    try
      pic.LoadFromFile(nomIma);

      BM1.Width := pic.Width;
      BM1.Height:= pic.Height;
      BM1.Canvas.Draw(0,0,pic.Graphic);

      Image3calcu.Width := BM1.Width;
      Image3calcu.Height := BM1.Height;
      Image3calcu.Picture.Assign(BM1);

      _x2 := BM1.Width;
      _y2 := BM1.Height;

      BMP2Mat(BM1, ImC);
    finally
      pic.Free
    end;
  end;
end;

// Sumar imagenes
procedure TAppPDI.Suma1Click(Sender: TObject);
begin
  Prepara();
  Calc_Suma(Im1, ImC, Im2);
  Presenta();
  PageControl1.ActivePageIndex := 0;
end;

// Restar imagenes
procedure TAppPDI.Resta1Click(Sender: TObject);
begin
  Prepara();
  Calc_Resta(Im1, ImC, Im2);
  Presenta();
  PageControl1.ActivePageIndex := 0;
end;

// AND Logico en imagenes
procedure TAppPDI.AND1Click(Sender: TObject);
begin
  Prepara();
  Calc_AND(Im1, ImC, Im2);
  Presenta();
  PageControl1.ActivePageIndex := 0;
end;

// OR Logico en imagenes
procedure TAppPDI.OR1Click(Sender: TObject);
begin
  Prepara();
  Calc_OR(Im1, ImC, Im2);
  Presenta();
  PageControl1.ActivePageIndex := 0;
end;




end.









