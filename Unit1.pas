// Sistema de desarrollo para la implementacion de
// Procesamiento Digital de iMgenes
// V 0.5.8
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

  UBase, UHisto, UPuntuales, URegionales;

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
    MedianaSimple1: TMenuItem;
    MedianasConvolucion1: TMenuItem;

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
    procedure Image2SelecMouseLeave(Sender: TObject);
    procedure Image2SelecMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Activarseleccion1Click(Sender: TObject);
    procedure ActivarSeleccionCir1Click(Sender: TObject);
    procedure Desactivarseleccion1Click(Sender: TObject);
    procedure Image2SelecMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image2SelecMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BlancoyNegro1Click(Sender: TObject);
    procedure BordesX1Click(Sender: TObject);
    procedure BordesY1Click(Sender: TObject);


    // Añadidos por Jordy
    //procedure BSX1Click(Sender: TObject);
    //procedure BSY1Click(Sender: TObject);
    //procedure MatrizYB1Click(Sender: TObject);
    //procedure MetMediasGen(Sender: TObject);



  private
    { Private declarations }

    procedure Prepara();
    procedure Presenta();
  public
    { Public declarations }
    nomIma   : string;
    BM1, BMS : TBitMap;
    valor    : single;
    nc, nr   : integer;
    MC1      : MatConv;
    Im1,Im2  : MatImg;
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
end;


// ************ Ajuste de las banderas de seleccion segun el cambio
procedure TAppPDI.Activarseleccion1Click(Sender: TObject);
begin
  _banRect := true;
  StatusBar1.Panels[6].Text := 'Activa';

  // Si lo primero que hace el usuario despues de abierta la imagen es la seleccion
  Mat2Mat(Im1,Im2);
end;
procedure TAppPDI.ActivarSeleccionCir1Click(Sender: TObject);
begin
  _banCir := true;
  StatusBar1.Panels[6].Text := 'Activa';
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

// ******** SELECCIONES
procedure TAppPDI.Image2SelecMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if _banRect and (Button = mbLeft) then begin
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

    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Rectangle(_x1,_y1,_x2,_y2);
    Image2Selec.Picture.Assign(BMSel);

    _boolSeleccionando := false;
  end;
end;



// ************Ajustar canales según el cambio
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


// Cambia Tema
procedure TAppPDI.EstiloFCC1Click(Sender: TObject);
begin
  ToolBar1.Images := ImageList1;
end;

procedure TAppPDI.EstiloPunk1Click(Sender: TObject);
begin
  ToolBar1.Images := ImageList2;
end;

// Histograma
procedure TAppPDI.Histograma1Click(Sender: TObject);
begin
  // Llamar a la interface del Histograma
  FormHisto.show;
end;


// Abrir una imagen
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

// Salvar Como
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

  if ((ssLeft in Shift) and _boolSeleccionando) then begin
    // Borramos el anterior
    BMSel.Canvas.Pen.Color := clWhite;
    BMSel.Canvas.Rectangle(0,0,BMSel.Width, BMSel.Height);
    Image2Selec.Picture.Assign(BMSel);

    // Dibujamos el temporal
    BMSel.Canvas.Pen.Color := clGreen;

    BMSel.Canvas.Rectangle(_x1,_y1,X,Y);
    Image2Selec.Picture.Assign(BMSel);
  end;
end;


// Hacer deshacer
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

//Funcion Exponencial
procedure TAppPDI.FuncionExponencial1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    valor := StrToFloat(Edit1.Text);
    Prepara();
    fp_exponencial(Im1,Im2,valor);
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

procedure TAppPDI.BlancoyNegro1Click(Sender: TObject);
begin
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


// ****************** REGIONALES ************************
procedure TAppPDI.BordesX1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fr_BSX(im1, im2);
    Presenta();
  end;
end;



// Borde simple en Y
procedure TAppPDI.BordesY1Click(Sender: TObject);
begin
  if CanalPrendido then begin
    Prepara();
    fr_BSY(im1, im2);
    Presenta();
  end;
end;

end.









