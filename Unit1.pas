// Sistema de desarrollo para la implementacion de
// Procesamiento Digital de iMgenes
// V 0.4
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
  UBase, UHisto, UPuntuales;

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

    // Metodos
    procedure Abrir1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
  private
    { Private declarations }

    procedure Prepara();
    procedure Presenta();
  public
    { Public declarations }
    nomIma   : string;
    BM1      : TBitMap;
    valor    : single;
    Im1,Im2  : MatImg;
    nc,nr    : integer;
  end;

var
  AppPDI: TAppPDI;

implementation

{$R *.dfm}

// Inicializacion de la Ventana
procedure TAppPDI.FormCreate(Sender: TObject);
begin
  StatusBar3.Panels[0].Text := 'Archivo';

  StatusBar2.Panels[0].Text := 'X';
  StatusBar2.Panels[1].Text := 'Y';

  StatusBar2.Panels[2].Text := 'R';
  StatusBar2.Panels[3].Text := 'G';
  StatusBar2.Panels[4].Text := 'B';

  Application.Icon.LoadFromFile('alien.ico');

  BM1 := TBitMap.Create;

  _banCir := false;

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

      StatusBar3.Panels[1].Text := nomIma
    finally
      pic.Free;
    end;

  end;
end;


// Aviso fuera de imagen
procedure TAppPDI.Image1MouseLeave(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '??';
  StatusBar1.Panels[1].Text := '??';

  StatusBar1.Panels[2].Text := '??';
  StatusBar1.Panels[3].Text := '??';
  StatusBar1.Panels[4].Text := '??';
end;

// Informa (X,Y) y RGB del pixel dentro de la Imagen
procedure TAppPDI.Image1MouseMove(Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer);
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
  Prepara();
  fp_negativo(Im1,Im2);
  Presenta();
end;

// Correccion Gamma
procedure TAppPDI.Gamma1Click(Sender: TObject);
begin
  valor := StrToFloat(Edit1.Text);

  Prepara();
  fp_gamma(Im1,Im2,valor);
  Presenta();
end;

// amplificacion logaritmica
procedure TAppPDI.Logaritmo1Click(Sender: TObject);
begin
  Prepara();
  fp_logaritmo(Im1,Im2);
  Presenta();
end;

//Funcion Seno
procedure TAppPDI.FuncionSeno1Click(Sender: TObject);
begin
Prepara();
fp_seno(Im1,Im2);
Presenta();
end;

//Funcion Exponencial
procedure TAppPDI.FuncionExponencial1Click(Sender: TObject);
begin
Prepara();
fp_exponencial(Im1,Im2);
Presenta();
end;

// aditivo
procedure TAppPDI.Constante50501Click(Sender: TObject);
begin
  valor := StrToFloat(Edit1.Text);

  if abs(valor)>=50 then begin
    ShowMessage('Fuera de rango, lea !!!');
    exit;
  end;

  Prepara();
  fp_constante(Im1,Im2, valor);
  Presenta();
end;

// porcentual
procedure TAppPDI.Porcentual50501Click(Sender: TObject);
begin
  valor := StrToFloat(Edit1.Text);

  if abs(valor)>=50 then begin
    ShowMessage('Fuera de rango, lea !!!');
    exit;
  end;

  Prepara();
  fp_porcentual (Im1,Im2, valor);
  Presenta();
end;


end.
