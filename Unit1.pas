unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants,System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.ExtDlgs,
  Vcl.ComCtrls, math, Vcl.StdCtrls,
  Jpeg, PNGImage, GIFImg, Vcl.ImgList, Vcl.ToolWin;

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
  private
    { Private declarations }
  public
    { Public declarations }
    nomIma : string;
    BM1    : TBitMap;
    valor  : single;
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


// Negativo usando el BitMap: BM1
procedure TAppPDI.Negativo1Click(Sender: TObject);
var
  x,y, pix,
  nc,nr    : integer;
  r,g,b    : byte;
begin
  // descomponer pixel  pixel e invertir
  // en cada canal 255 - z

  nc := BM1.Width;
  nr := BM1.Height;

  for y := 0 to nr-1 do
    for x := 0 to nc-1 do begin

      pix := BM1.Canvas.Pixels[X,Y];
      r := 255 - GetRValue(pix);
      g := 255 - GetGValue(pix);
      b := 255 - GetBValue(pix);

      BM1.Canvas.Pixels[X,Y] := RGB(r,g,b);
    end;

    Image1.Picture.Assign(BM1);
end;

// Correccion Gamma
procedure TAppPDI.Gamma1Click(Sender: TObject);
var
  x,y, pix,
  nc,nr    : integer;
  r,g,b    : byte;

  function gamma(z,gg: single): byte;
  begin
    result := ceil((255*power(z/255,gg)));
  end;

begin
  // descomponer pixel  pixel e invertir
  // en cada canal 255 - z

  nc := BM1.Width;
  nr := BM1.Height;

  valor := StrToFloat(Edit1.Text);

  for y := 0 to nr-1 do
    for x := 0 to nc-1 do begin

      pix := BM1.Canvas.Pixels[X,Y];

      r := gamma(GetRValue(pix),valor);
      g := gamma(GetGValue(pix),valor);
      b := gamma(GetBValue(pix),valor);


      BM1.Canvas.Pixels[X,Y] := RGB(r,g,b);
    end;

    Image1.Picture.Assign(BM1);
end;

end.
