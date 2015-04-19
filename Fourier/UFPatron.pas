unit UFPatron;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, math, Buttons;

type
  TFFPatron = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Image1: TImage;
    Button2: TButton;
    Bevel1: TBevel;
    Image2: TImage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button3: TButton;
    Label14: TLabel;
    Label15: TLabel;
    TrackBar4: TTrackBar;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label19: TLabel;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    MB : array of array of byte;
    BM : TBitMap;
    procedure actualiza_Panel();
    function Ajusta(x:integer):byte;
  public
    { Public declarations }
    ang, anch, maxim,porc : integer;
  end;

var
  FFPatron: TFFPatron;

implementation

uses UFourier;

{$R *.dfm}

procedure TFFPatron.FormCreate(Sender: TObject);
begin
  ang   := 0;
  anch  := 10;
  maxim := 10;
  porc  := 50;
  BM := TBitMap.Create;
  BM.PixelFormat := pf24bit;
end;

procedure TFFPatron.FormShow(Sender: TObject);
begin
  Image2.Picture.Assign(FFourier.Image1.Picture.Bitmap);
  Panel1.Caption := Format('Ang=%d Ancho=%d Max=%d',[ang,anch,maxim]);
end;

procedure TFFPatron.actualiza_Panel();
begin
  Panel1.Caption := Format('Ang=%d Ancho=%d Max=%d',[ang,anch,maxim]);
end;

function TFFPatron.Ajusta(x: Integer):byte;
begin
  result := x;
  if(x>255)then Result:=255;

end;

// Salva Patrón en formato BMP 24 bits
procedure TFFPatron.Button1Click(Sender: TObject);
var
  nx,ny,xm,ym,x,y,p,q  : integer;
  theta,phi,kk,kx,ky   : single;
  cx,cy,arg,ir         : single;
  nom                  : string;
begin
  if SaveDialog1.Execute then begin
    nom := SaveDialog1.FileName+'.bmp';
    { ---------------  Creamos patron -------------- }
    // ancho y alto  de la imagen a trabajar
    nx := FFourier.Image1.Width;
    ny := FFourier.Image1.Height;

    // Dimensionamos el Bitmap con el patrón
    BM.Width  := nx;
    BM.Height := ny;

    // punto medio de la matriz tipo lienzo
    xm := nx div 2;
    ym := ny div 2;

    // vector de onda
    theta := -PI*ang/180;
    phi   := PI/2 + theta;
    kk    := 2*PI/anch;
    kx    := kk*cos(phi);
    ky    := kk*sin(phi);

    for y := -ym to ym-1 do begin
      cy := ky*y;
      for x := -xm to xm-1 do begin
        cx  := kx*x;
        arg := cx+cy;
        ir  := maxim*(1+cos(arg));
        p   := round(ir);
        q   := RGB(p,p,p);
        BM.Canvas.Pixels[x+xm,y+ym] := q;
      end;
    end;
    BM.SaveToFile(nom);
  end;
end;

// Muestra Patrón
procedure TFFPatron.Button2Click(Sender: TObject);
var
  x,y,nx,ny      : integer;
  kk,kx,ky       : single;
  theta, phi     : single;
  xm,ym,ir,ii    : integer;
  cx,cy,arg      : single;
begin
  // ancho y alto
  nx := Image1.Width;
  ny := Image1.Height;

  // Limpiamos el lienzo
  Image1.Canvas.Pen.Color   := clWhite;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.Rectangle(0,0,nx,ny);

  // punto medio del lienzo
  xm := nx div 2;
  ym := ny div 2;

  // vector de onda
  theta := -PI*ang/180;
  phi   := PI/2 + theta;
  kk    := 2*PI/anch;
  kx    := kk*cos(phi);
  ky    := kk*sin(phi);

  for y := -ym to ym do begin
    cy := ky*y;
    for x := -xm to xm do begin
      cx  := kx*x;
      arg := cx+cy;
      ir  := round(maxim*(1+cos(arg)));
      ii  := RGB(ir,ir,ir);
      Image1.Canvas.Pixels[x+xm,y+ym] := ii;
    end;
  end;
end;

// Combina Imagen con Patrón
procedure TFFPatron.Button3Click(Sender: TObject);
var
  x,y,nx,ny      : integer;
  kk,kx,ky       : single;
  theta, phi     : single;
  xm,ym,peso     : integer;
  cx,cy,arg,ir   : single;
  p1,pp,qq       : integer;
  alf,bet,p2     : single;
begin
  { ---------------  Creamos patron -------------- }
  // ancho y alto  de la imagen a trabajar
  nx := FFourier.Image1.Width;
  ny := FFourier.Image1.Height;

  // Dimensionamos la Matriz con el patrón
  SetLength(MB,nx,ny);

  // punto medio de la matriz tipo lienzo
  xm := nx div 2;
  ym := ny div 2;

  // vector de onda
  theta := -PI*ang/180;
  phi   := PI/2 + theta;
  kk    := 2*PI/anch;
  kx    := kk*cos(phi);
  ky    := kk*sin(phi);

  for y := -ym to ym-1 do begin
    cy := ky*y;
    for x := -xm to xm-1 do begin
      cx  := kx*x;
      arg := cx+cy;
      ir  := maxim*(1+cos(arg));
      MB[x+xm][y+ym] := round(ir);
    end;
  end;

  { ------------- Hacemos la superposición ----------- }
  // leemos alpha
  alf := TrackBar4.Position/100;
  bet := 1.0 - alf;

  // Peso de la combinación
  if RadioButton1.Checked
    then peso := 1
    else peso := 2;

  for y := 0 to ny - 1 do
    for x := 0 to nx - 1 do begin
      p1 := FFourier.Image1.Canvas.Pixels[x,y] and $FF;
      p2 := MB[x][y];
      pp := Ajusta(round(peso*(alf*p1+bet*p2)));
      qq := RGB(pp,pp,pp);
      Image2.Canvas.Pixels[x,y] := qq;
    end;
end;

procedure TFFPatron.TrackBar1Change(Sender: TObject);
begin
  ang := TrackBar1.Position;
  Label7.Caption := IntToStr(ang);
  actualiza_Panel();
end;

procedure TFFPatron.TrackBar2Change(Sender: TObject);
begin
  anch := TrackBar2.Position;
  Label10.Caption := IntToStr(anch);
  actualiza_Panel();
end;

procedure TFFPatron.TrackBar3Change(Sender: TObject);
begin
  maxim := TrackBar3.Position;
  Label13.Caption := IntToStr(maxim);
  actualiza_Panel();
end;

procedure TFFPatron.TrackBar4Change(Sender: TObject);
begin
  porc := TrackBar4.Position;
  Label15.Caption := IntToStr(porc)+'%';
end;

end.
