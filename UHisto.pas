(*
Interface Modelo para mostrar Histogramas de una imagen en modelo de reales
Primavera de 2014

Curso
Procesamiento de Imágenes

Facultad de Ciencias de la Computación  (FCC)
Universidad Autónoma de Puebla  (BUAP)

Prof. Manuel Martín Ortíz

Desarrollado en Delphi 2007 Lite

Bibliotecas utilizadas:

1. Manejo de archivos de imagen
   gfx : Andreas Moser

---------------------------------
  Toma de la interface una de dos matrices que modelan
  la imagen como ternas de números reales cortos:

  tipo

  MatImg  = record
    nc,nr  : integer;
    dat    : array of array of array of single;
  end;

  declarado en UMetBase.
*)

unit UHisto;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, math, ComCtrls,
  UBase, Vcl.ExtDlgs;

const
  mar = 25;

type
  TFormHisto = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    SavePictureDialog1: TSavePictureDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    cols    : array [0..3] of TColor;
    HCans   : array [0..3] of boolean;
    LocMat  : MatImg;
    HX,HY   : integer;
    Lienzo  : TCanvas;
    banCalc : boolean; 

    procedure canales();
    procedure sumas();
    procedure maximos();
    procedure dibuja();

  public
    { Public declarations }
    Histo  : array of array[0..4] of integer;
    HNiv   : integer;
    Maxs   : array [0..3] of integer;
    Maxx   : integer;
  end;

var
  FormHisto: TFormHisto;

implementation

uses Unit1;

{$R *.dfm}

procedure TFormHisto.FormCreate(Sender: TObject);
begin
  cols[0] := clRed;
  cols[1] := clGreen;
  cols[2] := clBlue;
  cols[3] := clBlack;

  HX      := Image1.Width;
  HY      := Image1.Height;
  Lienzo  := Image1.Canvas;
  Lienzo.Brush.Color := clSilver;

  banCalc  := false;

  icon.LoadFromFile('11.ico');
end;

// refrescar
procedure TFormHisto.Button1Click(Sender: TObject);
begin
  if (AppPDI.Im1.nc = 0)
    then ShowMessage('No hay Datos para Procesar ...');

  if (AppPDI.Im2.nc = 0)
    then LocMat := AppPDI.Im1
    else LocMat := AppPDI.Im2;

  if (LocMat.nc > 0) and (LocMat.nr > 0)  then begin

    canales();
    sumas();
    maximos();
    dibuja();

    banCalc := true;
  end;
end;

procedure TFormHisto.canales();
begin
  Hcans[0] := CheckBox1.checked;
  Hcans[1] := CheckBox2.checked;
  Hcans[2] := CheckBox3.checked;
  Hcans[3] := CheckBox4.checked;
end;

procedure TFormHisto.sumas();
var
  kk, cc, x, y , tt : integer;
  gris,RR           : single;
begin
  if ComboBox1.ItemIndex = 0
    then HNiv := 256
    else HNiv := 512;

  // dimensiona
  SetLength(Histo,HNiv);

  // limpia listas
  for cc := 0 to 3 do if Hcans[cc] then
    for kk := 0 to Hniv - 1 do
      Histo[kk][cc] := 0;

  // cuenta
  if (Hniv = 256)
    then begin
      for cc := 0 to 2 do if Hcans[cc] then
         for y := _y1 to _y2 - 1 do
           for x := _x1 to _x2 - 1 do begin
         if _banCir
            then begin
              RR := sqr((x-_xc)/_Rx) + sqr((y-_yc)/_Ry);
              if (RR<=1) then begin
                tt := ajusta255(LocMat.dat[x][y][cc]);
                inc(Histo[tt][cc]);
              end;
            end
            else begin
              tt := ajusta255(LocMat.dat[x][y][cc]);
              inc(Histo[tt][cc]);
            end;
           end;

      for y := _y1 to _y2 - 1 do
         for x := _x1 to _x2 - 1 do begin
         if _banCir
            then begin
              RR := sqr((x-_xc)/_Rx) + sqr((y-_yc)/_Ry);
              if (RR<=1) then begin
                gris := (LocMat.dat[x][y][0]+LocMat.dat[x][y][1]+LocMat.dat[x][y][2])/3;
                tt := ajusta255(gris);
                inc(Histo[tt][3]);
              end;
            end
            else begin
              gris := (LocMat.dat[x][y][0]+LocMat.dat[x][y][1]+LocMat.dat[x][y][2])/3;
              tt := ajusta255(gris);
              inc(Histo[tt][3]);
            end;
         end
    end
    else begin
      for cc := 0 to 2 do if Hcans[cc] then
         for y := _y1 to _y2 - 1 do
           for x := _x1 to _x2 - 1 do begin
         if _banCir
            then begin
              RR := sqr((x-_xc)/_Rx) + sqr((y-_yc)/_Ry);
              if (RR<=1) then begin
                tt := ajusta511(2*LocMat.dat[x][y][cc]);
                inc(Histo[tt][cc]);
              end;
            end
            else begin
              tt := ajusta511(2*LocMat.dat[x][y][cc]);
              inc(Histo[tt][cc]);
            end;
           end;

      for y := _y1 to _y2 - 1 do
         for x := _x1 to _x2 - 1 do begin
         if _banCir
            then begin
              RR := sqr((x-_xc)/_Rx) + sqr((y-_yc)/_Ry);
              if (RR<=1) then begin
                gris := (LocMat.dat[x][y][0]+LocMat.dat[x][y][1]+LocMat.dat[x][y][2])/3;
                tt := ajusta511(2*gris);
                inc(Histo[tt][3]);
              end;
            end
            else begin
              gris := (LocMat.dat[x][y][0]+LocMat.dat[x][y][1]+LocMat.dat[x][y][2])/3;
              tt := ajusta511(2*gris);
              inc(Histo[tt][3]);
            end;
         end
    end;
end;

// máximos
procedure TFormHisto.maximos();
var
  kk, cc, mm: integer;
begin
  // hipótesis: se inicializan los canales solicitados en uno para evitar
  //            la división por cero cuando la imagen está vacia 
  for cc := 0 to 3 do if Hcans[cc]
    then maxs[cc] := Histo[0][cc]+1
    else maxs[cc] := 1;

  // corrección
  for cc := 0 to 3 do if Hcans[cc] then
    for kk := 1 to Hniv - 1 do begin
         mm := Histo[kk][cc];
         maxs[cc] := MAX(maxs[cc],mm);
       end;

  // Informe
  with StatusBar1 do
    for cc := 0 to 3 do if HCans[cc]
      then Panels[2*cc].Text := IntToStr(maxs[cc])
      else Panels[2*cc].Text := 'No Eval';
end;

// dibuja
procedure TFormHisto.dibuja();
const
  desp : integer  = 3;
var
  kk, cc, YY, elMax,paso, XX, nmar : integer;
  facy                             : single;
  letrero                          : string;
begin
  if RadioGroup2.ItemIndex = 0 then begin
    elMax := maxs[0];
    for cc := 1 to 3 do
      elMax := MAX(elMax, maxs[cc]);

    for cc := 0 to 3 do
      maxs[cc] := elMax;
  end;

  // limpia el lienzo
  Lienzo.Rectangle(-1,-1,Hx+1,Hy+1);
  paso  := 512 div HNiv;

  if RadioGroup1.ItemIndex = 0
  then begin
    for cc := 0 to 3 do if HCans[cc] then begin
      Lienzo.Pen.Color := cols[cc];
      facy := (HY - 2*mar)/maxs[cc];
      YY   := ceil(facy*Histo[0][cc]);
      Lienzo.MoveTo(mar,HY - YY - mar);
      for kk := 1 to HNiv - 1 do begin
        YY   := ceil(facy*Histo[kk][cc]);
        Lienzo.LineTo(mar+paso*kk,HY - YY - mar);
      end;
    end
  end
  else begin
    for cc := 0 to 3 do if HCans[cc] then begin
      Lienzo.Pen.Color := cols[cc];
      facy := (HY - 2*mar)/LNXp1(maxs[cc]);
      YY   := ceil(facy*Histo[0][cc]);
      Lienzo.MoveTo(mar,HY - YY - mar);
      for kk := 1 to HNiv - 1 do begin
        YY   := ceil(facy*LNXp1(Histo[kk][cc]));
        Lienzo.LineTo(mar+paso*kk,HY - YY - mar);
      end;
    end;
  end;

  // Ejes
  Lienzo.Pen.Color := clWhite;
  Lienzo.MoveTo(mar-1   ,HY-mar+1);
  Lienzo.LineTo(Hx-mar-1,HY-mar+1);

  Lienzo.MoveTo(mar-1,mar-1);
  Lienzo.LineTo(mar-1,Hy-mar+1);

  // etiquetas de los ejes
  with Lienzo do begin
    // color de las etiquetas
    Font.Color := clYellow;
    // del eje horizontal
    TextOut(Hx-mar-desp, HY-mar+desp,'k');

    // del eje vertical
    TextOut(desp, mar+desp,'H[k]');

    // del origen
    TextOut(mar+desp, Hy-mar+desp,'0'); // en k
    TextOut(2*desp, Hy-mar-2*desp,'0'); // en H[k]

    // marcas del eje k
    nmar := 256 div 8;
    if ComboBox1.ItemIndex = 0
      then cc := 1
      else cc := 2;

    YY := Hy-mar+desp;
    for kk := 1 to 8 do begin
      XX := kk*nmar;
      TextOut(mar + 2*XX - 10, YY , IntToStr(cc*XX) );
    end;

    // Titulo de la Gráfica
    letrero := 'Histograma Cromático';
    XX := Hx div 2 - length(letrero)*Font.Size div 2;
    YY := mar div 2;
    TextOut(XX,YY,letrero);
  end;

end;

// Muestra poblaciones por canal
procedure TFormHisto.Image1MouseMove(Sender: TObject;
    Shift: TShiftState;
    X, Y: Integer);
var
  nn,opc  : integer;
begin
  if banCalc then begin
    opc := ComboBox1.ItemIndex;
    if opc = 0
      then nn := (X-mar) div 2   // 256 tonos
      else nn := (X-mar);        // 512 tonos

    if nn<0 then nn := 0;

    case opc of
      0: if nn>255 then nn := 255;
      1: if nn>511 then nn := 511;
    end;

    StatusBar1.Panels[1].Text := IntToStr(Histo[nn][0]);
    StatusBar1.Panels[3].Text := IntToStr(Histo[nn][1]);
    StatusBar1.Panels[5].Text := IntToStr(Histo[nn][2]);
    StatusBar1.Panels[7].Text := IntToStr(Histo[nn][3]);
    StatusBar1.Panels[8].Text := IntToStr(nn);
  end;
end;

// Salvar Imagen del histograma
procedure TFormHisto.Button2Click(Sender: TObject);
var
  nom  : string;

begin
  if SavePictureDialog1.Execute then begin
    nom := SavePictureDialog1.FileName;

    // salvaImagen(Image1.Picture.Bitmap  ,nom);
  end;
end;

// Salvar datos del histograma
procedure TFormHisto.Button3Click(Sender: TObject);
var
  nom,ren      : string;
  kk,nniv,opc  : integer;
  fid          : TextFile;

begin
  SaveDialog1.Filter := 'Archivo Tabular (*.dat)|*.dat|Todos|*.*|';
  SaveDialog1.DefaultExt := 'dat';
  if SaveDialog1.Execute then begin
    nom := SaveDialog1.FileName;

    opc := ComboBox1.ItemIndex;

    Button1.Click;
    if opc = 0
      then nniv := 256
      else nniv := 512;

    AssignFile(fid,nom);
    Rewrite(fid);

    writeln(fid,'Datos Histograma');
    writeln(fid,'Número de Niveles: ' + IntToStr(nniv) );
    writeln(fid);
    for kk := 0 to nniv - 1 do begin
      ren := Format('%3d %8d %8d %8d %8d',[kk, Histo[kk][0], Histo[kk][1],
                                               Histo[kk][2], Histo[kk][3] ] );
      writeln(fid,ren);
    end;

    CloseFile(fid);
  end;
end;

end.
