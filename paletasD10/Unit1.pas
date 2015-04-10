unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, parser1, Grids, StdCtrls, Buttons, ExtCtrls, math, ComCtrls;

type
  TForm1 = class(TForm)
    tabla: TStringGrid;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label1: TLabel;
    Memo1: TMemo;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    Label6: TLabel;
    BitBtn4: TBitBtn;
    Button1: TButton;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fun                : array[0..2] of TEvaluador;
    Hx,Hy,NT,Dy        : integer;
    pal                : array of array of single;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function ajusta(x:single):byte;
begin
  if x<=0
    then result := 0
    else if x>255
      then result := 255
      else result := round(x);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tabla.Cells[0,0] := ' Canal ';
  tabla.Cells[1,0] := ' F(z) ';

  tabla.Cells[0,1] := ' R(z) = ';
  tabla.Cells[0,2] := ' G(z) = ';
  tabla.Cells[0,3] := ' B(z) = ';

  tabla.Cells[1,1] := '4*(z-127.5)*(z-127.5)/255';
  tabla.Cells[1,2] := '255*abs(sin(2*Pi*z/255))';
  tabla.Cells[1,3] := '4*z*(255-z)/255';

  fun[0] := Tevaluador.Create;
  fun[1] := Tevaluador.Create;
  fun[2] := Tevaluador.Create;

  NT := 256;

  Hx := Image1.Width;
  Hy := Image1.Height;
  Dy := Hy div 4;
  Image1.Canvas.Rectangle(0,0,Hx,Hy);

  Memo1.Lines.Clear;
end;

// genera paleta
procedure TForm1.BitBtn1Click(Sender: TObject);
var
  t        : single;
  c,k      : word;
  z,dz     : single;
  cp       : TColor;
  ss       : string;
  y1,y2    : integer;
begin
  // definimos funciones
  fun[0].DefineParser(tabla.Cells[1,1],'z');
  fun[1].DefineParser(tabla.Cells[1,2],'z');
  fun[2].DefineParser(tabla.Cells[1,3],'z');

  // dimensionamos la paleta
  SetLength(pal,3,NT);

  // llenamos la paleta
  dz := 255/pred(NT);
  for c := 0 to 2 do begin
    z := 0;
    for k := 0 to NT-1 do begin
      t := fun[c].Eval(z);
      pal[c][k] := t;
      z := z + dz;
    end;
  end;

  // mostramos la paleta
  Memo1.Lines.Clear;
  for k := 0 to NT - 1 do begin
    ss := Format('[%4d]   %6.2f   %6.2f   %6.2f',
                  [k,pal[0][k],pal[1][k],pal[2][k]]);
    Memo1.Lines.Add(ss);
  end;


  if NT = 256 then begin
    // canal rojo
    y1 := 1;
    y2 := Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(ajusta(pal[0][k]),0,0 );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(2*k,y1);
      Image1.Canvas.LineTo(2*k, y2);

      Image1.Canvas.MoveTo(2*k+1,y1);
      Image1.Canvas.LineTo(2*k+1,y2);
    end;

    // canal verde
    y1 := Dy  +1;
    y2 := 2*Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(0,ajusta(pal[1][k]),0 );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(2*k,y1);
      Image1.Canvas.LineTo(2*k, y2);

      Image1.Canvas.MoveTo(2*k+1,y1);
      Image1.Canvas.LineTo(2*k+1,y2);
    end;

    // canal azul
    y1 := 2*Dy+1;
    y2 := 3*Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(0,0,ajusta(pal[2][k]));
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(2*k,y1);
      Image1.Canvas.LineTo(2*k, y2);

      Image1.Canvas.MoveTo(2*k+1,y1);
      Image1.Canvas.LineTo(2*k+1,y2);
    end;

    // mezcla de canales
    y1 := 3*Dy+1;
    for k := 0 to NT - 1 do begin
      cp := RGB(ajusta(pal[0][k]), ajusta(pal[1][k]), ajusta(pal[2][k]) );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(2*k,y1);
      Image1.Canvas.LineTo(2*k,Hy);

      Image1.Canvas.MoveTo(2*k+1,y1);
      Image1.Canvas.LineTo(2*k+1,Hy);
    end;
  end;

  if NT = 512 then begin
    // canal rojo
    y1 := 1;
    y2 := Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(ajusta(pal[0][k]),0,0 );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(k,y1);
      Image1.Canvas.LineTo(k, y2);
    end;

    // canal verde
    y1 := Dy  +1;
    y2 := 2*Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(0,ajusta(pal[1][k]),0 );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(k, y1);
      Image1.Canvas.LineTo(k, y2);
    end;

    // canal azul
    y1 := 2*Dy+1;
    y2 := 3*Dy-1;
    for k := 0 to NT - 1 do begin
      cp := RGB(0,0,ajusta(pal[2][k]));
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(k,y1);
      Image1.Canvas.LineTo(k, y2);
    end;

    // mezcla de canales
    y1 := 3*Dy+1;
    for k := 0 to NT - 1 do begin
      cp := RGB(ajusta(pal[0][k]), ajusta(pal[1][k]), ajusta(pal[2][k]) );
      Image1.Canvas.Pen.Color := cp;

      Image1.Canvas.MoveTo(k,y1);
      Image1.Canvas.LineTo(k,Hy);
    end;
  end;

  // fin
end;

// selecciona un juego de funciones
procedure TForm1.BitBtn4Click(Sender: TObject);
var
  kk       : shortint;
  s0,s1,s2 : string;
begin
  kk := ListBox1.ItemIndex;

  case kk of
    0 : begin
          s0 := '4*(z-127.5)*(z-127.5)/255';
          s1 := '255*abs(sin(2*Pi*z/255))';
          s2 := '4*z*(255-z)/255';
        end;
    1 : begin
          s0 := '255*abs(sin(2*Pi*z/255))';
          s1 := '255*abs(sin(2*Pi*z/255 - Pi/8))';
          s2 := '255*abs(sin(2*Pi*z/255 - Pi/4))';
        end;
    2 : begin
          s0 := '255*abs(sin(5*Pi*z/510))';
          s1 := '255*abs(sin(5*Pi*z/510 - Pi/8))';
          s2 := '255*abs(sin(5*Pi*z/510 - Pi/4))';
        end;

    else ShowMessage('Debe seleccionar un modelo primero ...');
  end;

  tabla.Cells[1,1] := s0;
  tabla.Cells[1,2] := s1;
  tabla.Cells[1,3] := s2;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
 vv : string;
begin
  vv := ComboBox1.Items[  ComboBox1.ItemIndex ];
  NT := StrToInt(vv);
end;

// salva la paleta
procedure TForm1.BitBtn2Click(Sender: TObject);
var
  id  : TextFile;
  k   : word;
  nom : string;
  rr  : string;
  cp  : TColor;
begin
  if SaveDialog1.Execute then begin
    nom := SaveDialog1.FileName;
    AssignFile(id,nom);
    rewrite(id);
    writeln(id,'Archivo de paleta FCC BUAP');

    writeln(id,'# ',NT);
    writeln(id,'# ',tabla.Cells[1,1]);
    writeln(id,'# ',tabla.Cells[1,2]);
    writeln(id,'# ',tabla.Cells[1,3]);
    writeln(id);

    for k := 1 to NT do begin
       rr := Format('%8.3f   %8.3f   %8.3f', [pal[0][k],pal[1][k],pal[2][k]]);
       writeln(id,rr);
    end;

    writeln(id);
    for k := 1 to NT do begin
      cp := RGB(ajusta(pal[0][k]), ajusta(pal[1][k]), ajusta(pal[2][k]) );
      writeln(id,cp);
    end;
    CloseFile(id);
  end;
end;

// carga paleta
procedure TForm1.BitBtn3Click(Sender: TObject);
var
  id  : TextFile;
  nom : string;
  rr  : string;
begin
  if OpenDialog1.Execute then begin
    nom := OpenDialog1.FileName;
    AssignFile(id,nom);
    reset(id);
    readln(id);

    readln(id,rr); rr := copy(rr,3,3); NT := StrToInt(rr);
    if NT=256
      then ComboBox1.ItemIndex := 0
      else ComboBox1.ItemIndex := 1;

    readln(id,rr); rr := copy(rr,3,length(rr)); tabla.Cells[1,1] := rr;
    readln(id,rr); rr := copy(rr,3,length(rr)); tabla.Cells[1,2] := rr;
    readln(id,rr); rr := copy(rr,3,length(rr)); tabla.Cells[1,3] := rr;

    CloseFile(id);
  end;
end;


// diseñador
procedure TForm1.Button1Click(Sender: TObject);
var
  dz,t,z,Ey  : single;
  x,y,
  c,k,dx,
  Hx,Hy,
  marx,mary  : word;
  cols       : array [0..2] of TColor;
begin
  // definimos funciones
  fun[0].DefineParser(tabla.Cells[1,1],'z');
  fun[1].DefineParser(tabla.Cells[1,2],'z');
  fun[2].DefineParser(tabla.Cells[1,3],'z');

  // dimensionamos la paleta
  SetLength(pal,3,NT);

  // llenamos la paleta
  dz := 255/pred(NT);
  for c := 0 to 2 do begin
    z := 0;
    for k := 0 to NT-1 do begin
      t := fun[c].Eval(z);
      pal[c][k] := t;
      z := z + dz;
    end;
  end;

  // Graficamos
  Hx := Image2.Width;
  Hy := Image2.Height;

  marx := 25;
  mary := 25;

  // paso
  if NT = 256
    then dx := 2
    else dx := 1;

  cols[0] := clRed;
  cols[1] := clGreen;
  cols[2] := clBlue;

  Ey := (0.8*Hy-mary)/255;

  // gráfica de datos
  Image2.Canvas.Brush.Color := clSilver;
  Image2.Canvas.Rectangle(0,0,Hx,Hy);

  for c := 0 to 2 do begin
    Image2.Canvas.Pen.Color := cols[c];
    x := marx;
    y := (Hy - mary) - round(Ey*pal[c][0]);
    Image2.Canvas.MoveTo(x,y);
    for k := 1 to NT - 1 do begin
      x := marx + k*dx;
      y := Hy - mary - round(Ey*pal[c][k]);
      Image2.Canvas.LineTo(x,y);
    end;
  end;

  // Ejes
  Image2.Canvas.Pen.Color := clBlack;
  Image2.Canvas.MoveTo(marx , Hy-mary+1 );
  Image2.Canvas.LineTo(Hx-3 , Hy-mary+1 );

  Image2.Canvas.MoveTo(marx , round(0.1*Hy ));
  Image2.Canvas.LineTo(marx , Hy-mary+1 );

  // letreros

  Image2.Canvas.TextOut(2,Hy-mary+5,'(0,0)');
  Image2.Canvas.TextOut(2,round(0.2*Hy) ,'255');
  Image2.Canvas.TextOut(Hx-marx,Hy-mary+5,'255');

end;


end.
