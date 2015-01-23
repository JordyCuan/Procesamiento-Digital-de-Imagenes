unit UBase;

interface

uses
  VCL.Graphics, math;

var
  _x1, _x2, _y1, _y2  : integer;
  _xc, _yc, _Rx, _Ry  : integer;
  _banCir             : boolean;

type
  MatImg = record
    nc,nr  : integer;
    dat    : array of array of array of single;
  end;

  // metodos
  function ajusta255(z : single) : byte;
  function ajusta511(z : single) : integer;
  procedure BMP2Mat(BM : TBitMap; var Mat : MatImg);

implementation

// ajustra a 255 tonos
function ajusta255(z : single) : byte;
begin
  if z<0
    then result := 0
    else if z>255
           then result := 255
           else result := ceil(z);
end;

// Ajusta 511 tonos
function ajusta511(z : single) : integer;
begin
  if z<0
    then result := 0
    else if z>511
           then result := 511
           else result := ceil(z);
end;

procedure BMP2Mat( BM : TBitMap; var Mat : MatImg);
var
  x,y,c,pix : integer;
  nnc,nnr   : integer;
  r,g,b     : byte;
begin
  nnr := BM.Height ;
  nnc := BM.Width;

  Mat.nc := nnc;
  Mat.nr := nnr;
  Setlength(Mat.dat,nnc,nnr,3);

  for y := 0 to nnr-1 do
    for x := 0 to nnc-1 do begin
      pix := BM.Canvas.Pixels[x,y];

      Mat.dat[x][y][0] :=  pix AND $FF;
      Mat.dat[x][y][1] := (pix SHR  8) AND $FF;
      Mat.dat[x][y][2] := (pix SHR 16) AND $FF;
    end;
end;


end.
