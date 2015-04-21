unit UBase;

interface

uses
  VCL.Graphics, math;

type
  MatImg = record
    nc,nr  : integer;
    dat    : array of array of array of single;
  end;

  //Matris de convolucion
  MatConv = record
    nc, nr : integer;
    dat    : array of array of single;
  end;

  MatConvNM = record
    nc, nr : integer;
    fac    : single;
    dat  : array of array of single;
  end;


var
  //Regiones establecidas en la selección de la imagen
  BMSel               : TBitmap;
  _x1, _x2, _y1, _y2  : integer;
  // Seleccion Circular
  xx1, xx2, yy1, yy2  : integer;
  _xc, _yc, _Rx, _Ry  : integer;
  _xs, _ys            : integer;
  RR, xx, yy          : single;

  _banCir, _banRect   : boolean;
  _x1Selec, _y1Selec,
  _x2Selec, _y2Selec  : integer;
  _boolSeleccionando  : Boolean;

  _kan                : array [0..2] of boolean;
  _MC1Y, _MC1X        : MatConv;
  idf : TextFile;
  x,y : integer;

  // Norma que define el tipo de borde (Val Absoluto / Repujado)
  _Norma              : byte;

  // Falso color
  _Paleta             : array [0..255] of array [0..2] of single;




  // metodos
  function ajusta255(z : single) : byte;
  function ajusta511(z : single) : integer;

  //function CanalPrendido() : boolean;

  procedure BMP2Mat(BM : TBitMap; var Mat : MatImg);
  procedure Mat2BMP(Mat : MatImg; var BM  : TBitMap);
  procedure Mat2Mat(MA : MatImg ; var MB  : MatImg);


  function CanalPrendido() : boolean;

  function repuja(z : single) : single;


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

// Pasar de la representacion matricial a un BitMap
procedure Mat2BMP(Mat : MatImg; var BM : TBitMap);
var
  x,y,c,pix : integer;
  nnc,nnr   : integer;
  r,g,b     : integer;
begin
  nnr := Mat.nr;
  nnc := Mat.nc;

  BM.Width  := nnc;
  BM.Height := nnr;

  for y := 0 to nnr-1 do
    for x := 0 to nnc-1 do begin

      r := ajusta255(Mat.dat[x][y][0]);
      g := ajusta255(Mat.dat[x][y][1]) SHL 8;
      b := ajusta255(Mat.dat[x][y][2]) SHL 16;

      BM.Canvas.Pixels[x,y] := r OR g OR b;
    end;
end;

// Copia Matriz en Matriz (MA -> MB)
procedure Mat2Mat(MA : MatImg ; var MB  : MatImg);
var
  x,y,c,nc,nr  : integer;
begin
  MB.nc := MA.nc;
  MB.nr := MA.nr;
  SetLength(MB.dat,MB.nc,MB.nr,3);

  for y := 0 to MB.nr-1 do
    for x := 0 to MB.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := MA.dat[x][y][c];

end;



// Create???
    (*
begin
  // abrir Archivo para lectura
  Assign(idfile, ArchiString)
  Reset(idfile) // Abrir para lectura


  readln(idfile) //Leer la linea de descripcion


          //Array of MatConv
  readln(idfile, NumMCG) // Lee numero de matrices / filtros de bordes
  SetLenght(_MCG, NumMCG);
  SelLenght(_ACMC, NumMCG);



  for k:=0 to _NumMCG-1 do begin
    readln(idfile, _ACMC[k]); //Leer nombre del filtro
    readln(idfile, _MCG[k].nc, _MCG[k].nr); // Leer dimenciones de la matriz

    //Dar tamaño a la matriz
    SetLenght(_MCG[k].dat, _MCG[k].nc, _MCG[k].nr)

    // Leer la matriz
    for y := 0 to _MCG[k].nc-1 do begin
      for x:=0 to _MCG[k].nr-1 do
        read(idfile, _MCG[k].dat[x][y])
      readln(idfile)
    end;
  end;
*)

function CanalPrendido() : boolean;
begin
  result := _kan[0] or _kan[1] or _kan[2];
end;


function repuja(z : single) : single;
begin
  result := (z+255) / 2;
end;


begin
  _Norma := 0; // Valor Absoluto


  // Matriz Convolucion Bordes Y
  SetLength(_MC1Y.dat, 3,3);
  _MC1Y.nc := 3;
  _MC1Y.nr := 3;

  _MC1Y.dat[0][0] := -1;   _MC1Y.dat[1][0] := 0;   _MC1Y.dat[2][0] := 1;
  _MC1Y.dat[0][1] := -1;   _MC1Y.dat[1][1] := 0;   _MC1Y.dat[2][1] := 1;
  _MC1Y.dat[0][2] := -1;   _MC1Y.dat[1][2] := 0;   _MC1Y.dat[2][2] := 1;


  // Matriz Convolucion Bordes X
  SetLength(_MC1X.dat, 3,3);
  _MC1X.nc := 3;
  _MC1X.nr := 3;

  _MC1X.dat[0][0] := -1;   _MC1X.dat[1][0] := 0;   _MC1X.dat[2][0] := 1;
  _MC1X.dat[0][1] := -1;   _MC1X.dat[1][1] := 0;   _MC1X.dat[2][1] := 1;
  _MC1X.dat[0][2] := -1;   _MC1X.dat[1][2] := 0;   _MC1X.dat[2][2] := 1;
end.
