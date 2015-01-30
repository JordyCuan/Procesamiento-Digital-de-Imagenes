unit UPuntuales;

interface

uses
  math, UBase;

  procedure fp_negativo   (MA: MatImg; var MB: MatImg);
  procedure fp_gamma      (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_logaritmo  (MA: MatImg; var MB: MatImg);
  procedure fp_constante  (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);

implementation

// proceso de negativo
procedure fp_negativo(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
begin
  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := 255 - MA.dat[x][y][c];
end;

// Proceso de filtrado gamma (rango dinamico)
procedure fp_gamma   (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;

  function gamma(z,gg: single): single;
  begin
    result := 255*power(z/255,gg);
  end;

begin
  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := gamma(MA.dat[x][y][c],vv);

end;

// Filtro logaritmico - tipo vision nocturna
procedure fp_logaritmo (MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
  ff      : single;

  function loga(z: single): single;
  begin
    result := ff*log10(z+1);
  end;

begin
  ff := 255/log10(256);

  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := loga(MA.dat[x][y][c]);

end;

// Aplica aditivamente una constante
// proceso de negativo
procedure fp_constante(MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
begin
  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := MA.dat[x][y][c] + vv;
end;

// Aplica multiplicativa una constante
// proceso de negativo
procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
  ff      : single;
begin
  ff := 1 + vv/100;

  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := ff*MA.dat[x][y][c];
end;


end.
