unit UPuntuales;

interface

uses
  math, UBase;

  procedure fp_negativo (MA: MatImg; var MB: MatImg);
  procedure fp_gamma    (MA: MatImg; var MB: MatImg; valor : single);
  procedure fp_logaritmo(MA: MatImg; var MB: MatImg);

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
procedure fp_gamma(MA: MatImg; var MB: MatImg; valor : single);
var
  x,y,c   : integer;

  function gamma(z,gammaValue: single): byte;
  begin
    result := ceil(255*power(z/255,gammaValue));
  end;

begin
  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := gamma(MA.dat[x][y][c],valor);

end;


procedure fp_logaritmo(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;

  function logaritmo(z: single): byte;
  begin
    result := ceil(
        (Log10(z + 1) / Log10(255 + 1)) * 255);
  end;

begin
  for y := 0 to MA.nr-1 do
    for x := 0 to MA.nc-1 do
      for c := 0 to 2 do
        MB.dat[x][y][c] := logaritmo(MA.dat[x][y][c]);
end;



// End main
end.
