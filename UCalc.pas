unit UCalc;

interface

uses
  math, UBase;

  // Aritmeticas
  procedure Calc_Suma(I1,I2 : MatImg; var I3 : MatImg);
  procedure Calc_Resta(I1,I2 : MatImg; var I3 : MatImg);

  // Logicas
  procedure Calc_AND(I1,I2 : MatImg; var I3 : MatImg);
  procedure Calc_OR(I1,I2 : MatImg; var I3 : MatImg);


implementation

// Suma de Imagenes
procedure Calc_Suma(I1,I2 : MatImg; var I3 : MatImg);
var
  x,y,c   :integer;
  nxp,nyp :integer;
begin
  nxp := MIN(I1.nc,I2.nc);
  NYP := MIN(I1.nr,I2.nr);

  SetLength(I3.dat,nxp,nyp,3);

  I3.nc := nxp;
  I3.nr := nyp;

  for c := 0 to 2 do
    if _kan[c] then
      for y := 0 to nyp-1 do
        for x := 0 to nxp-1 do
          I3.dat[x][y][c] := (I1.dat[x][y][c] + I2.dat[x][y][c]) / 2;
end;

// Resta de Imagenes
procedure Calc_Resta(I1,I2 : MatImg; var I3 : MatImg);
var
  x,y,c   :integer;
  nxp,nyp :integer;
begin
  nxp := MIN(I1.nc,I2.nc);
  NYP := MIN(I1.nr,I2.nr);

  SetLength(I3.dat,nxp,nyp,3);

  I3.nc := nxp;
  I3.nr := nyp;

  for c := 0 to 2 do
    if _kan[c] then
      for y := 0 to nyp-1 do
        for x := 0 to nxp-1 do
          I3.dat[x][y][c] := (I1.dat[x][y][c] - I2.dat[x][y][c])// / 2;
end;

// And Logico de Imagenes
procedure Calc_AND(I1,I2 : MatImg; var I3 : MatImg);
var
  x,y,c   :integer;
  nxp,nyp :integer;
begin
  nxp := MIN(I1.nc,I2.nc);
  NYP := MIN(I1.nr,I2.nr);

  SetLength(I3.dat,nxp,nyp,3);

  I3.nc := nxp;
  I3.nr := nyp;

  for c := 0 to 2 do
    if _kan[c] then
      for y := 0 to nyp-1 do
        for x := 0 to nxp-1 do
          I3.dat[x][y][c] := ceil(I1.dat[x][y][c]) AND ceil(I2.dat[x][y][c]);
end;


// Or Logico de Imagenes
procedure Calc_OR(I1,I2 : MatImg; var I3 : MatImg);
var
  x,y,c   :integer;
  nxp,nyp :integer;
begin
  nxp := MIN(I1.nc,I2.nc);
  NYP := MIN(I1.nr,I2.nr);

  SetLength(I3.dat,nxp,nyp,3);

  I3.nc := nxp;
  I3.nr := nyp;

  for c := 0 to 2 do
    if _kan[c] then
      for y := 0 to nyp-1 do
        for x := 0 to nxp-1 do
          I3.dat[x][y][c] := ceil(I1.dat[x][y][c]) OR ceil(I2.dat[x][y][c]);
end;

end.
