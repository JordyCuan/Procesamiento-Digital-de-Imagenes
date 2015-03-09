unit UGeometricos;

interface

uses
  math, UBase;

  procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; aa : single);
  procedure fg_rotaMas90(MA : MatImg; var MB : MatImg);
  procedure fg_rotaMenos90(MA : MatImg; var MB : MatImg);
  procedure fg_rota180(MA : MatImg; var MB : MatImg);


implementation


procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; aa : single);
begin
  //
  //
  //
  //
  //
  //
  //
  //

{  for y := 0 to nnr - 1 do begin
    yp := y - ycr;
    xa := yp * sa + xc;
    ya := yp * ca + yc;

    for x := 0 to nnc do begin
      xp := x-xcr;
      xt := xp *

    end;
  end;
 }
end;




procedure fg_rotaMas90(MA : MatImg; var MB : MatImg);
var
  xc, yr, ir, jc, kan : integer;
begin
  // Invertimos las dimensiones
  MB.nc := MA.nr;
  MB.nr := MA.nc;
  xc := MB.nc;
  yr := MB.nr;

  // Establecemos el tamaño
  Setlength(MB.dat,MB.nc,MB.nr,3);

  // Intercambio
  for kan := 0 to 2 do begin
    for ir := 0 to yr - 1 do begin
      for jc := 0 to xc - 1 do begin
        MB.dat[jc][ir][kan] := MA.dat[ir][xc - 1 - jc][kan];
      end;
    end;
  end;

end;


procedure fg_rotaMenos90(MA : MatImg; var MB : MatImg);
var
  xc, yr, ir, jc, kan : integer;
begin
  // Invertimos las dimensiones
  MB.nc := MA.nr;
  MB.nr := MA.nc;
  xc := MB.nc;
  yr := MB.nr;

  // Establecemos el tamaño
  Setlength(MB.dat,MB.nc,MB.nr,3);

  // Intercambio
  for kan := 0 to 2 do begin
    for ir := 0 to yr - 1 do begin
      for jc := 0 to xc - 1 do begin
        MB.dat[jc][ir][kan] := MA.dat[ir][jc][kan];
      end;
    end;
  end;

end;




procedure fg_rota180(MA : MatImg; var MB : MatImg);
var
  xc, yr, ir, jc, kan : integer;
begin
  // Invertimos las dimensiones
  xc := MA.nc;
  yr := MA.nr;

  // Intercambio
  for kan := 0 to 2 do begin
    for ir := 0 to yr - 1 do begin
      for jc := 0 to xc - 1 do begin
        MB.dat[jc][ir][kan] := MA.dat[xc - 1 - jc][yr - 1 - ir][kan];
      end;
    end;
  end;
end;









end.//Fin *.pas
