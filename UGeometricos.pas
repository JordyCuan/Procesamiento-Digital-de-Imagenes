unit UGeometricos;

interface

uses
  math, UBase;

  procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; aa : single);
  procedure fg_flipX(Ma:MatImg; var MB:MatImg);
  procedure fg_flipY(Ma:MatImg; var MB:MatImg);


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

//Flip en X
procedure fg_flipX(Ma:MatImg; var MB:MatImg);
var
x,y,c,NCX,NCY:  integer;
begin
  NCX:=Ma.nc-1;
  NCY:=Ma.nr-1;
  for c := 0 to 2 do
    for y:= 0 to NCY do
      for x := 0 to NCX do
        MB.dat[x][y][c]:=MA.dat[NCX-x][y][c]


end;

//Flip en Y
 procedure fg_flipY(Ma:MatImg; var MB:MatImg);
 var
x,y,c,NCX,NCY:  integer;
begin
  NCX:=Ma.nc-1;
  NCY:=Ma.nr-1;
  for c := 0 to 2 do
    for y:= 0 to NCY do
      for x := 0 to NCX do
        MB.dat[x][y][c]:=MA.dat[x][NCY-y][c]


end;


end.
