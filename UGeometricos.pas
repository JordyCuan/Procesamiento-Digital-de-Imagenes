unit UGeometricos;

interface

uses
  math, UBase;

  procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; aa : single);
  procedure fg_rotaMas90(MA : MatImg; var MB : MatImg);
  procedure fg_rotaMenos90(MA : MatImg; var MB : MatImg);
  procedure fg_rota180(MA : MatImg; var MB : MatImg);
  procedure fg_flipX(Ma:MatImg; var MB:MatImg);
  procedure fg_flipY(Ma:MatImg; var MB:MatImg);
  procedure fg_zoomF2x(MA:MatImg; var MB:MatImg);
  procedure fg_zoom2x(MA:MatImg; var MB:MatImg);
  procedure fg_zoomIBL(MA:MatImg; var MB:MatImg; nx,ny:integer);
  procedure fg_zoomVC(MA:MatImg; var MB:MatImg; nx,ny :integer);


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
  xc, yr, ir, jc, kan, auxX : integer;
begin
  // Invertimos las dimensiones
  MB.nc := MA.nr;
  MB.nr := MA.nc;
  xc := MB.nc;
  yr := MB.nr;

  // Establecemos el tamaño
  Setlength(MB.dat,MB.nc,MB.nr,3);

  auxX := xc - 1;

  // Intercambio
  for kan := 0 to 2 do begin
    for ir := 0 to yr - 1 do begin
      for jc := 0 to xc - 1 do begin
        MB.dat[jc][ir][kan] := MA.dat[ir][auxX - jc][kan];
      end;
    end;
  end;

  _x2 := MB.nc;
  _y2 := MB.nr;

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

  _x2 := MB.nc;
  _y2 := MB.nr;

end;




procedure fg_rota180(MA : MatImg; var MB : MatImg);
var
  xc, yr, ir, jc, kan, auxX, auxY : integer;
begin
  // Invertimos las dimensiones
  xc := MA.nc;
  yr := MA.nr;

  auxX := xc - 1;
  auxY := yr - 1;

  // Intercambio
  for kan := 0 to 2 do begin
    for ir := 0 to yr - 1 do begin
      for jc := 0 to xc - 1 do begin
        MB.dat[jc][ir][kan] := MA.dat[xc - 1 - jc][yr - 1 - ir][kan];
      end;
    end;
  end;
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

//Zoom del Flojo
 procedure fg_zoomF2x(MA:MatImg; var MB:MatImg);
 var
 x,y,c,NCX,NCY  :integer;
 x2,y2          :integer;
 rr             :single;
 begin
 NCX:=MA.nc-1;
 NCY:=MA.nr-1;

 MB.nc:=2*MA.nc;
 MB.nr:=2*MA.nr;

 setLength(MB.dat,MB.nc,MB.nr,3);

 for c := 0 to 2 do
  for y := 0 to NCY do
    for x := 0 to NCX do begin
        x2:=2*x;
        y2:=2*y;
        rr:=MA.dat[x][y][c];
        MB.dat[x2 ][y2  ][c ]:=rr;
        MB.dat[x2+1][y2 ][c ]:=rr;
        MB.dat[x2 ][y2+1 ][c  ]:=rr;
        MB.dat[x2+1][y2+1][c  ]:=rr;
 end;
 _x2 := MB.nc;
  _y2 := MB.nr;
 end;

 //Zoom Por Promedio
 procedure fg_zoom2x(MA:MatImg; var MB: MatImg);
var
x,y,c,NCX,NCY	:integer;
x2,y2,ym1,y2m1,xm1,x2m1,y1	:integer;
ro,rd,ra,re	:single;
ta,td		:single;

begin
NCX:=MA.nc-1;
NCY:=MA.nr-1;

MB.nc:=2*MA.nc;
MB.nr:=2*MA.nr;

setLength(MB.dat,MB.nc,MB.nr,3);
//El bulto
for c := 0 to 2 do
	for y := 0 to NCY-1 do begin
			y2:=2*y;
			y2m1:=y2+1;
			ym1:=y+1;
for x := 0 to NCX-1 do begin
			x2:=2*x;

			ro:=MA.dat[x][y][c];
			rd:=MA.dat[x][ym1][c];
			ra:=MA.dat[x+1][y][c];
			re:=MA.dat[x+1][ym1][c];
      ta:=(ro+ra)/2;


      MB.dat[x2][y2][c]:=ro;
			MB.dat[x2+1][y2][c]:=(ro+rd)/2;
			MB.dat[x2][y2m1][c]:=(ro+ra)/2;
			MB.dat[x2+1][y2m1][c]:=(ro+re)/2;
end;
end;

//Orilla inferior-Ultimo Renglon


for c := 0 to 2 do begin
	 y :=NCY;
	 y2:=2*y;
	 y2m1:=y2+1;
	 ym1:=y+1;

for x := 0 to NCX-1 do begin
			x2:=2*x;

			ro:=MA.dat[x][y][c];
			rd:=MA.dat[x+1][y][c];
			td:=(ro+rd)/2;

      MB.dat[x2][y2][c]:=ro;
			MB.dat[x2+1][y2][c]:=td;
			MB.dat[x2][y2m1][c]:=ro;
			MB.dat[x2+1][y2m1][c]:=td;
end;
end;

//Orilla Derecha
   x :=NCX;
	 xm1:=x+1;
	 x2:=2*x;
	 x2m1:=x2+1;
for c := 0 to 2 do begin
  for y := 0 to NCY-1 do begin
			y2:=2*y;
			y2m1:=y2+1;
			ym1:=y+1;

			ro:=MA.dat[x][y][c];
			ra:=MA.dat[x][ym1][c];
			ta:=(ro+ra)/2;


      MB.dat[x2][y2][c]:=ro;
			MB.dat[x2m1][y2][c]:=ro;
			MB.dat[x2][y2m1][c]:=ta;
			MB.dat[x2m1][y2m1][c]:=ta;
      end;
end;

//Esquina Derecha inferior
   y :=NCY;
	 x:= NCX;
	 y2:=2*y;
	 x2:=2*x;
for c := 0 to 2 do begin
			ro:=MA.dat[x][y][c];
      MB.dat[x2][y2][c]:=ro;
			MB.dat[x2+1][y2][c]:=ro;
			MB.dat[x2][y2+1][c]:=ro;
			MB.dat[x2+1][y2+1][c]:=ro;
end;


_x2:=MB.nc;
_y2:=MB.nr;
end;

//Zooom IBL
procedure fg_zoomIBL(MA:MatImg; var MB:MatImg; nx,ny  :integer);
  //----------------------------------
var
x,y,c,NCX,NCY,
xtm1,ytm1,xt,yt:  Integer;
vac,
dx,dy,
cx,cy,
f1,f2,f3,f4,
fx,fy,
xx,yy          :  single;

begin
  NCX := MA.nc-1;
  NCY := MA.nr-1;

  MB.nc := nx;
  MB.nr := ny;
  setLength(MB.dat,MB.nc,MB.nr,3);

  //Factores de escala
  fx := NCX/(nx-1);
  fy := NCY/(ny-1);

  //El bulto

      for y := 0 to ny-2 do begin
        yy    := fy*y;
        yt    := floor(yy);
        ytm1  := yt+1;
        dy    := yy-yt;
        cy    := 1 -dy;
        for x := 0 to nx-2 do begin
          xx   := fx*x;
          xt   := floor(xx);
          xtm1 := xt+1;
          dx   := xx-xt;
          cx   := 1-dx;

          f1 := cx * cy;
          f2 := dx * cy;
          f3 := cx * dy;
          f4 := dy * dy;
          for c := 0 to 2 do begin

           vac :=       f1 * MA.dat[xt   ][yt  ][c] ;
           vac := vac + f2 * MA.dat[xtm1][yt  ][c] ;
           vac := vac + f3 * MA.dat[xt  ][ytm1][c] ;
           vac := vac + f4 * MA.dat[xtm1][ytm1][c] ;

           MB.dat[x][y][c] := vac;
          end;
       end;
      end;
      _x2:=MB.nc;
      _y2:=MB.nr;
end;

//Zoom Vecinos Mas Cercanos
procedure fg_zoomVC(MA:MatImg; var MB:MatImg; nx,ny: integer);
  var
    x,y,c,NCX,NCY:  integer;
    xt,yt        :  integer;
    fx,fy,xx,yy  :  single;

begin

NCX:=MA.nc-1;
NCY:=MA.nr-1;

MB.nc:=nx;
MB.nr:=ny;

setLength(MB.dat,MB.nc,MB.nr,3);

//Factores de Escala

fx:=NCX/(nx-1);
fy:=NCY/(ny-1);
//Reduccion nx y ny
//El bulto
for c := 0 to 2 do
  for y := 0 to ny-1 do begin
    yy:=fy*y;
    yt:=ceil(yy);
    for x := 0 to nx-1 do begin
      xx:=fx*x;
      xt:=ceil(xx);
      MB.dat[x][y][c]:=MA.dat[xt][yt][c];
    end;
  end;
  _x2:=MB.nc;
  _y2:=MB.nr;
end;




end.//Fin *.pas
