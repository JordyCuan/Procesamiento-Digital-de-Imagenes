unit UGeometricos;

interface

uses
  math, UBase;

  procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; ang: single; tfondo: integer; modo:byte);
  procedure fg_rotaVMC(MA : MatImg; var MB : MatImg; ang: single; tfondo: integer; modo:byte);
  procedure fg_rotaMas90(MA : MatImg; var MB : MatImg);
  procedure fg_rotaMenos90(MA : MatImg; var MB : MatImg);
  procedure fg_rota180(MA : MatImg; var MB : MatImg);
  procedure fg_flipX(Ma:MatImg; var MB:MatImg);
  procedure fg_flipY(Ma:MatImg; var MB:MatImg);
  procedure fg_zoomF2x(MA:MatImg; var MB:MatImg);
  procedure fg_ReduceF5x(MA:MatImg; var MB:MatImg);
  procedure fg_zoom2x(MA:MatImg; var MB:MatImg);
  procedure fg_zoomIBL(MA:MatImg; var MB:MatImg; nx,ny:integer);
  procedure fg_zoomVC(MA:MatImg; var MB:MatImg; nx,ny :integer);
  procedure fg_ReduceP5x(MA:MatImg; var MB:MatImg);


implementation

 //-----------------------------ROTACION IBL---------------------------------------
procedure fg_rotaIBL(MA : MatImg; var MB : MatImg; ang: single; tfondo: integer;modo:byte);
var
  nnx,nny,
  x,y,can,
  n,m,kk,
  xp,yp,
  np,mp,
  dx,dy,
  xpm1,ypm1,
  xx,yy    : integer;

  tt,
  ar,ca,sa,
  ysa,yca,

  xmi,ymi,
  xma,yma,
  xpp,ypp,
  xf,yf,
  ax, ay,
  t1,t2,t3,t4,
  f1,f2,f3,f4   : single;
  flag,flagC : boolean;
  cotas: array [0..3] of array [0..1] of integer;

  EX,EY     : array [0..3] of single;
  MC : MatImg;

begin

  ar := DegToRad(ang);
  sa := sin(ar);
  ca := cos(ar);

  n := MA.nc;
  m := MA.nr;

  // hallar tamaño del lienzo nuevo
  EX[0] :=   0; EY[0] :=   0;
  EX[1] := n-1; EY[1] :=   0;
  EX[2] :=   0; EY[2] := m-1;
  EX[3] := n-1; EY[3] := m-1;

  // max y min de X e Y
  xmi := 0; ymi := 0;
  xma := 0; yma := 0;

  for kk := 1 to 3 do begin
    xpp :=  EX[kk]*ca + EY[kk]*sa;
    ypp := -EX[kk]*sa + EY[kk]*ca;

    xmi := MIN(xmi,xpp);
    xma := MAX(xma,xpp);
    ymi := MIN(ymi,ypp);
    yma := MAX(yma,ypp);
  end;

  np := ceil(xma-xmi+1);
  mp := ceil(yma-ymi+1);

  //setLength(MB.dat,np,mp,3);
  setLength(MB.dat,np,mp,3);

  _x1 := 0  ; _y1 := 0  ;
  _x2 := np ; _y2 := mp ;

  for y := 0 to mp - 1 do
    for x := 0 to np - 1 do
      for can := 0 to 2 do
        MB.dat[x][y][can] := tfondo;

      case modo of
       // Vecinos
        0: begin
            // Rotacion
            // Desplazamientos debido a la rotacion de las esquinas
            dx := floor(xmi);
            dy := floor(ymi);

            // Rotacion propiamente dicha
            for y := 0 to mp - 1 do begin
              yy := y+dy;
              ysa := yy*sa;
              yca := yy*ca;

              for x := 0 to np - 1 do begin
                xx := x+dx;
                xpp := xx*ca - ysa;
                ypp := xx*sa + yca;

                xp := ceil(xpp);
                yp := ceil(ypp);

                if (xp>=0) and (yp>=0) and (xp<n) and (yp<m) then
                for can := 0 to 2 do begin
                  tt := MA.dat[xp][yp][can];
                  // ---
                  MB.dat[x][y][can] := tt;
                  // ---
                end;
              end;
            end;
        end;
        1: begin
            // IBL
            // Desplazamientos debido a la rotacion de las esquinas
            dx := floor(xmi);
            dy := floor(ymi);

            // Rotacion propiamente dicha
            for y := 0 to mp - 2 do begin
              yy := y+dy;
              ysa := yy*sa;
              yca := yy*ca;

              for x := 0 to np - 2 do begin
                xx := x+dx;
                xpp := xx*ca - ysa;
                ypp := xx*sa + yca;

                xp := floor(xpp);
                xpm1 := xp + 1;
                xf := xpp-xp;
                ax := 1-xf;

                yp := floor(ypp);
                ypm1 := yp + 1;
                yf := ypp-yp;
                ay := 1-yf;

                f1 := ax * ay;
                f2 := xf * ay;
                f3 := ax * yf;
                f4 := xf * yf;

                if (xp>=0) and (yp>=0) and (xp<n-1) and (yp<m-1) then
                  for can := 0 to 2 do begin
                    t1 := MA.dat[xp][yp][can];
                    t2 := MA.dat[xpm1][yp][can];
                    t3 := MA.dat[xp][ypm1][can];
                    t4 := MA.dat[xpm1][ypm1][can];
                    // ---
                    tt :=      f1*t1 + f2*t2;
                    tt := tt + f3*t3 + f4*t4;
                    MB.dat[x][y][can] := tt;
                    // ---
                  end;
              end;
            end;
        end;     // Fin de 1
      end; // Fin del case

      // Izquierda
      x := 0;
      flag := true;

      while flag  and (x < np) do begin
        y := 0;
        while flag and (y < mp) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(y);
        end;
        inc(x);
      end;

      dec(x);
      dec(y);

      cotas[0, 0] := x;
      cotas[0, 1] := y;

      // Derecha
      x := np - 1;
      flag := true;

      while flag  and (x >= 0) do begin
        y := 0;
        while flag and (y < mp) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(y);
        end;
        dec(x);
      end;

      x := x + 2;
      dec(y);

      cotas[1, 0] := x;
      cotas[1, 1] := y;

      // Superior
      y := 0;
      flag := true;

      while flag  and (y < mp) do begin
        x := 0;
        while flag and (x < np) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(x);
        end;
        inc(y);
      end;

      dec(x);
      dec(y);

      cotas[2, 0] := x;
      cotas[2, 1] := y;

      // Inferior
      y := mp - 1;
      flag := true;

      while flag  and (y >= 0) do begin
        x := 0;
        while flag and (x < np) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(x);
        end;
        dec(y);
      end;

      dec(x);
      y := y + 2;

      cotas[3, 0] := x;
      cotas[3, 1] := y;

      // Cortamos
      _x1 := cotas[0,0];
      _x2 := cotas[1,0];
      _y1 := cotas[2,1];
      _y2 := cotas[3,1];

      nnx := _x2 - _x1;
      nny := _y2 - _y1;

      setLength(MC.dat, nnx, nny, 3);

      for x := 0 to nnx - 1 do
        for y := 0 to nny - 1 do begin
          for can := 0 to 2 do begin
            MC.dat[x][y][can] := MB.dat[x+_x1][y+_y1][can];
          end;
        end;


    setLength(MC.dat,0,0,0);

  _x2 := MB.nc;
  _y2 := MB.nr;
end;

//--------------------------ROTACION VMC---------------------------------
procedure fg_rotaVMC(MA : MatImg; var MB : MatImg; ang: single; tfondo: integer; modo:byte);
var
  nnx,nny,
  x,y,can,
  n,m,kk,
  xp,yp,
  np,mp,
  dx,dy,
  xpm1,ypm1,
  xx,yy    : integer;

  tt,
  ar,ca,sa,
  ysa,yca,

  xmi,ymi,
  xma,yma,
  xpp,ypp,
  xf,yf,
  ax, ay,
  t1,t2,t3,t4,
  f1,f2,f3,f4   : single;
  flag,flagC : boolean;
  cotas: array [0..3] of array [0..1] of integer;

  EX,EY     : array [0..3] of single;
  MC : MatImg;

begin

  ar := DegToRad(ang);
  sa := sin(ar);
  ca := cos(ar);

  n := MA.nc;
  m := MA.nr;

  // hallar tamaño del lienzo nuevo
  EX[0] :=   0; EY[0] :=   0;
  EX[1] := n-1; EY[1] :=   0;
  EX[2] :=   0; EY[2] := m-1;
  EX[3] := n-1; EY[3] := m-1;

  // max y min de X e Y
  xmi := 0; ymi := 0;
  xma := 0; yma := 0;

  for kk := 1 to 3 do begin
    xpp :=  EX[kk]*ca + EY[kk]*sa;
    ypp := -EX[kk]*sa + EY[kk]*ca;

    xmi := MIN(xmi,xpp);
    xma := MAX(xma,xpp);
    ymi := MIN(ymi,ypp);
    yma := MAX(yma,ypp);
  end;

  np := ceil(xma-xmi+1);
  mp := ceil(yma-ymi+1);

  setLength(MB.dat,np,mp,3);

  _x1 := 0  ; _y1 := 0  ;
  _x2 := np ; _y2 := mp ;

  for y := 0 to mp - 1 do
    for x := 0 to np - 1 do
      for can := 0 to 2 do
        MB.dat[x][y][can] := tfondo;

      case modo of
        // Vecinos
        0: begin
            // Rotacion
            // Desplazamientos debido a la rotacion de las esquinas
            dx := floor(xmi);
            dy := floor(ymi);

            // Rotacion propiamente dicha
            for y := 0 to mp - 1 do begin
              yy := y+dy;
              ysa := yy*sa;
              yca := yy*ca;

              for x := 0 to np - 1 do begin
                xx := x+dx;
                xpp := xx*ca - ysa;
                ypp := xx*sa + yca;

                xp := ceil(xpp);
                yp := ceil(ypp);

                if (xp>=0) and (yp>=0) and (xp<n) and (yp<m) then
                for can := 0 to 2 do begin
                  tt := MA.dat[xp][yp][can];
                  // ---
                  MB.dat[x][y][can] := tt;
                  // ---
                end;
              end;
            end;
        end;
        1: begin
            // IBL
            // Desplazamientos debido a la rotacion de las esquinas
            dx := floor(xmi);
            dy := floor(ymi);

            // Rotacion propiamente dicha
            for y := 0 to mp - 2 do begin
              yy := y+dy;
              ysa := yy*sa;
              yca := yy*ca;

              for x := 0 to np - 2 do begin
                xx := x+dx;
                xpp := xx*ca - ysa;
                ypp := xx*sa + yca;

                xp := floor(xpp);
                xpm1 := xp + 1;
                xf := xpp-xp;
                ax := 1-xf;

                yp := floor(ypp);
                ypm1 := yp + 1;
                yf := ypp-yp;
                ay := 1-yf;

                f1 := ax * ay;
                f2 := xf * ay;
                f3 := ax * yf;
                f4 := xf * yf;

                if (xp>=0) and (yp>=0) and (xp<n-1) and (yp<m-1) then
                  for can := 0 to 2 do begin
                    t1 := MA.dat[xp][yp][can];
                    t2 := MA.dat[xpm1][yp][can];
                    t3 := MA.dat[xp][ypm1][can];
                    t4 := MA.dat[xpm1][ypm1][can];
                    // ---
                    tt :=      f1*t1 + f2*t2;
                    tt := tt + f3*t3 + f4*t4;
                    MB.dat[x][y][can] := tt;
                    // ---
                  end;
              end;
            end;
        end;     // Fin de 1
      end; // Fin del case

      // Izquierda
      x := 0;
      flag := true;

      while flag  and (x < np) do begin
        y := 0;
        while flag and (y < mp) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(y);
        end;
        inc(x);
      end;

      dec(x);
      dec(y);

      cotas[0, 0] := x;
      cotas[0, 1] := y;

      // Derecha
      x := np - 1;
      flag := true;

      while flag  and (x >= 0) do begin
        y := 0;
        while flag and (y < mp) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(y);
        end;
        dec(x);
      end;

      x := x + 2;
      dec(y);

      cotas[1, 0] := x;
      cotas[1, 1] := y;

      // Superior
      y := 0;
      flag := true;

      while flag  and (y < mp) do begin
        x := 0;
        while flag and (x < np) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(x);
        end;
        inc(y);
      end;

      dec(x);
      dec(y);

      cotas[2, 0] := x;
      cotas[2, 1] := y;

      // Inferior
      y := mp - 1;
      flag := true;

      while flag  and (y >= 0) do begin
        x := 0;
        while flag and (x < np) do begin
          can := 0;
          flagC := true;
          while flagC and (can < 3) do begin
            if ceil(MB.dat[x][y][can]) <> tfondo then
              flagC := false;
            inc(can);
          end;
          if not flagC then
            flag := false;
          inc(x);
        end;
        dec(y);
      end;

      dec(x);
      y := y + 2;

      cotas[3, 0] := x;
      cotas[3, 1] := y;

      // Cortamos
      _x1 := cotas[0,0];
      _x2 := cotas[1,0];
      _y1 := cotas[2,1];
      _y2 := cotas[3,1];

      nnx := _x2 - _x1;
      nny := _y2 - _y1;

      setLength(MC.dat, nnx, nny, 3);

      for x := 0 to nnx - 1 do
        for y := 0 to nny - 1 do begin
          for can := 0 to 2 do begin
            MC.dat[x][y][can] := MB.dat[x+_x1][y+_y1][can];
          end;
        end;


      setLength(MC.dat,0,0,0);

  _x2 := MB.nc;
  _y2 := MB.nr;
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
  _x2 := MB.nc;
  _y2 := MB.nr;
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
      for x := 0 to NCX do  begin
        MB.dat[x][y][c]:=MA.dat[NCX-x][y][c];
      end;

   _x2 := MB.nc;
  _y2 := MB.nr;
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
      for x := 0 to NCX do begin
        MB.dat[x][y][c]:=MA.dat[x][NCY-y][c];
      end;

  _x2 := MB.nc;
  _y2 := MB.nr;
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

 //Reduccion 0.5 por metodo del Flojo
 procedure fg_ReduceF5x(MA:MatImg; var MB:MatImg);
 var
  np,mp,
  x,y,can : integer;
  xp,yp : integer;
  temp : single;
begin
  np := MA.nc div 2;
  mp := MA.nr div 2;
  MB.nc:=MA.nc div 2;
  Mb.nr:=Ma.nr div 2;
  SetLength(MB.dat, np, mp, 3);
  for y := 0 to mp - 1 do begin
    yp := 2*y;
    for x := 0 to np - 1 do begin
      xp := 2*x;
      for can := 0 to 2 do begin
        MB.dat[x,   y,   can] := MA.dat[xp, yp, can];
      end;
    end;
  end;
  _x2 := MB.nc;
  _y2 := MB.nr;
end;

//Reduccion 0.5 Por Promedio
procedure fg_ReduceP5x(MA:MatImg; var MB:MatImg);
var
  np,mp,
  x,y,can : integer;
  xp,yp,xx, yy : integer;
  temp,tder,tult : single;
begin
  np := MA.nc div 2;
  mp := MA.nr div 2;

  MB.nc:=MA.nc div 2;
  Mb.nr:=Ma.nr div 2;
  setLength(MB.dat,MB.nc,MB.nr, 3);

  for y := 0 to mp - 1 do begin
    yp := 2*y;
    yy := yp + 1;
    for x := 0 to np - 1 do begin
      xp := 2*x;
      xx := xp + 1;
      for can := 0 to 2 do begin
        temp := MA.dat[xp, yp, can];
        temp := temp + MA.dat[xx,yp,can];
        temp := temp + MA.dat[xp,yy,can];
        temp := temp + MA.dat[xx,yy,can];

        MB.dat[x,   y,   can] := temp / 4;
      end;
    end;
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

    _x2 :=MB.nc;
    _y2 :=MB.nr;
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
