unit UPuntuales;

interface

uses
  math, UBase, SysUtils;

  procedure fp_negativo   (MA: MatImg; var MB: MatImg);
  procedure fp_gamma      (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_logaritmo  (MA: MatImg; var MB: MatImg);
  procedure fp_logaritmoPar (MA: MatImg; var MB: MatImg; vv: single);
  procedure fp_blancoNegro(MA: MatImg; var MB: MatImg);
  procedure fp_constante  (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_seno       (MA: MatImg; var MB: MatImg);
  procedure fp_exponencial(MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_coseno     (MA: MatImg; var MB: MatImg);
  procedure fp_claroOscuro(MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_OscFuerte  (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_Senoidal   (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_Luminancia (MA: MatImg; var MB: MatImg);
  procedure fp_Binarizacion(MA: MatImg; var MB:MatImg);
  procedure fp_BinarizacionPar(MA: MatImg; var MB:MatImg; vv:single);
  procedure fp_PerfilTriangular(MA: MatImg; var MB:MatImg; vv: single);


implementation

// proceso de negativo
procedure fp_negativo(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
begin

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := 255 - MA.dat[x][y][c];
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := 255 - MA.dat[x][y][c]
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
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
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := gamma(MA.dat[x][y][c],vv)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := gamma(MA.dat[x][y][c],vv)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
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

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := loga(MA.dat[x][y][c])
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := loga(MA.dat[x][y][c])
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

procedure fp_logaritmoPar (MA: MatImg; var MB: MatImg; vv: single);
var
  x,y,c   : integer;
  ff      : single;

  function loga(z: single): single;
  begin
    result := ff*log10(vv*z+1);
  end;

begin
  ff := 255/log10(256);

  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c] := loga(MA.dat[x][y][c])
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];

end;

//Aplicacion de Fitro de Seno para Aclarado

procedure fp_seno(MA:MatImg; var MB:MatImg);
var
  x,y,c   :integer;
  ff,kk    :single;

  function sen(z:single): single;
  begin
    result:=ff*sin((3.1416*z)/(2*ff));
  end;

begin
  ff:=3.1416/2*255;

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := sen(MA.dat[x][y][c])
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := sen(MA.dat[x][y][c])
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Filtro de Coseno para Obscuresimiento
procedure fp_coseno(MA: MatImg; var MB: MatImg);
var
x,y,c   :integer;
ff,kk   :single;

function coseno(z:single):  single;
begin
  result:= 255*(1-cos((3.1416*z)/(2*255)));
end;

begin

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := coseno(MA.dat[x][y][c])
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := coseno(MA.dat[x][y][c])
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Filtro de Oscurecimiento Fuerte
procedure fp_OscFuerte  (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   :integer;
  ff,kk   :single;

  function osc(z,ee:single): single;
  begin
    result:=ff*(exp((ee*z)/255)-1);
  end;

begin
  ff:=255/(exp(vv)-1);

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := osc(MA.dat[x][y][c],vv)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := osc(MA.dat[x][y][c],vv)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Filtro Exponencial para Aclarado
procedure fp_exponencial(MA:MatImg; var MB:MatImg; vv:single);
var
  x,y,c   :integer;

  function expo(z,ee:single): single;
  begin
    result:=z/(1-exp(-abs(ee)));
  end;
begin
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := expo(MA.dat[x][y][c],vv)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := expo(MA.dat[x][y][c],vv)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Filtro Senodial para Contraste
procedure fp_Senoidal   (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   :integer;
  ff,kk   :single;
  function senoidal(z,ee:single): single;
  begin
    result:=z-ee*sin((kk*z)/255);
  end;

begin
  kk := 2*3.1416;

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := senoidal(MA.dat[x][y][c],vv)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := senoidal(MA.dat[x][y][c],vv)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Fitro arcotangente para claro-obscuro
procedure fp_claroOscuro(MA:MatImg; var MB:MatImg; vv:single);
var
  x,y,c    :integer;
  function claOsc(z,aa:single): single;
  begin
    result:=(255/2)*(1+tanh(abs(aa)*(z-(255/2))));
  end;

begin
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := claOsc(MA.dat[x][y][c],vv)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := claOsc(MA.dat[x][y][c],vv)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Aplicacion de Binarizacion
procedure fp_Binarizacion(MA: MatImg; var MB:MatImg);
const
  Lamb=255;
var
  x,y,c,nx,ny : integer;
  function Bin(z,la:single): single;
  begin
      if(z<Lamb/2) then
        result:=0
        else
        result:=Lamb;
  end;

begin
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := Bin(MA.dat[x][y][c],Lamb)
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := Bin(MA.dat[x][y][c],Lamb)
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

//Binarizacion con Parametro de Interfaz
procedure fp_BinarizacionPar(MA: MatImg; var MB:MatImg;vv: single );
var
  x,y,c,
  nx,ny : integer;

  function BinP(z:single): single;
  begin
    if(z<vv) then
      result:=0
    else
      result:=255;
  end;

begin
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := BinP(MA.dat[x][y][c])
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := BinP(MA.dat[x][y][c])
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;
//Contraste Matriz Triangular
procedure fp_PerfilTriangular(MA: MatImg; var MB:MatImg; vv: single);
var
  lamb2,
  x,y,c,
  nx,ny : integer;  //Num de pix en x y num de pix en y
  z, zp,
  fact  : single;

begin
  lamb2 := 255 * 2;

  for c := 0 to 2 do
  if _kan[c] then
      for y := _y1 to _y2 - 1 do
        for x := _x1 to _x2 - 1 do begin
            z := ajusta255(MA.dat[x][y][c]);
            if (z >= 0) and (z <= vv)then
              zp := power(z, 2) / vv
            else
              if (z > vv) and (z <= 255) then
                zp := vv + (z - vv) / (255 - vv) * (lamb2 - z - vv);

            MB.dat[x][y][c] := zp;
        end
    else
      for y := _y1 to _y2 - 1 do
        for x := _x1 to _x2 - 1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
end;



//Aplicacion de Luminancia
procedure fp_Luminancia (MA: MatImg; var MB: MatImg);
var
  x,y,c: integer;  //Num de pix en x y num de pix en y
  lum: single;
begin
  for x := _x1 to _x2 - 1 do
    for y := _y1 to _y2 - 1 do begin
      lum:=0.3*MA.dat[x][y][0]+0.59*MA.dat[x][y][1]+0.11*MA.dat[x][y][2];
      for c := 0 to 2 do
        MB.dat[x][y][c]:=lum;
    end;
end;




// proceso Blanco y Negro
procedure fp_blancoNegro(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
  temp    : single;
begin

  if _banCir then begin
    xx1 := _xc - _Rx;
    xx2 := _xc + _Rx;
    yy1 := _yc - _Ry;
    yy2 := _yc + _Ry;

    for y := _y1 to _y2-1 do begin
      yy := sqr((y - _yc) / _Ry);
      for x := _x1 to _x2-1 do begin
        xx := sqr((x - _xc) / _Rx);
        RR := xx + yy;
        if RR <= 1 then begin
          temp := 0;
          for c := 0 to 2 do begin
            temp := temp + MA.dat[x][y][c];
          end;
          MB.dat[x][y][0] := temp / 3;
          MB.dat[x][y][1] := temp / 3;
          MB.dat[x][y][2] := temp / 3;
        end;
      end;
    end;
  end

  // Selección rectangular o normal
  else begin
    for y := _y1 to _y2-1 do
      for x := _x1 to _x2-1 do begin
        temp := 0;
        for c := 0 to 2 do begin
          temp := temp + MA.dat[x][y][c];
        end;
        MB.dat[x][y][0] := temp / 3;
        MB.dat[x][y][1] := temp / 3;
        MB.dat[x][y][2] := temp / 3;
      end;
  end;
end;


// Aplica aditivamente una constante
// proceso de negativo
procedure fp_constante(MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
begin
  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := MA.dat[x][y][c] + vv
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c] + vv
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;

// Aplica multiplicativa una constante
// proceso de negativo
procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
  ff      : single;
begin
  ff := 1 + vv/100;

  if _banCir then begin
    for c := 0 to 2 do
      if _kan[c] then begin
        xx1 := _xc - _Rx;
        xx2 := _xc + _Rx;
        yy1 := _yc - _Ry;
        yy2 := _yc + _Ry;

        for y := _y1 to _y2-1 do begin
          yy := sqr((y - _yc) / _Ry);
          for x := _x1 to _x2-1 do begin
            xx := sqr((x - _xc) / _Rx);
            RR := xx + yy;
            if RR <= 1 then
              MB.dat[x][y][c] := ff*MA.dat[x][y][c]
          end;
        end;
      end
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end

  // Selección rectangular o normal
  else begin
    for c := 0 to 2 do
      if _kan[c] then
        for y := _y1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := ff*MA.dat[x][y][c]
      else
        for y := 0 to MA.nr-1 do
          for x := 0 to MA.nc-1 do
            MB.dat[x][y][c] := MA.dat[x][y][c];
  end;
end;



end.
