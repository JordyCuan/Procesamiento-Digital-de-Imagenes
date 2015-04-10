unit UPuntuales;

interface

uses
  math, UBase;

  procedure fp_negativo   (MA: MatImg; var MB: MatImg);
  procedure fp_gamma      (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_logaritmo  (MA: MatImg; var MB: MatImg);
  procedure fp_blancoNegro(MA: MatImg; var MB: MatImg);
  procedure fp_constante  (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_seno       (MA: MatImg; var MB: MatImg);
  procedure fp_exponencial(MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_coseno     (MA: MatImg; var MB: MatImg);
  procedure fp_claroOscuro(MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_OscFuerte  (MA: MatImg; var MB: MatImg; vv : single);
  procedure fp_Senoidal   (MA: MatImg; var MB: MatImg; vv : single);


implementation

// proceso de negativo
procedure fp_negativo(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
begin

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

// Proceso de filtrado gamma (rango dinamico)
procedure fp_gamma   (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;

  function gamma(z,gg: single): single;
  begin
    result := 255*power(z/255,gg);
  end;

begin
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

  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c]:= coseno(MA.dat[x][y][c])
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];
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
  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c]:= osc(MA.dat[x][y][c],vv)
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];
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
  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c]:= expo(MA.dat[x][y][c],vv)
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];
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
  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c]:= senoidal(MA.dat[x][y][c],vv)
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];

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
  for c := 0 to 2 do
    if _kan[c] then
      for y := _y1 to _y2-1 do
        for x := _x1 to _x2-1 do
          MB.dat[x][y][c]:= claOsc(MA.dat[x][y][c],vv)
    else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c];
end;


// proceso Blanco y Negro
procedure fp_blancoNegro(MA: MatImg; var MB: MatImg);
var
  x,y,c   : integer;
  temp    : single;
begin

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


// Aplica aditivamente una constante
// proceso de negativo
procedure fp_constante(MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
begin
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

// Aplica multiplicativa una constante
// proceso de negativo
procedure fp_porcentual (MA: MatImg; var MB: MatImg; vv : single);
var
  x,y,c   : integer;
  ff      : single;
begin
  ff := 1 + vv/100;
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

end.
