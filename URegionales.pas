unit URegionales;

interface


uses
	math, Ubase;


procedure fr_BSX(MA: MatImg; var MB: MatImg);
procedure fr_BSY(MA: MatImg; var MB: MatImg);
procedure fr_BSXY(MA: MatImg; var MB: MatImg);


function CONVB(MatI : MatImg; MatC : MatConv; wx,wy,x,y,c : integer) : single; overload;

procedure fr_BSCY(MA: MatImg; MC: MatConv; var MB: MatImg);


procedure fr_BordeConX(MA: MatImg; MC: MatConv; var MB: MatImg);
procedure FR_MediaConX(Ma: MatImg; MC: MatConvNM; var Mb: MatImg);
procedure fr_MedianaX(Ma: MatImg; MCMed: MatConvNM; var Mb: MatImg);


implementation


// *****************************************************
// ********************** BORDES ***********************
// *****************************************************

// Bordes Simples en X
procedure fr_BSX(MA: MatImg; var MB: MatImg);
var
	x,y,c : integer;
begin
	for c:= 0 to 2 do
		if _kan[c] then
      if _Norma = 0 then
        for y := _y1+1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := abs(MA.dat[x][y][c] - MA.dat[x][y-1][c])
      else
        for y := _y1+1 to _y2-1 do
          for x := _x1 to _x2-1 do
            MB.dat[x][y][c] := repuja(MA.dat[x][y][c] - MA.dat[x][y-1][c])
		else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;


// Bordes Simples en Y
procedure fr_BSY(MA: MatImg; var MB: MatImg);
var
	x,y,c : integer;
begin
	for c:= 0 to 2 do
		if _kan[c] then
      if _Norma = 0 then
        for y := _y1 to _y2-1 do
          for x := _x1+1 to _x2-1 do
            MB.dat[x][y][c] := abs(MA.dat[x][y][c] - MA.dat[x-1][y][c])
      else
        for y := _y1 to _y2-1 do
          for x := _x1+1 to _x2-1 do
            MB.dat[x][y][c] := repuja(MA.dat[x][y][c] - MA.dat[x-1][y][c])
		else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;


// Bordes Simples en XY
procedure fr_BSXY(MA: MatImg; var MB: MatImg);
var
	x,y,c : integer;
begin
	for c:= 0 to 2 do
		if _kan[c] then
      if _Norma = 0 then
        for y := _y1+1 to _y2-1 do
          for x := _x1+1 to _x2-1 do
            MB.dat[x][y][c] := abs(MA.dat[x][y][c] - MA.dat[x-1][y-1][c])
      else
        for y := _y1+1 to _y2-1 do
          for x := _x1+1 to _x2-1 do
            MB.dat[x][y][c] := repuja(MA.dat[x][y][c] - MA.dat[x-1][y-1][c])
		else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;


function CONVB(MatI : MatImg; MatC : MatConv; wx,wy,x,y,c : integer) : single; overload;
var
	i,j 	 : integer;
	ii, jj : integer;
	ss 		 : single;

begin
	ss := 0.0;

  jj := 0;
	for j := y-wy to y+wy do begin
    ii := 0;
		for i := x-wx to x+wx do begin
			ss := ss + MatI.dat[i][j][c] * MatC.dat[ii][jj];
      ii := ii + 1;
		end;
    jj := jj + 1;
	end;

	result := ss;
end;

function CONVB(MatI : MatImg; MatC : MatConvNM; wx,wy,x,y,c : integer) : single; overload;
var
	i,j 	 : integer;
	ii, jj : integer;
	ss 		 : single;

begin
	ss := 0.0;

  jj := 0;
	for j := y-wy to y+wy do begin
    ii := 0;
		for i := x-wx to x+wx do begin
			ss := ss + MatI.dat[i][j][c] * MatC.dat[ii][jj];
      ii := ii + 1;
		end;
    jj := jj + 1;
	end;

	result := ss;
end;


procedure fr_BSCY(MA: MatImg; MC: MatConv; var MB: MatImg);
var
	x,y,c : integer;
	Lx, Ly: integer;

begin
	Lx := MC.nc div 2;
	Ly := MC.nr div 2;

	for c := 0 to 2 do
		if _kan[c] then
      if _Norma = 0 then
        for y := _y1 + Ly to _y2-1-Ly do
          for x := _x1+Lx to _x2-1-Lx do
            MB.dat[x][y][c] := abs(CONVB(MA, MC, Lx, Ly, x,y,c))
      else
        for y := _y1 + Ly to _y2-1-Ly do
          for x := _x1+Lx to _x2-1-Lx do
            MB.dat[x][y][c] := repuja(CONVB(MA, MC, Lx, Ly, x,y,c))
		else
			for y := 0 to MA.nr - 1 do
				for x := 0 to MA.nc - 1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;


// Bordes click, cargado de archivos
procedure fr_BordeConX(Ma: MatImg; MC: MatConv; var Mb: MatImg);
begin
  fr_BSCY(MA, MC, MB); // Aprovechamos que ya está definida esta funcion
end;

// *****************************************************
// *********************** FIN *************************
// ********************** BORDES ***********************
// *****************************************************




// *****************************************************
// ********************** MEDIAS ***********************
// *****************************************************

// Convolución con medias de nxm
procedure fr_MediaConX (Ma: MatImg; MC: MatConvNM; var Mb: MatImg);
var
  factor : single;
  n,m,
  Lx, Ly,
  x,y,c : integer;

begin
  Lx := MC.nc div 2;
  Ly := MC.nr div 2;
  factor := MC.fac;

  for c := 0 to 2 do
    if _kan[c] then // aplica el filtro
      for y := _y1 + Ly to _y2-1-Ly do
        for x := _x1+Lx to _x2-1-Lx do
          Mb.dat[x][y][c]:= CONVB(MA, MC, Lx, Ly, x,y,c) / factor
    else
			for y := 0 to MA.nr - 1 do
				for x := 0 to MA.nc - 1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;




// *****************************************************
// ********************* MEDIANAS **********************
// *****************************************************

// Convolución con medianas de nxm
procedure fr_MedianaX (Ma: MatImg; MCMed: MatConvNM; var Mb: MatImg);
var
  unos,
  Lx,Ly,c,x,y : integer;
  arreglo     : array of single;
  numD,med    : integer;



        function Mediana(xx,yy: integer): single;
        var
          i,j  : integer;
          flag : boolean;
          temp : single;


                  procedure recogePixeles(xx,yy: integer);
                  var
                    i,j,k : integer;
                  begin
                    k := 0;

                    for j := -Ly to Ly do
                      for i := -Lx to Lx do begin
                        // Recogemos unicamente los pixeles que coinciden con 1
                        if MCMed.dat[i + Lx, j + Ly] = 1 then begin
                          arreglo[k] := Ma.dat[xx+i, yy+j, c];
                          inc(k);
                        end;
                      end;
                  end;

        begin
          recogePixeles(xx,yy);

          //Ordenacion burbuja
          for j := 0 to numD - 2 do begin
            flag := false;
            for i := 0 to numD - 2 - j do begin
              if arreglo[i] > arreglo[i+1] then begin
                temp := arreglo[i];
                arreglo[i] := arreglo[i+1];
                arreglo[i+1] := temp;
                flag := true;
              end;
            end;
            if not flag then break;
          end;
          result := arreglo[med];
        end;

begin
  Lx := MCMed.nc div 2;
  Ly := MCMed.nr div 2;

  //Buscamos el total de unos
  unos := 0;
  for y := 0 to MCMed.nr-1 do
    for x := 0 to MCMed.nc-1 do
      if MCMed.dat[x,y] = 1 then
        inc(unos);

  numD := unos;
  med := unos div 2;
  SetLength(arreglo, unos);

  for c := 0 to 2 do
    if _kan[c] then // aplica el filtro
      for y := _y1 + Ly to _y2-1-Ly do
        for x := _x1+Lx to _x2-1-Lx do
          Mb.dat[x][y][c]:= Mediana(x,y)
    else
			for y := 0 to MA.nr - 1 do
				for x := 0 to MA.nc - 1 do
					MB.dat[x][y][c] := MA.dat[x][y][c]
end;

end.