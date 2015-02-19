unit URegionales;

interface


uses
	math, Ubase;


function CONVB(MatI : MatImg; MatM : MatConv; wx,wy,xx,yy,cc : integer) : single;

procedure fr_BSX(MA: MatImg; var MB: MatImg);
procedure fr_BSY(MA: MatImg; var MB: MatImg);

procedure fr_BSCY(MA: MatImg; MC: MatConv; var MB: MatImg);


implementation

procedure fr_BSX(MA: MatImg; var MB: MatImg);
var
	x,y,c : integer;

begin
	for c:= 0 to 2 do
		if _kan[c] then
			for y := _y1+1 to _y2-1 do
				for x := _x1 to _x2-1 do
					MB.dat[x][y][c] := abs(MA.dat[x][y][c] - MA.dat[x][y-1][c])
		else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MB.dat[x][y][c]
end;


procedure fr_BSY(MA: MatImg; var MB: MatImg);
var
	x,y,c : integer;

begin
	for c:= 0 to 2 do
		if _kan[c] then
			for y := _y1 to _y2-1 do
				for x := _x1+1 to _x2-1 do
					MB.dat[x][y][c] := abs(MA.dat[x][y][c] - MA.dat[x-1][y][c])
		else
			for y := 0 to MA.nr-1 do
				for x := 0 to MA.nc-1 do
					MB.dat[x][y][c] := MB.dat[x][y][c]
end;


function CONVB(MatI : MatImg; MatM : MatConv; wx,wy,xx,yy,cc : integer) : single;
var
	i,j 	 : integer;
	ii, jj : integer;
	ss 		 : single;

begin
	ss := 0.0;
	for j := yy-wy to yy+wy do begin
		jj := j + wy;   // INC?
		for i := xx-wx to xx+wx do begin
			ii := i + wx; // INC?
			ss := ss + MatI.dat[xx+i][yy+j][cc] * MatM.dat[ii][jj];
		end;	
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
			for y := _y1 + Ly to _y2-1-Ly do
				for x := _x1+Lx to _x2-1-Lx do
					MB.dat[x][y][c] := abs(CONVB(MA, MC, Lx, Ly, x,y,c))

		else
			for y := 0 to MA.nr - 1 do
				for x := 0 to MA.nc - 1 do
					MB.dat[x][y][c] := MB.dat[x][y][c]
end;


end.