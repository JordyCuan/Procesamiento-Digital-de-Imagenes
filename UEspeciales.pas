unit UEspeciales;

interface

uses
  math,UBase,SysUtils;


  //Metodos
  procedure Fe_Ecualizacion(MA:MatImg; MB:MatImg);
  procedure Fe_ExpHisto(MA: MatImg; MB: MatImg; zmin,zmax :integer );

implementation

   //Ecualizacion
  procedure Fe_Ecualizacion(MA:MatImg; MB:MatImg);
  var
  tono,
  x,y,can : integer;
  HIS   : array [0..2, 0..255] of integer;//Histograma y acumulativa
  AC   : array [0..2, 0..255] of single;

  // Histograma
  procedure FEHisto();
  var
    canl,
    xx,yy,k : integer;
  begin
    // Limpiar histogramas
    for canl := 0 to 2 do
      if _kan[canl] then
        for k := 0 to 255 do HIS[canl, k] := 0;

    // Contar por tono : hacer el histograma
    for canl := 0 to 2 do
      if _kan[canl] then
      for yy := _y1 to _y2 - 1 do
        for xx := _x1 to _x2 - 1 do begin
          k := ajusta255(MA.dat[xx, yy, canl]); //x,y
          inc(HIS[canl, k]);
        end;
   end;

   // Acumulativa
   procedure FEAcumu();
   var
     canl, vol,
     k : integer;
     fac : single;
   begin

     //Sumamos
     for canl := 0 to 2 do
      if _kan[canl] then begin
        AC[canl, 0] := HIS[canl,0];
        for k := 1 to 255 do
          AC[canl, k] := AC[canl, k - 1] + HIS[canl, k];
     end;

     // Volumen de pixeles
     vol := (_x2 - _x1 + 1) * (_y2 - _y1 + 1);

     //Normalizamos
     fac := 255 / vol;

     for canl := 0 to 2 do
      if _kan[canl] then
        for k := 1 to 255 do
          AC[canl, k] := fac*AC[canl, k];
   end;

begin
  FEHisto();
  FEAcumu();

  for can := 0 to 2 do
    if _kan[can] then
    for y := _y1 to _y2 - 1 do
      for x := _x1 to _x2 - 1 do begin
        tono := ajusta255(MA.dat[x][y][can]);
        MB.dat[x, y, can] := AC[can, tono];
      end;

end;

//Expansion del Histograma
procedure Fe_ExpHisto(MA: MatImg; MB: MatImg; zmin,zmax :integer );
var
  x,y,c : integer;
  HIS   : array [0..2, 0..255] of integer;//Histograma
  t,tp,m,b   : single;

begin

  m := 255 / (zmax - zmin);
  b := -(m * zmin);

  for c := 0 to 2 do
    if _kan[c] then
    for y := _y1 to _y2 - 1 do
      for x := _x1 to _x2 - 1 do begin
        t  := MA.dat[x][y][c];
        tp := t * m + b;
        MB.dat[x][y][c] := ajusta255(tp);
      end;

end;


end.
