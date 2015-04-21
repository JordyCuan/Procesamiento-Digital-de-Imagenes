unit UFourierBase;

interface

uses
  Dialogs, math, UFTipos;

  function PowerOf2(n: integer; var m : integer):boolean;

  function Ajusta255(x:real):byte;

  function ComplexProd(x,y : TComplex):TComplex;overload;
  function ComplexProd(x: TComplex;y : real):TComplex;overload;
  function ComplexProd(x: real; y : TComplex):TComplex;overload;

  procedure reordena(var mat : TComplexMatrix; kx,ky : integer);overload;

  procedure ReOrdena(var mat : BMatrix2D; kx,ky : integer);overload;

  procedure MatrizEsc(  var mc   : TCOmplexMatrix;
                        var mat  : Bmatrix2D;
                        kx,ky    : integer;
                        Tv       : TVista;
                        k        : integer);

  procedure MatrizNor(   var mc  : TComplexMatrix;
                         var mat : Bmatrix2D;
                         kx,ky   : integer;
                         ts      : Tpot = Pcompleja);

  procedure MatrizCompleja( var mb  : Bmatrix2D;
                            var mc  : TCOmplexMatrix;
                            kx,ky   : integer);

  procedure CopyMat     (  var m1  : TCOmplexMatrix;
                           var m2  : TCOmplexMatrix;
                           kx,ky   : integer);overload;

  procedure CopyMat     (  var m1  : Bmatrix2D;
                           var m2  : Bmatrix2D;
                           kx,ky   : integer);overload;

  procedure FFT2D(var c : TComplexMatrix; kx,ky, dir : integer);
  procedure FFT( dir,m : integer; var x, y : Rvector);

implementation

// Potencia de 2
function PowerOf2(n : integer; var m : integer): boolean;
var cop,pow : integer;
begin
  if n <= 1 then begin
    PowerOf2 := false;
    exit;
  end;

  m   := 0;
  cop := n;
  while cop > 1 do begin
      inc(m);
      cop := cop shr 1;
  end;

  pow := 1 shl m;
  PowerOf2 := (pow=n);
end;

// Ajusta 255
function Ajusta255(x:real):byte;
var t : byte;
begin
  if x<0 then t := 0
    else if x>255 then t := 255
      else t := round(x);
  Ajusta255 := t;
end;

// Producto de 2 complejos
function ComplexProd(x,y : TComplex):TComplex;
begin
  ComplexProd.r := x.r*y.r - x.i*y.i;
  ComplexProd.i := x.r*y.i + x.i*y.r;
end;

// Producto de un complejo con un real
function ComplexProd(x: TComplex;y : real):TComplex;
begin
  ComplexProd.r := x.r*y;
  ComplexProd.i := x.i*y;
end;

// Producto de un real con un complejo ... juego de sobrecarga
function ComplexProd(x: real;y : TComplex):TComplex;
begin
  ComplexProd.r := y.r*x;
  ComplexProd.i := y.i*x;
end;

// multiplica por (-1)^(i+j) los elementos de la matriz mat compleja
procedure ReOrdena(var mat : TComplexMatrix ; kx,ky : integer);
var
  i,j : integer;
begin
  for i := 0 to kx-1 do
    for  j := 0 to ky-1 do
      if ((i + j) and 1) = 1 then begin
        mat[i,j].r := -mat[i,j].r;
        mat[i,j].i := -mat[i,j].i;
      end;
end;

// multiplica por (-1)^(i+j) los elementos de la matriz mat real
procedure ReOrdena(var mat : BMatrix2D ; kx,ky : integer);
var
  i,j : integer;
begin
  for i := 0 to kx-1 do
    for  j := 0 to ky-1 do
      if ((i + j) and 1) = 1 then mat[i,j] := -mat[i,j];
end;

// Matriz escalada por un filtro tv con parametro k
procedure MatrizEsc(var mc  : TCOmplexMatrix;
                    var mat : Bmatrix2D;
                    kx,ky   : integer;
                    tv      : TVista;
                    k       : integer );
var
  i,j           : integer;
  pmin,pmax,pot : real;
  m,b,p2        : real;
  mt            : RMatrix2D;
begin
  // Copiamos a la matriz auxiliar la magnitud de los complejos
  // almacenados en el vector que arroja la FFT
  //  < esta es la potencia para cada pixel no normalizada >
  // y se obtiene el mayor y menor valor de la potencia
  // Se usa una transformación  ln(x+1) para ampliar el rango dinámico

  pmin := 1.0E100; pmax := -1.0E100;
  SetLength(mt,kx,ky);

  // Caso ln(z+1)
  if tv = VLn then begin
    p2   := power(mc[0][0].r,2) + power(mc[0][0].i,2);
    pmin := ln(p2+1);
    pmax := pmin;

    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        p2 := power(mc[i][j].r,2) + power(mc[i][j].i,2);
        pot := ln(p2+1);
        mt[i,j] := pot;

        pmax := MAX(pmax, pot);
        pmin := MIN(pmin, pot);
      end;
  end;

  // Caso ln(z+1) / solo reales
  if tv = VLnR then begin
    p2   := power(mc[0][0].r,2);
    pmin := ln(p2+1);
    pmax := pmin;

    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        p2 := power(mc[i][j].r,2);
        pot := ln(p2+1);
        mt[i,j] := pot;

        pmax := MAX(pmax, pot);
        pmin := MIN(pmin, pot);
      end;
  end;

  // Caso ln(k*z+1)
  if tv = VLnk then begin
    p2   := power(mc[0][0].r,2) + power(mc[0][0].i,2);
    pmin := ln(k*p2+1);
    pmax := pmin;

    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        p2 := power(mc[i][j].r,2) + power(mc[i][j].i,2);
        pot := ln(k*p2+1);
        mt[i,j] := pot;

        pmax := MAX(pmax, pot);
        pmin := MIN(pmin, pot);
      end;
  end;

  // Caso ln(k*z+1) / solo reales
  if tv = VLnkR then begin
    p2   := power(mc[0][0].r,2);
    pmin := ln(k*p2+1);
    pmax := pmin;

    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        p2 := power(mc[i][j].r,2);
        pot := ln(k*p2+1);
        mt[i,j] := pot;

        pmax := MAX(pmax, pot);
        pmin := MIN(pmin, pot);
      end;
  end;

  // Hacemos que el mínimo se vaya a 0 y el máximo a 255
  if pmax = pmin then pmax := pmin + 1;
  m := 255/(pmax-pmin);
  b := 255 - m*pmax;

  for j := 0 to ky-1 do begin
    for i := 0 to kx-1 do
      mat[i,j] := Ajusta255 (m*mt[i,j] + b);
  end;

end;

procedure MatrizNor( var mc  : TComplexMatrix;
                     var mat : Bmatrix2D;
                     kx,ky   : integer;
                     ts      : TPot);
var
  i,j  : integer;
  pot  : real;
begin
  // Copiamos los cuadrados de los complejos
  // almacenados en el vector que arroja la FFT inversa
  //  < esta es la potencia para cada pixel >

  // potencia compleja
  if ts = Pcompleja then
    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        pot := power(mc[i][j].r,2) + power(mc[i][j].i,2);
        mat[i,j] := Ajusta255(sqrt(pot));
      end;

  // potencia real
  if ts = Preal then
    for j := 0 to ky-1 do
      for i := 0 to kx-1 do begin
        mat[i,j] := Ajusta255(mc[i][j].r);
      end;
end;

procedure MatrizCompleja(var mb  : Bmatrix2D;
                         var mc  : TCOmplexMatrix;
                         kx,ky   : integer);
var
  i,j : integer;
begin
  for j := 0 to ky-1 do
    for i := 0 to kx-1 do begin
       mc[i][j].r := mb[i,j];
       mc[i][j].i := 0.0;
    end;
end;

procedure CopyMat       (var m1  : TCOmplexMatrix; // origen
                         var m2  : TCOmplexMatrix; // destino
                         kx,ky   : integer);
var
  i,j : integer;
begin
  for j := 0 to ky-1 do
    for i := 0 to kx-1 do begin
      m2[i][j].r := m1[i][j].r;
      m2[i][j].i := m1[i][j].i;
    end;
end;

procedure CopyMat       (var m1  : Bmatrix2D; // origen
                         var m2  : Bmatrix2D; // destino
                         kx,ky   : integer);
var
  i,j : integer;
begin
  for j := 0 to ky-1 do
    for i := 0 to kx-1 do
      m2[i][j] := m1[i][j];
end;

(*-------------------------------------------------------------------------
              Perform a 2D FFT inplace given a complex 2D array
              The direction dir, 1 for forward, -1 for reverse
              The size of the array (kx,ky)
              Finish and Returns if the dimensions are not powers of 2
 -------------------------------------------------------------------------*)

procedure FFT2D(var c : TComplexMatrix; kx, ky, dir : integer);
var
   i,j          : integer;
   m            : integer;
   Vreal, Vimag : RVector;
begin
   if NOT Powerof2(kx,m)then begin
     ShowMessage('Error en las dimensiones de la imagen Nx != 2^n');
     exit;
   end;

   if NOT Powerof2(ky,m) then begin
     ShowMessage('Error en las dimensiones de la imagen Ny != 2^n');
     exit;
   end;

   // Transforma the rows
   SetLength(Vreal,kx);
   SetLength(Vimag,ky);

   for j := 0 to ky-1 do begin
      for i := 0 to kx-1 do begin
        Vreal[i] := c[i][j].r;
        Vimag[i] := c[i][j].i;
      end;

      FFT(dir,m,Vreal,Vimag);

      for i := 0 to kx-1 do begin
         c[i][j].r := Vreal[i];
         c[i][j].i := Vimag[i];
      end;
   end;

   // Transform the columns
   SetLength(Vreal,kx);
   SetLength(Vimag,ky);

   for i := 0 to kx-1 do begin
      for j := 0 to ky-1 do begin
         Vreal[j] := c[i][j].r;
         Vimag[j] := c[i][j].i;
      end;

      FFT(dir,m,Vreal,Vimag);

      for j := 0 to ky-1 do begin
         c[i][j].r := Vreal[j];
         c[i][j].i := Vimag[j];
      end;
   end;
end;

(* -------------------------------------------------------------------------
   This computes an in-place complex-to-complex FFT
   x and y are the real and imaginary arrays of 2^m points.
   dir =  1 gives forward transform
   dir = -1 gives reverse transform

     Formula: forward
                  N-1
                  ---
              1   \          - (2 i pi n/N) k
      X(n) = ---   >   x(k) e                    = forward transform
              N   /                                n=0..N-1
                  ---
                  k=0

      Formula: reverse
                  N-1
                  ---
                  \            (2 i pi n/N) k
      X(n) =       >   x(k) e                    = forward transform
                  /                                n=0..N-1
                  ---
                  k=0
*)

procedure FFT( dir, m : integer; var x, y : Rvector);
var
   nn,i,i1,j,k,i2,l,l1,l2     : integer;
   c1,c2,tx,ty,t1,t2,u1,u2,z  : real ;
begin
   // Calculate the number of points
   nn := 1;
   for i := 0 to m-1 do nn := nn shl 1;

   // Do the bit reversal
   i2 := nn shr 1;
   j  := 0;
   for i := 0 to nn-2 do begin
      if (i < j) then begin
         tx   := x[i];
         ty   := y[i];
         x[i] := x[j];
         y[i] := y[j];
         x[j] := tx;
         y[j] := ty;
      end;

      k := i2;
      while (k <= j) do begin
         j := j - k;
         k := k shr 1;
      end;
      j := j + k;
   end;

   // Compute of the FFT
   c1 := -1.0;
   c2 := 0.0;
   l2 := 1;
   for l := 0 to m-1 do begin
      l1 := l2;
      l2 := l2 shl 1;
      u1 := 1.0;
      u2 := 0.0;
      for j := 0 to l1-1 do begin
         i := j;
         while i < nn do begin
            i1    := i + l1;
            t1    := u1 * x[i1] - u2 * y[i1];
            t2    := u1 * y[i1] + u2 * x[i1];
            x[i1] := x[i] - t1;
            y[i1] := y[i] - t2;
            x[i]  := x[i] + t1;
            y[i]  := y[i] + t2;
            inc(i,l2);
         end;

         z  := u1 * c1 - u2 * c2;
         u2 := u1 * c2 + u2 * c1;
         u1 := z;
      end;
      c2 := sqrt((1.0 - c1) / 2.0);
      if (dir = 1) then c2 := -c2;
      c1 := sqrt((1.0 + c1) / 2.0);
   end;

   // Scaling for forward transform
   if (dir = 1) then
      for i := 0 to nn-1 do begin
         x[i] := x[i]/nn;
         y[i] := y[i]/nn;
      end;
end;

end.
