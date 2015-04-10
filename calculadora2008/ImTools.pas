// AUTOR
// Manuel Martin O
// FCC - BUAP - México
//
// Ultima Revisión
// 24 de abril 2008

unit ImTools;

interface

uses
  Graphics, Math, SysUtils;

const
  capacity = 4095;
  _lambda  = 255;

  numelts  = 100;
  abovelts = 101;
type

  aptr     = 1 .. numelts;
  aptr2    = 0 .. abovelts;

  Mat24    = Array of Array of Array of single;
  Mat24S   = Array of Array of Array of single;
  MatMono  = Array of Array of single;
  Vector   = Array of single;

  Vec9       = Array [1..numelts] of single;
  Vec3       = Array [0..3] of single;
  Vec3Real   = Array [0..3] of real;
  Mat3x3Real = Array [1..3,1..3] of Real;
  Vec3Bool   = Array [0..2] of boolean;
  MatPobla   = Array [0..3,0..255] of integer;
  T8Colors   = Array [1..8] of TColor;
  T16Colors  = Array [1..16] of TColor;

  ArrayByte256   = Array [0..256] of byte;
  ArrayByte3x256 = Array [1..3,0..256] of byte;
  ArrayInt256    = Array [0..256] of integer;
  ArrayByteN     = Array [0..capacity] of byte;

  Mat3x3conv = record
    m : Mat3x3Real;
    p : real;
    nom : string;
  end;

  ArrayMat3x3conv = Array of Mat3x3Conv;

  Set8 = set of byte;

  ffunc = function(x:single) : single;

  // Prototipos

  function  mediana9(aa:Vec9) : single ;
  procedure sort(var aa:Vector) ; overload;
  procedure sort(var aa:Vec9) ; overload;
//  procedure sort_inser(var aa:Vec9);
  procedure quickSort(var aa : Vec9; n : aptr);
  function  ajusta255(x : single) : byte;
  procedure Vuelca24(n,m : integer; Matx: Mat24; var BMx :TBitMap);
  procedure Transforma24bits(var BB : TBitMap);

  function  Soft(x : real): integer;

  procedure Copia_Matriz(n,m : integer;
                         M1 : Mat24; var M2 : Mat24);overload;
  procedure Copia_Matriz(n,m : integer;
                         M1 : Mat24S; var M2 : Mat24S);overload;
  procedure Copia_Matriz(n,m : integer;
                         M1 : Mat24; var M2 : Mat24;canal:byte);overload;
  procedure Copia_Matriz(n,m : integer;
                         M1 : Mat24; var M2 : Mat24;c1,c2:byte);overload;

  procedure Copia_MatMono(n,m : integer; M1 : Mat24; var M2 : MatMono);

  procedure Escala_Matriz(x1,y1,x2,y2 : integer; var M2 : Mat24; fac : real );

  procedure LLena_Matriz(var Mat : Mat24; BitMap : TBitMap);
  procedure AplicaLUT (MatIn :Mat24; var MatOut : Mat24;
                       x1,y1,x2,y2 : integer;
                       TT : ArrayByte256; Kcan : Vec3Bool);
  procedure AplicaFUN (MatIn :Mat24; var MatOut : Mat24;
                       x1,y1,x2,y2 : integer;
                       ff : ffunc; Kcan : Vec3Bool);
  procedure CuentaPobla(Mat : Mat24; x1,x2,y1,y2 : integer;
                        var MatPo : MatPobla);overload;
  procedure CuentaPobla(Mat : Mat24; x1,x2,y1,y2 : integer;
                        var MatPo : MatPobla; Canal : byte);overload;
  procedure CotasPobla  (MatPo : MatPobla; var mr,mv,ma,mi : integer);

  function Croja (c : TColor):byte;
  function Cverde(c : TColor):byte;
  function Cazul (c : TColor):byte;

  function RGB (r,g,b : byte):TColor;

  function Gris(r,g,b : byte)  : byte  ; overload;
  function Gris(r,g,b : single): single; overload;

  function GrisB(r,g,b : single):byte;

type
ByteList = class
  private
    N : integer;
    A : ArrayByteN;
  public
    constructor create;
    procedure clear;
    procedure add(x : byte);
    function size : integer;
    function ReadItem(i : integer): byte;
end;

implementation

constructor ByteList.create;
begin
  N := 0;
end;

procedure ByteList.clear;
begin
  N := 0;
end;

procedure ByteList.add(x : byte);
begin
  A[N] := x;
  inc(N);
  if N > Capacity then N := Capacity;
end;

function ByteList.size : integer;
begin
  size := N;
end;

function ByteList.ReadItem(i : integer) : byte;
begin
  ReadItem := A[i];
end;

{ -- Regresa el elemento central del arreglo luego de ordenarlo -- }

function mediana9(aa:Vec9) : single ;
begin
  quickSort(aa,9);
  mediana9 := aa[5];
end;

procedure sort(var aa:Vector);
var
  N       : integer;
  k,LL    : byte;
  tt      : single;
  bb      : boolean;
begin
  N  := length(aa);
  LL := N div 2;
  repeat
    bb := true;
    k  := 1;
    while (k+LL) <= N do begin
      if aa[k] > aa[k+LL] then begin
        tt := aa[k]; aa[k] := aa[k+LL]; aa[k+LL] := tt; bb := false;
      end;
      inc(k,LL);
    end;
    LL := Max(LL div 2,1); if LL > 1 then bb := false;
  until bb and (LL=1);
end;

{ -- Ordena un arreglo de 9 elementos -------------------------- }

procedure sort(var aa : Vec9) ;
const
  N = 9;
var
  k,   LL : byte;
  tt      : single;
  bb      : boolean;
begin
  LL := N div 2;
  repeat
    bb := true;
    k  := 1;
    while (k+LL) <= N do begin
      if aa[k] > aa[k+LL] then begin
        tt := aa[k]; aa[k] := aa[k+LL]; aa[k+LL] := tt; bb := false;
      end;
      inc(k,LL);
    end;
    LL := Max(LL div 2,1); if LL > 1 then bb := false;
  until bb and (LL=1);
end;

procedure quicksort(var aa: Vec9; n : aptr);

{ -- Ordena un arreglo de 9 elementos usando QuickSort ---------- }
procedure quick(lb,ub : aptr2) ;
var
  j : aptr;

  { -- Reordena el arreglo para el QuickSort ------------ }
  procedure reordena(lb,ub : aptr2 ; var j : aptr) ;
  var
    up,down : aptr;
    a       : single;
  begin
    a    := aa[lb];
    j    := lb;
    up   := ub;
    down := lb;
    repeat
      while ((up>down) and (aa[up]>=a)) do dec(up);;
      j := up;
      if up <> down then begin
        aa[down] := aa[up];
        while ((down<up) and (aa[down]<=a)) do inc(down);
        j := down;
        if down <> up then aa[up] := aa[down]
      end;
    until down = up;
    aa[j] := a;
  end;

begin
  if lb < ub then begin
    reordena(lb,ub,j);
    quick(lb,j-1);
    quick(j+1,ub);
  end;
end;

begin
  quick(1,n);
end;


{ -- Ordena un arreglo de 9 elementos por inserción ------------ }
{
procedure sort_inser(var aa : Vec9) ;
const
  N = 9;
var
  i,j  : byte;
  tt   : single;
begin
  for i:=2 to N do begin
    tt := aa[i];
    j := i-1;

    while (aa[j]>tt) and (j>=0) do begin
      aa[j+1]:=aa[j];
      j:=j-1;;
    end;

    aa[j+1]:=tt;
  end;
end;
}

{ --------------------- Ajusta 255 (func) ----------------------}

function ajusta255(x : single) : byte;
begin
  ajusta255 := round(x);
  if x < 0   then ajusta255 :=       0;
  if x > 255 then ajusta255 := _lambda;
end;

{ --------------------- Copia Matriz nxm  ----------------------}

// Copia matriz de bytes
procedure Copia_Matriz(n,m: integer; M1 : Mat24; var M2 : Mat24);
var
  i,j,k : integer;
begin
  for j := 0 to m-1 do
    for i := 0 to n-1 do
      for k := 0 to 2 do
        M2[i,j,k] := M1[i,j,k];

end;

// Copia matriz de singles
procedure Copia_Matriz(n,m: integer; M1 : Mat24S; var M2 : Mat24S);
var
  i,j,k : integer;
begin
  for j := 0 to m-1 do
    for i := 0 to n-1 do
      for k := 0 to 2 do
        M2[i,j,k] := M1[i,j,k];

end;

// Copia un plano
procedure Copia_Matriz(n,m : integer; M1 : Mat24; var M2 : Mat24;canal:byte);
var
  i,j : integer;
begin
  for j := 0 to m-1 do
    for i := 0 to n-1 do
      M2[i,j,canal] := M1[i,j,canal];

end;

// Copia dos planos
procedure Copia_Matriz(n,m : integer; M1 : Mat24; var M2 : Mat24;c1,c2:byte);
var
  i,j : integer;
begin
  for j := 0 to m-1 do
    for i := 0 to n-1 do begin
      M2[i,j,c1] := M1[i,j,c1];
      M2[i,j,c2] := M1[i,j,c2];
    end;

end;

{ --------------------- Copia Matriz nxm Color -> Mono ---------}

procedure Copia_MatMono(n,m: integer; M1 : Mat24; var M2 : MatMono);
var
  i,j : integer;
begin
  // Dado que la entrada está en Grises se elige el canal 0 (rojo)
  for j := 0 to m-1 do
    for i := 0 to n-1 do
      M2[i,j] := M1[i,j,0];

end;

{ --------------------- Escala Matriz ------------------------ }

procedure Escala_Matriz(x1,y1,x2,y2 : integer; var M2 : Mat24; fac : real );
var
  i,j,k : integer;
begin
  for j := y1 to y2-1 do
    for i := x1 to x2-1 do
      for k := 0 to 2 do
        M2[i,j,k] := round(fac*M2[i,j,k]);

end;

{ --------------------- Vuelca24 ----------------------------- }

procedure Vuelca24(n,m : integer; Matx : Mat24; var BMx : TBitMap);
var
  i,j,k    : Integer;
  XBline   : PByteArray;
begin
  BMx.Width  := n;
  BMx.Height := m;

  for j := 0 to m-1 do begin
    XBline := BMx.ScanLine[j];
    for i := 0 to n-1 do begin
      k := 3*i;
      XBline[k]    := ajusta255(Matx[i,j,2]);
      XBline[k+1]  := ajusta255(Matx[i,j,1]);
      XBline[k+2]  := ajusta255(Matx[i,j,0]);
    end;
  end;
end;

{ ------------- Transforma a 24 bits -------------------------- }

procedure Transforma24bits(var BB : TBitMap);
var
  i,j  : integer;
  BMT  : TBitMap;
//  pp   : TPixelFormat;
begin
  BMT             := TBitMap.Create;
  BMT.Width       := BB.Width;
  BMT.Height      := BB.Height;
  BMT.Palette     := 0;
  BMT.PixelFormat := pf24bit;

  for j := 0 to BMT.Height-1 do
    for i := 0 to BMT.Width-1 do
      BMT.Canvas.Pixels[i,j] := BB.Canvas.Pixels[i,j];

  BB.Assign(BMT);
  BMT.Free;
end;

{ ------------------- Llena_Matriz24 -------------------------- }

procedure LLena_Matriz( var Mat : Mat24; BitMap : TBitMap);
var
  i,j,k : Integer;
  n,m   : integer;
  XBline   : PByteArray;
begin
  n := BitMap.Width;
  m := BitMap.Height;

  SetLength(Mat, n, m, 3);

  for j := 0 to m-1 do begin
    XBline := BitMap.ScanLine[j];
    for i := 0 to n-1 do begin
      k := 3*i;
      Mat[i,j,2] := XBline[k];
      Mat[i,j,1] := XBline[k+1];
      Mat[i,j,0] := XBline[k+2];
    end;
  end;
end;

{ ------------------- Soft - Suaviza -------------------------- }

function  Soft(x : real): integer;
var
  z : real;
begin
  z := 100*x;
  Soft := round(z) div 100;
end;

{ ------------------- Aplica LUT a MatIn -> MatOut ------------ }

procedure AplicaLUT (MatIn : Mat24; var MatOut : Mat24;
                     x1,y1,x2,y2 : integer;
                     TT : ArrayByte256; Kcan : Vec3Bool);
var
  i,j,k : Integer;
  p     : Byte;
begin
  // Aplicando LUT a la imagen
  for j := y1 to y2 do begin
    for i:= x1 to x2 do begin
      for k := 0 to 2 do
         if Kcan[k] then begin
           p := ajusta255(MatIn[i,j,k]);
           MatOut[i,j,k] := TT[p];
         end
         else MatOut[i,j,k] := MatIn[i,j,k];
    end;
  end;
end;

procedure Aplicafun (MatIn : Mat24; var MatOut : Mat24;
                     x1,y1,x2,y2 : integer;
                     ff : ffunc; Kcan : Vec3Bool);
var
  i,j,k : integer;
begin
  // Aplicando LUT a la imagen
  for j := y1 to y2 do begin
    for i:= x1 to x2 do begin
      for k := 0 to 2 do
         if Kcan[k] then begin
           MatOut[i,j,k] := ff(MatIn[i,j,k]);
         end
         else MatOut[i,j,k] := MatIn[i,j,k];
    end;
  end;
end;

{ ------------------- Cuenta Poblaciones -------------------- }

procedure CuentaPobla(Mat : Mat24; x1,x2,y1,y2 : integer;
                       var MatPo : MatPobla);
var
  i,j,pr,pv,pa,pi : integer;

begin
  {Limpia Arreglos del Histograma}
  for j := 0 to 3 do
    for i := 0 to 255 do
      MatPo[j,i] := 0;

  {Llena Arreglos del Histograma}
  for j := y1 to y2-1 do
    for i := x1 to x2-1 do begin
      {Cuenta Rojos}   pr := ajusta255(Mat[i,j,0]); inc(MatPo[3,pr]);
      {Cuenta Verdes}  pv := ajusta255(Mat[i,j,1]); inc(MatPo[2,pv]);
      {Cuenta Azules}  pa := ajusta255(Mat[i,j,2]); inc(MatPo[1,pa]);
      {Intensidades}   pi := (pr+pv+pa) div 3; inc(MatPo[0,pi]);
    end;
end;

{ ------------------- Cuenta Población Canal K --------------- }

procedure CuentaPobla(Mat : Mat24; x1,x2,y1,y2 : integer;
                      var MatPo : MatPobla; Canal : byte);
var
  i,j,p,pr,pv,pa : integer;
begin
  {Limpia Arreglo del Histograma del canal correspondinte}
    for i := 0 to 255 do MatPo[3-canal,i] := 0;

  {Llena Arreglos del Histograma}
  if canal < 3  then begin
    for j := y1 to y2-1 do
      for i := x1 to x2-1 do begin
        {Canal K} p := ajusta255(Mat[i,j,canal]); inc(MatPo[3-canal,p]);
      end;
  end
  else begin
    for j := y1 to y2-1 do
      for i := x1 to x2-1 do begin
         pr := ajusta255(Mat[i,j,0]);
         pv := ajusta255(Mat[i,j,1]);
         pa := ajusta255(Mat[i,j,2]);
        {Intensidades} p := (pr+pv+pa) div 3; inc(MatPo[0,p]);
      end;
   end;
end;

{ ----------------------- Cotas Poblaciones ------------------- }

procedure CotasPobla  (MatPo : MatPobla; var mr,mv,ma,mi : integer);
var
  i : integer;
begin
  {Evalua Cotas por plano}
  mr   := MatPo[3,0];
  mv   := MatPo[2,0];
  ma   := MatPo[1,0];
  mi   := MatPo[0,0];
  for i := 1 to 255 do begin
    {Rojo}       mr := Max(MatPo[3,i], mr);
    {Verde}      mv := Max(MatPo[2,i], mv);
    {Azul}       ma := Max(MatPo[1,i], ma);
    {Intensidad} mi := Max(MatPo[0,i], mi);
  end;
end;

// Componentes Cromáticas

function Croja (c : TColor):byte;
begin
  Croja  := c and $FF;
end;

function Cverde(c : TColor):byte;
begin
  Cverde := (c and $FF00) shr 8;
end;

function Cazul (c : TColor):byte;
begin
  Cazul  := (c and $FF0000) shr 16;
end;

function RGB(r,g,b : byte):TColor;
var
  CC : TColor;
begin
  CC  := r;
  CC  := CC or (g shl  8);
  CC  := CC or (b shl 16);
  RGB := $02000000 or CC;
end;

// Determina tono de gris de un punto en el espacio RGB
function Gris(r,g,b : byte):byte;
begin
  Gris := (r+g+b) div 3;
end;

function Gris(r,g,b : single):single;
begin
  Gris := (r+g+b)/3;
end;

function GrisB(r,g,b : single):byte;
var gg : single;
begin
  gg := (r+g+b)/3;
  GrisB := round(gg) and $FF;
end;


{ ----------------------- FIN --------------------------------- }
end.
