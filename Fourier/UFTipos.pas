unit UFTipos;

interface

type
  PRuido = record
    x,y,L  : integer;
    estado : boolean;
  end;

  TFil   = (FNull,FPBaja,FPAlta,FPBanda,FruiPe,FButter);
  TVista = (VLn, VLnk, VLnR, VLnkR,VRuiPe);
  TPot   = (Preal,Pcompleja);

  TComplex  = record
    r, i : real;
  end;

  Bmatrix2D       = array of array of byte;
  Rmatrix2D       = array of array of real;
  TComplexMatrix  = array of array of Tcomplex;
  Rvector         = array of real;
  TPuntos_ruido   = array of Pruido;

implementation

end.
 