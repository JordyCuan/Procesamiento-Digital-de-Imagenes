{***********************************************************************
Unit gfx_crypt.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi 4

    gfx_crypt is part of the gfx_library collection

    You may use this sourcecode for your freewareproducts.
    You may modify this source-code for your own use.
    You may recompile this source-code for your own use.

    All functions, procedures and classes may NOT be used in commercial
    products without the permission of the author. For parts of this library
    not written by me, you have to ask for permission by their
    respective authors.

    Disclaimer of warranty: "This software is supplied as is. The author
    disclaims all warranties, expressed or implied, including, without
    limitation, the warranties of merchantability and of fitness for any
    purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this software."

    All brand and product names are marks or registered marks of their
    respective companies.

    Please report bugs to:
    Andreas Moser  amoser@amoser.de


********************************************************************************}

unit gfx_crypt;

interface
uses SysUtils,
     classes,
     Windows,
     JPEG,
     Graphics,
     io_files,
     gfx_basedef,
     gfx_files;
{$R-}

procedure EncryptBitmapToFile(SourceBitmap:TBitmap;Key:Integer;Filename:String);
procedure DecryptBitmapFromFile(DestBitmap:TBitmap;Key:Integer;Filename:String);

// Internal functions
procedure EncryptBMP(SourceBitmap,EncryptBitmap:TBitmap;Key:Integer);
procedure DecryptBMP(SourceBitmap,DecryptBitmap:TBitmap;Key:Integer);

implementation

procedure EncryptBitmapToFile(SourceBitmap:TBitmap;Key:Integer;Filename:String);
var EncryptBitmap:TBitmap;
begin
  EncryptBitmap:=TBitmap.Create;
  try
    EncryptBMP(SourceBitmap,EncryptBitmap,Key);
    _SaveBMP(EncryptBitmap,Filename);
  finally
    EncryptBitmap.Free;
  end;
end;

procedure DecryptBitmapFromFile(DestBitmap:TBitmap;Key:Integer;Filename:String);
var DecryptBitmap:TBitmap;
begin
  DecryptBitmap:=TBitmap.Create;
  try
    _LoadBMP(DecryptBitmap,Filename);
    DecryptBMP(DecryptBitmap,DestBitmap,Key);
  finally
    DecryptBitmap.Free;
  end;
end;


procedure EncryptBMP(SourceBitmap,EncryptBitmap:TBitmap;Key:Integer);
var
      i,j,count       :integer;
      rndValue        :byte;
      rowSrc,rowDest  :pByteArray;
BEGIN
    SetBitmapsEql(SourceBitmap,encryptBitmap);
    count := abs(Integer(SourceBitmap.Scanline[1])-Integer(SourceBitmap.Scanline[0]));
    RandSeed := Key;

    for j := 0 to SourceBitmap.Height-1 do
    begin
      RowSrc  := SourceBitmap.Scanline[j];
      RowDest := encryptBitmap.Scanline[j];
      for i := 0 TO count-1 do
      begin
        rndValue := Random(256);
        RowDest[i]   := RowSrc[i] XOR rndValue
      end;
    end;
end;

procedure DecryptBMP(SourceBitmap,DecryptBitmap:TBitmap;Key:Integer);
var
      i,j,count       :integer;
      rndValue        :byte;
      rowSrc,rowDest  :pByteArray;
BEGIN
    SetBitmapsEql(SourceBitmap,DecryptBitmap);
    count := abs(Integer(SourceBitmap.Scanline[1])-Integer(SourceBitmap.Scanline[0]));
    RandSeed := Key;

    for j := 0 to SourceBitmap.Height-1 do
    begin
      RowSrc  := SourceBitmap.Scanline[j];
      RowDest := DecryptBitmap.Scanline[j];
      for i := 0 TO count-1 do
      begin
        RndValue := Random(256);
        RowDest[i]   := RowSrc[i] XOR RndValue
      end;
    end;
end;

end.
