unit gfx_compression;
{***********************************************************************
Unit gfx_compression.PAS v1.0  0400
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi  4

    gfx_compression is part of the gfx_library collection

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

    The TIFFLZW - and TIFFPackBitsalgorythm are based on source codes by
        Mike Lischke (public@lischke-online.de)

********************************************************************************}


// NOTE : Because the TIFF-LZW algorythm is patented by unisys, i had to remove the lzw decompressor...sorry

interface
uses windows,classes,sysutils;

  procedure DecodeTIFFRLE(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
  procedure EncodeTIFFRLE(Source, Dest: Pointer; var Count: Cardinal);
  procedure DecodeTIFFPackBits(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
  procedure EncodeTIFFPackBits(Source, Dest: Pointer; var Count: Cardinal);
  procedure DecodePCXRLE(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
  procedure EncodePCXRLE(Source, Dest: Pointer; var Count: Cardinal);

  const
    ClearCode = 256;
    EOICode = 257;
    NoLZWCode = 4096;

implementation


procedure DecodeTIFFRLE(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
begin
  //not implemented yet
end;

procedure EncodeTIFFRLE(Source, Dest: Pointer; var Count: Cardinal);
begin
  //not implemented yet
end;

procedure DecodeTIFFPackBits(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
// decodes a simple run-length encoded strip of size PackedSize

var
  SourcePtr,
  TargetPtr: PByte;
  N: SmallInt;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while PackedSize > 0 do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N < 0 then // replicate next Byte -N + 1 times
    begin
      if N = -128 then Continue; // nop
      N := -N + 1;
      FillChar(TargetPtr^, N, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, N);
      Dec(PackedSize);
    end
    else
    begin // copy next N + 1 bytes literally
      Move(SourcePtr^, TargetPtr^, N + 1);
      Inc(TargetPtr, N + 1);
      Inc(SourcePtr, N + 1);
      Dec(PackedSize, N + 1);
    end;
  end;
end;

procedure EncodeTIFFPackBits(Source, Dest: Pointer; var Count: Cardinal);
begin
  //not implemented yet
end;

procedure DecodePCXRLE(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
Var
  databyte,db2:byte;
  repeater:SmallInt;
  Planes,n,p:Word;
Begin
 n:=0;
 repeat

    databyte:=PByte(Source)^;
    inc(PByte(Source));
    If databyte >=$C0 Then
    Begin
      repeater:=databyte And $3F;
      db2:=PByte(Source)^;
      inc(PByte(Source));
      while repeater>0 do
      begin
        pByte(Dest)^:=db2;
        Inc(PByte(Dest));
        dec(repeater);
        Inc(n);
      end;
    end else
      Begin
        pByte(Dest)^:=dataByte;
        Inc(PByte(Dest));
        Inc(n);
      end;
 until n>=UnpackedSize ;
end;

procedure EncodePCXRLE(Source, Dest: Pointer; var Count: Cardinal);
begin
  //not implemented yet
end;


end.
