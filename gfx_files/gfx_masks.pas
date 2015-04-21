{***********************************************************************
Unit gfx_masks.PAS v1.2 0801
    (c) by Andreas Moser, AMoser@amoser.de

    Delphi version : Delphi 4

    gfx_mask is part of the gfx_library collection

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

    NOTE: This source is adapted from
          "Magic Wand", Francesco Savastano 2001
          Web site : http://digilander.iol.it/gensavas/francosava/

********************************************************************************}

unit gfx_masks;

interface

uses Windows,Graphics, SysUtils,extctrls, gfx_basedef;

procedure MaskMagicWand(var SrcBitmap:TBitmap;a,b:Integer;OldColor:TColor;Tolerance:Real;var mask:TBool2dArray);
procedure ShowMagicWandFrame(var SrcBitmap:TBitmap;var mask:TBool2dArray);
procedure ShowMagicWandMask(var SrcBitmap,DestBitmap:TBitmap;var mask:TBool2dArray);
procedure GetMagicWandSelection(var SrcBitmap, DestBitmap:TBitmap;var Mask:TBool2dArray);

implementation

var
  fillx:array[0..80000]of integer;
  filly:array[0..80000]of integer;


  {magic wand algorithm same as seed fill algorithm}
procedure MaskMagicwand(var SrcBitmap:TBitmap;a,b:Integer;OldColor:TColor;Tolerance:Real;var Mask:TBool2dArray);
var
  nf:integer;
  i,j,ir:integer;
  xf,yf:integer;
  procedure MagicWandProcessor(var SrcBitmap:tbitmap;xf,yf:integer;var nf:integer;oldcolor:tcolor;tolerance:real;var mask:tbool2darray);
  var
   hh,ll,ss,h1,l1,s1,aa,bb,cc:real;
   c1,c2,c3,pixr,pixg,pixb,oldr,oldg,oldb,newr,newg,newb:byte;
   pix:tcolorref;
   jj:byte;
   xr,yr:longint;
   gray:real;
   pp:pRGBarray;
   begin
     oldr:=getrvalue(oldcolor);
     oldg:=getgvalue(oldcolor);
     oldb:=getbvalue(oldcolor);

     for jj:=1 to 4 do
     begin
       case jj of
       1:begin
           xr:=xf+1;
           yr:=yf;
         end;
       2:begin
           xr:=xf-1;
           yr:=yf;
          end;
       3:begin
           xr:=xf;
           yr:=yf+1;
         end;
       4:begin
           xr:=xf;
           yr:=yf-1;
         end;
       end;
       if ((xr< SrcBitmap.width)and(jj=1))or((xr>=0)and(jj=2)) or ((yr< SrcBitmap.height)and(jj=3))or((yr>=0)and(jj=4)) then
       begin
         pp:=SrcBitmap.scanline[yr];
         pix:=rgb(pp[xr].rgbtred,pp[xr].rgbtgreen,pp[xr].rgbtblue);
         pixr:=getrvalue(pix);
         pixg:=getgvalue(pix);
         pixb:=getbvalue(pix);
         if (not mask[xr,yr])and(abs(pixr-oldr)<=tolerance*150)and(abs(pixg-oldg)<=tolerance*150)and(abs(pixb-oldb)<=tolerance*150)then
         begin
           mask[xr,yr]:=true;
           nf := nf+1;
           fillx[nf]:= xr;
           filly[nf]:= yr;
         end;
       end;
      end;
    end;

begin
  Setlength(Mask,SrcBitmap.width+5,SrcBitmap.height+5);
  nf := 1;
  ir := 1;
  fillx[nf]:= a;
  filly[nf]:= b;
  MagicWandProcessor(SrcBitmap,a,b,nf,OldColor,Tolerance,Mask);
  while nf>ir do
  begin
    ir := ir+1;
    xf := fillx[ir];
    yf := filly[ir];
    MagicWandProcessor(SrcBitmap,xf,yf,nf,OldColor,Tolerance,Mask);
    if (nf>75000)  then
    begin
      for i := 1 to nf-ir do
      begin
        fillx[i] := fillx[ir+i];
        filly[i] := filly[ir+i];
      end;
      nf := nf-ir ;
      ir := 0;
    end;
  end;
end;


procedure ShowMagicWandFrame(var SrcBitmap:TBitmap;var mask:TBool2dArray);
var
  i,j:integer;
  test:boolean;
  pp:pRGBarray;
begin
  for j:=1 to SrcBitmap.height-2 do
  begin
    pp:=SrcBitmap.scanline[j];
    for i:=1 to SrcBitmap.width-2 do
    begin
      if mask[i,j]then
      begin
        test:= mask[i-1,j] and mask[i+1,j] and mask[i,j-1] and mask[i,j+1] ;
        if not test then
        begin
          {simply invert the colors of the boundary pixels }
          with pp[i] do
          begin
            rgbtred:=255-rgbtred;
            rgbtgreen:=255-rgbtgreen;
            rgbtblue:=255-rgbtblue;
          end;
        end;
      end;
    end;
  end;
end;

procedure ShowMagicWandMask(var SrcBitmap,DestBitmap:TBitmap;var mask:TBool2dArray);
var
  i,j:integer;
  test,mStart:boolean;
  pp,pd:pRGBarray;
begin
  SetBitmapsEql(SrcBitmap,DestBitmap);
  for j:=1 to SrcBitmap.height-2 do
  begin
    pp:=SrcBitmap.scanline[j];
    pd:=DestBitmap.scanline[j];
    mStart:=False;
    for i:=1 to SrcBitmap.width-2 do
    begin
      mStart:=mask[i,j];
      if mask[i,j] then pd[i]:=pp[i];
    end;
  end;
end;

procedure GetMagicWandSelection(var SrcBitmap, DestBitmap:TBitmap;var Mask:TBool2dArray);
var i,j,ul,ut,lr,lb: Integer;
    gUl,gUt,glR,glb:Boolean;
    mStart:boolean;
    pp,pd:pRGBarray;
begin
    gul:=false;
    gut:=false;
    glr:=false;
    glb:=false;
    ul:=SrcBitmap.Width;
    ut:=SrcBitmap.Height;
    lr:=0;
    lb:=0;

    //get bounds
    for j:=1 to SrcBitmap.Height-2 do
    begin
      for i:=1 to SrcBitmap.Width-2 do
      begin
         if Mask[i,j] then
         begin
            if j<ut then ut:=j;
            if j>lb then lb:=j;
            if i<ul then ul:=i;
            if i>lr then lr:=i;
         end;
      end;
    end;
    DestBitmap.PixelFormat:=pf24Bit;
    DestBitmap.Height:= lb-ut;
    DestBitmap.Width:=lr-ul;

    for j:=ut to lb-1 do
    begin
      pp:=SrcBitmap.scanline[j];
      pd:=DestBitmap.scanline[j-ut];
      mStart:=False;
      for i:=ul to lr-1 do
      begin
        mStart:=mask[i,j];
        if mask[i,j] then pd[i-ul]:=pp[i] else pd[i-ul]:=ColorToRGB(clNone);
    end;
  end;

end;

end.
