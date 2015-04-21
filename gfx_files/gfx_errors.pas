unit gfx_errors;
{***********************************************************************
Unit gfx_errors.PAS v1.2 0801
    (c) by Andreas Moser, amoser@amoser.de,

    Delphi version : Delphi 4

    gfx_errors is part of the gfx_library collection

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
interface
uses classes;
var
    GFXFileErrorList:TStringList;
    GFXRaiseErrors:Boolean;

implementation

initialization
  GFXFileErrorList:=TStringList.Create;
  GFXRaiseErrors:=TRUE;

finalization

   GFXFileErrorList.Free;

end.
 