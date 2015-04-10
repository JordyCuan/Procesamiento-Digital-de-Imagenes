unit parser1;

interface

uses
  Classes, SysUtils, ParseExpr, Dialogs;

type

  { TForm1 }

  // Clase evaluador     f( x )
  TEvaluador = class(TExpressionParser)
    private
      __x : double;

    public
      function  DefineParser(ss : String; vx : char): boolean;
      procedure CleanParser();
      function  Eval(x : double):double;
  end;

implementation

// Clase TEvaluador

function TEvaluador.DefineParser(ss : String; vx : char): boolean;
begin
  DefineParser := true;
  ClearExpressions;
  DefineVariable(vx, @__x);
  try
    AddExpression(ss);
  except on E : Exception do begin
      DefineParser := false;
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TEvaluador.CleanParser();
begin
  ClearExpressions;
end;

function TEvaluador.Eval(x : double):double;
begin
  __x := x;
  Eval := EvaluateCurrent;
end;

initialization

end.

