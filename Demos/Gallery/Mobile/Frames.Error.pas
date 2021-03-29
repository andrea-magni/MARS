unit Frames.Error;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FrameStand, SubjectStand;

type
  TErrorFrame = class(TFrame)
    MessageLabel: TLabel;
  private
    [FrameInfo] FrameInfo: TFrameInfo<TErrorFrame>;
    function GetErrorMessage: string;
    procedure SetErrorMessage(const Value: string);
  public
    [BeforeShow]
    procedure BeforeShow;

    property ErrorMessage: string read GetErrorMessage write SetErrorMessage;
  end;

implementation

{$R *.fmx}

{ TErrorFrame }

procedure TErrorFrame.BeforeShow;
begin
  TDelayedAction.Execute(5000
  , procedure
    begin
      if Assigned(FrameInfo) and FrameInfo.IsVisible then
        FrameInfo.Hide(0
          , procedure
            begin
              FrameInfo.Close;
            end
        );
    end
  );
end;

function TErrorFrame.GetErrorMessage: string;
begin
  Result := MessageLabel.Text;
end;

procedure TErrorFrame.SetErrorMessage(const Value: string);
begin
  MessageLabel.Text := Value;
end;

end.
