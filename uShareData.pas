unit uShareData;

interface

uses
  SysUtils, Classes, ImgList, Controls, XPMan, ActnMan, ActnColorMaps,
  SynEditHighlighter, SynHighlighterPas, SynEditMiscClasses,
  SynEditRegexSearch, SynEditSearch, SynEditOptionsDialog;

type
  TdmShareData = class(TDataModule)
    ilActionsSmall: TImageList;
    imlMainSmall: TImageList;
    imlMain: TImageList;
    synPas: TSynPasSyn;
    synRegexSearch: TSynEditRegexSearch;
    synEditSearch: TSynEditSearch;
    synEditOptDlg: TSynEditOptionsDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmShareData: TdmShareData;

implementation

{$R *.dfm}

end.
