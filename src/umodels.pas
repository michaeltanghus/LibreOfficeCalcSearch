unit uModels;

{$mode objfpc}{$H+}

interface

type
  TResultMode = (rmCellOnly, rmWholeRow, rmWholeColumn);

  TSearchCriteria = record
    QueryText: string;
    Directory: string;
    FileMask: string;      // e.g. "*.ods;*.xlsx"
    SearchSubfolders: Boolean;

    MatchCase: Boolean;
    WholeWord: Boolean;
    UseRegex: Boolean;

    ResultMode: TResultMode;
  end;

  TSearchHit = record
    FilePath: string;
    SheetName: string;
    RowIndex: Cardinal;  // 0-based
    ColIndex: Cardinal;  // 0-based
    CellA1: string;
    CellText: string;
    ContextText: string;
  end;

implementation

end.

