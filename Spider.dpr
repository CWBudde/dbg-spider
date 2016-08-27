program Spider;

uses
  FastMM4,
  Vcl.Themes,
  Vcl.Styles,
  Forms,
  ClassUtils in 'ClassUtils.pas',
  DbgHookTypes in 'DbgHookTypes.pas',
  Debuger in 'Debuger.pas',
  DebugerTypes in 'DebugerTypes.pas',
  DebugHook in 'DebugHook.pas',
  DebugInfo in 'DebugInfo.pas',
  DelphiDebugInfo in 'DelphiDebugInfo.pas',
  JclPeImage in 'JclPeImage.pas',
  JclTD32Ex in 'JclTD32Ex.pas',
  WinAPIUtils in 'WinAPIUtils.pas',
  Collections.Bags in 'Collections\Collections.Bags.pas',
  Collections.Base in 'Collections\Collections.Base.pas',
  Collections.BidiDictionaries in 'Collections\Collections.BidiDictionaries.pas',
  Collections.BidiMaps in 'Collections\Collections.BidiMaps.pas',
  Collections.Dictionaries in 'Collections\Collections.Dictionaries.pas',
  Collections.Dynamic in 'Collections\Collections.Dynamic.pas',
  Collections.Lists in 'Collections\Collections.Lists.pas',
  Collections.MultiMaps in 'Collections\Collections.MultiMaps.pas',
  Collections.Queues in 'Collections\Collections.Queues.pas',
  Collections.Serialization in 'Collections\Collections.Serialization.pas',
  Collections.Sets in 'Collections\Collections.Sets.pas',
  Collections.Stacks in 'Collections\Collections.Stacks.pas',
  CollectList in 'CollectList.pas',
  GdiPlus in 'GdiPlus\GdiPlus.pas',
  GdiPlusHelpers in 'GdiPlus\GdiPlusHelpers.pas',
  MapDebugInfo in 'MapDebugInfo.pas',
  uActionController in 'uActionController.pas',
  uDebugerThread in 'uDebugerThread.pas',
  uExceptionHook in 'uExceptionHook.pas',
  uFeedback in 'uFeedback.pas' {frmFeedback},
  uGA in 'uGA.pas',
  uMain in 'uMain.pas' {MainForm},
  uProcessList in 'uProcessList.pas' {frmProcessList},
  uProjectOptions in 'uProjectOptions.pas' {fmProjectOptions},
  uRWLock in 'uRWLock.pas',
  uSelectSource in 'uSelectSource.pas' {fmSelectSource},
  uShareData in 'uShareData.pas' {dmShareData: TDataModule},
  uSharedObject in 'uSharedObject.pas',
  uSourceViewFrame in 'uSourceViewFrame.pas' {SourceViewFrame: TFrame},
  uSpiderOptions in 'uSpiderOptions.pas',
  uSQLiteDB in 'uSQLiteDB.pas',
  uUpdateInfo in 'uUpdateInfo.pas',
  DbgMemoryProfiler in 'DbgMemoryProfiler.pas',
  DbgWorkerThread in 'DbgWorkerThread.pas',
  DbgSyncObjsProfiler in 'DbgSyncObjsProfiler.pas',
  DbgSamplingProfiler in 'DbgSamplingProfiler.pas',
  DbgCodeProfiler in 'DbgCodeProfiler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Spider';
  Application.CreateForm(TdmShareData, dmShareData);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
