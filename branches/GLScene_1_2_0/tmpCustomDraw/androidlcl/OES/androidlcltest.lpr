library androidlcltest;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}   //многопоточность не врубилась пришлось изнасиловать и врубить самому!
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  customdrawnint,
  Interfaces,
  Forms,
  uDev, GLScene_Runtime;

exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLOnDraw name 'Java_com_pascal_lcltest_LCLActivity_LCLOnDraw',
  Java_com_pascal_lclproject_LCLActivity_LCLOnCreate name 'Java_com_pascal_lcltest_LCLActivity_LCLOnCreate',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMessageBoxFinished',
  Java_com_pascal_lclproject_LCLActivity_LCLOnKey name 'Java_com_pascal_lcltest_LCLActivity_LCLOnKey',
  Java_com_pascal_lclproject_LCLActivity_LCLOnTimer name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTimer',
  Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnConfigurationChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnSensorChanged',

  Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceCreated name 'Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceCreated',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceDestroyed name 'Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceDestroyed',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceChanged name 'Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceChanged',

  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
begin
  Application.Initialize;
  Application.CreateForm(TGLMainForm5, GLMainForm5);
  Application.Run;
end;

begin
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
end.

