library androidlcltest;

{$mode objfpc}{$H+}

uses
  customdrawnint,
  Interfaces,
  Forms,
  mainform, GLScene_Runtime{, customdrawn_android, customdrawndrawers};

exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLOnDraw name 'Java_com_pascal_lcltest_LCLActivity_LCLOnDraw',
  Java_com_pascal_lclproject_LCLActivity_LCLOnCreate name 'Java_com_pascal_lcltest_LCLActivity_LCLOnCreate',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMessageBoxFinished',
  Java_com_pascal_lclproject_LCLActivity_LCLOnKey name 'Java_com_pascal_lcltest_LCLActivity_LCLOnKey',
  Java_com_pascal_lclproject_LCLActivity_LCLOnTimer name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTimer',
  Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnConfigurationChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnSensorChanged',

  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end;

begin
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
 // CDWidgetset.CreateOpenGLContext;
end.

