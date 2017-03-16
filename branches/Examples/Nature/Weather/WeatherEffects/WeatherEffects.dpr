////////////////////////////////////////////////////////////////////////
//  GLScene Rain Demo v1.0                                            //
//====================================================================//
//  демонстраци€ получени€ "простого" дожд€, снега и тумана           //
//--------------------------------------------------------------------//
//                                                                    //
//  ѕри ипользовании частиц основна€ сложность - куча настроек,       //
//  где начинающим всЄ кажетс€ лишним и непон€тным...                 //
//                                                                    //
// Ђƒождьї                                                            //
//  использу€ PointLightPFX c AspectRatio в 0,05 получаем линию, похо //
//  жую на каплю в движении, если задать нужный угол (Rotation), ско  //
//  рость (PFX Source->InitialVelocity) и разброс (PFX Source->Positi //
//  onDispersion), то получаетс€ неплохой эффект дожд€ =)             //
//  дл€ создани€ эффекта частиц надо:                                 //
//  Х Scene objects->Add object->Particle Systems->PFX Renderer       //
//  Х создаем PointLightPFXManager из вкладки "GLScene PFX"           //
//  Х подключаем к нему "PFX Renderer" и работающий GLCadencer        //
//  Х в объекте-"источнике" добавл€ем Effects->Add->PFX Source        //
//  всЄ, "источник" создан, теперь надо выставить параметры:          //
//  Х PointLightPFXManager->AspectRatio=0.05 тонкие частицы           //
//  Х PointLightPFXManager->ParticleSize=0.1 делаем их помельче       //
//  Х PointLightPFXManager->ColorInner=[1,1,1,0.5] полупрозрачные     //
//  Х PFX Source->ParticleInterval=0.001 чем меньше, тем больше       //
//  Х PFX Source->PositionDispersionRange=[1,0,1] разброс по ос€м     //
//  Х PFX Source->PositionDispersion=5 множитель разброса             //
//  ѕараметры выставлены, теперь через GLCadencer будем управл€ть     //
//  PFX Source->InitialPosition и PFX Source->InitialVelocity,        //
//  задава€ нужный угол частиц через PointLightPFXManager->Rotation   //
//                                                                    //
// Ђ—негї                                                             //
//  создание снега аналогично созданию "ƒожд€":                       //
//  Х создаем PointLightPFXManager из вкладки "GLScene PFX"           //
//  Х подключаем к нему "PFX Renderer" и работающий GLCadencer        //
//  Х в объекте-"источнике" добавл€ем Effects->Add->PFX Source        //
//  теперь выставл€ем параметры:                                      //
//  Х PointLightPFXManager->AspectRatio=0.5 легкое искажение формы    //
//  Х PointLightPFXManager->ParticleSize=0.1 делаем помельче          //
//  Х PointLightPFXManager->ColorInner=clrWhite белые пушинки         //
//  Х PFX Source->ParticleInterval=0.001 чем меньше, тем больше       //
//  Х PFX Source->PositionDispersionRange=[1,0,1] разброс по ос€м     //
//  Х PFX Source->PositionDispersion=5 множитель разброса             //
//  Х PFX Source->VelocityDispersion=1 разлЄт частиц                  //
//  параметры выставлены, а через GLCadencer также будем управл€ть    //
//  PFX Source->InitialPosition и PFX Source->InitialVelocity         //
//                                                                    //
// Ђ“уманї                                                            //
//  клубы пара - созданы использу€ PerlinPFX дл€ придани€ детализации //
//  создание подобно пердыдущим:                                      //
//  Х создаем PerlinPFXManager из вкладки "GLScene PFX"               //
//  Х подключаем к нему "PFX Renderer" и работающий GLCadencer        //
//  Х в объекте-"источнике" добавл€ем Effects->Add->PFX Source        //
//  выставл€ем параметры:                                             //
//  Х PointLightPFXManager->Brightness=0.2 приглушаем €ркость         //
//  Х PointLightPFXManager->ColorMode=scmFade более м€гка€ текстурка  //
//  Х PointLightPFXManager->ColorInner=[1,1,1,0.5] чуть прозрачности  //
//  Х PointLightPFXManager->ParticleSize=1.5 крупные клубы пара       //
//  Х PFX Source->InitialPosition=[0,-3,0] эмиттер создаем внизу      //
//  Х PFX Source->InitialVelocity=[0,0.5,0] и направл€ем вверх        //
//  Х PFX Source->ParticleInterval=0.005 чем меньше, тем хуже         //
//  Х PFX Source->PositionDispersionRange=[4,0,1] разброс вдоль линии //
//  Х PFX Source->RotationDispersion=1 немного вращени€               //
//  Х PFX Source->VelocityDispersion=1 разлЄт частиц                  //
//                                                                    //
//====================================================================//
//  GLScene.ru                                                        //
////////////////////////////////////////////////////////////////////////

program WeatherEffects;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
