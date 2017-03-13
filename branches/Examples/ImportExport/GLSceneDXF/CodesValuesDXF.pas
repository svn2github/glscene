{: CodesValuesDXF<p>

  DXF codes and values<p>

  <b>History :</b><font size=-1><ul>
    <li>25/01/03 - DA - Moved InterpretCodes and EndOfPart methods in the TDXFIOObject object,
                        Moved TDXFIOObject to the TypesDXF unit,
                        Renamed this unit to CodesValuesDXF
    <li>21/01/03 - DA - Added the SaveToStream method to the TDXFIOObject
    <li>06/12/02 - DA - Fixed a bug when opening a blanck file
    <li>23/11/02 - DA - Added TDXFIOObject with base DXF IO operations
    <li>13/10/02 - DA - Added sections, header and entities constants
    <li>05/10/02 - SJ - Unit creation, added consts
  </ul></font>
}

Unit CodesValuesDXF;

interface

const

  //: DXF codes
  DXF_START            = 0;
  DXF_TEXT_DEF         = 1;
  DXF_NAME             = 2;
  DXF_text_prompt      = 3;
  DXF_othername2       = 4;
  DXF_entity_handle    = 5;
  DXF_LINE_TYPE        = 6;
  DXF_TEXT_STYLE       = 7;
  DXF_LAYER_NAME       = 8;
  DXF_VAR_NAME         = 9;
  DXF_PRIMARY_X        = 10;
  DXF_PRIMARY_Y        = 20;
  DXF_PRIMARY_Z        = 30;
  DXF_OTHER_X_1        = 11;
  DXF_OTHER_Y_1        = 21;
  DXF_OTHER_Z_1        = 31;
  DXF_OTHER_X_2        = 12;
  DXF_OTHER_Y_2        = 22;
  DXF_OTHER_Z_2        = 32;
  DXF_OTHER_X_3        = 13;
  DXF_OTHER_Y_3        = 23;
  DXF_OTHER_Z_3        = 33;
  DXF_elevation        = 38;
  DXF_THICKNESS        = 39;
  DXF_FLOATVAL         = 40;
  DXF_FLOATVALS1       = 41;
  DXF_FLOATVALS2       = 42;
  DXF_FLOATVALS3       = 43;
  DXF_repeat           = 49;
  DXF_ANGLE1           = 50;
  DXF_ANGLE2           = 51;
  DXF_angle3           = 52;
  DXF_angle4           = 53;
  DXF_angle5           = 54;
  DXF_angle6           = 55;
  DXF_angle7           = 56;
  DXF_angle8           = 57;
  DXF_angle9           = 58;
  DXF_VISIBLE          = 60;
  DXF_COLORNUM         = 62;
  DXF_ENTITIES_FLG     = 66;
  DXF_ent_ident        = 67;
  DXF_view_state       = 69;
  DXF_70FLAG           = 70;
  DXF_71FLAG           = 71;
  DXF_72FLAG           = 72;
  DXF_73FLAG           = 73;
  DXF_74FLAG           = 74;
  DXF_75FLAG           = 75;  
  DXF_EXTRUSIONX       = 210;
  DXF_EXTRUSIONY       = 220;
  DXF_EXTRUSIONZ       = 230;
  DXF_comment          = 999;

  // DXF sections
  DXFV_SECTION          = 'SECTION';
  DXFV_ENDSEC           = 'ENDSEC';
  DXFV_HEADER           = 'HEADER';
  DXFV_CLASSES          = 'CLASSES';
  DXFV_TABLES           = 'TABLES';
  DXFV_TABLE            = 'TABLE';
  DXFV_ENDTAB           = 'ENDTAB';      
  DXFV_BLOCKS           = 'BLOCKS';
  DXFV_ENTITIES         = 'ENTITIES';
  DXFV_OBJECTS          = 'OBJECTS';
  DXFV_THUMBNAILIMAGE   = 'THUMBNAILIMAGE';

  // header variables
  DXFV_EXTMAX           = '$EXTMAX';
  DXFV_EXTMIN           = '$EXTMIN';
  DXFV_CECOLOR          = '$CECOLOR';

  // tables section related
  DXFV_LAYER            = 'LAYER';

  // blocks section related
  DXFV_BLOCK            = 'BLOCK';
  DXFV_ENDBLK           = 'ENDBLK';
    
  // entities
  DXFV_ARC              = 'ARC';
  DXFV_CIRCLE           = 'CIRCLE';
  DXFV_INSERT           = 'INSERT';
  DXFV_LINE             = 'LINE';
  DXFV_LWPOLYLINE       = 'LWPOLYLINE';
  DXFV_MLINE            = 'MLINE';
  DXFV_MTEXT            = 'MTEXT';
  DXFV_POINT            = 'POINT';
  DXFV_POLYLINE         = 'POLYLINE';
  DXFV_SPLINE           = 'GLSpline';
  DXFV_TEXT             = 'TEXT';
  DXFV_VERTEX           = 'VERTEX';
  DXFV_3DFACE           = '3DFACE';
  DXFV_SOLID            = 'SOLID';

  // polyline
  DXFV_SEQEND           = 'SEQEND';

implementation

end.