*&---------------------------------------------------------------------*
*&  Include           ZASSETRAK_CONTROLLER_SUB
*&---------------------------------------------------------------------*


FORM FTP_FILE  USING    PIV_FILENAME_S
                        PIV_FILENAME_T
                        PIT_LINES TYPE TABLE   "TYPE zsyst_tabix
                        PIT_DATA TYPE TRUXS_T_TEXT_DATA.

  CLEAR GV_FILE.

*  DATA : result type.

  DATA:  LV_USER(30) TYPE C , "user name of ftp server
         LV_PWD(30) TYPE C, " VALUE 'Au@@1234', "scrambled password of ftp server
         LV_HOST(64) TYPE C , "ip address of FTP server
         LV_DEST LIKE RFCDES-RFCDEST VALUE 'SAPFTPA'."Background RFC destination
  DATA: LV_HDL TYPE I,
        GC_KEY TYPE I VALUE 26101957,
        LV_SLEN TYPE I.
  DATA : RESULT TYPE TRUXS_T_TEXT_DATA.
*  CONCATENATE piv_filename '.TXT' INTO gv_file .


*HTTP_SCRAMBLE: used to scramble the password provided in a format recognized by SAP.
  SET EXTENDED CHECK OFF.
  LV_SLEN = STRLEN( LV_PWD ).

  LV_USER = 'tirupaticor\arssc'.
  LV_PWD = 'arbl@123'.
  LV_HOST = '10.111.0.21'.
  SET EXTENDED CHECK OFF.
  LV_SLEN = STRLEN( LV_PWD ).
  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE      = LV_PWD
      SOURCELEN   = 8
      KEY         = GC_KEY
    IMPORTING
      DESTINATION = LV_PWD.


*  SELECT SINGLE low FROM tvarvc
*         INTO lv_user WHERE name = 'ZFTP_USER'.
*  IF sy-subrc NE 0.
*    lv_user = 'ASTRFTPUSR'.
*      ENDIF.


*  SELECT SINGLE low FROM tvarvc
*         INTO lv_pwd WHERE name = 'ZFTP_SCRAMBLE_PWD'.
*  IF sy-subrc NE 0.
*    lv_pwd = '87C80B5732193F5B'.
*  ENDIF.
*
**   lv_pwd = 'Eshaan@345'.
*  lv_host = '10.111.0.21'.

*   SET EXTENDED CHECK OFF.
*  lv_slen = strlen( lv_pwd ).
*  SELECT SINGLE low FROM tvarvc
*         INTO lv_host WHERE name = 'ZFTP_HOST'.
*  IF sy-subrc NE 0.

*  ENDIF.

* To Connect to the Server using FTP

  CLEAR LV_HDL.
  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      USER            = LV_USER
      PASSWORD        = LV_PWD
      HOST            = LV_HOST
      RFC_DESTINATION = LV_DEST
    IMPORTING
      HANDLE          = LV_HDL
    EXCEPTIONS
      OTHERS          = 1.

  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      HANDLE        = LV_HDL
      COMMAND       = 'set passive on'
    TABLES
      DATA          = RESULT
    EXCEPTIONS
      COMMAND_ERROR = 1
      TCPIP_ERROR   = 2.

  IF PIT_LINES IS NOT INITIAL.

*FTP_R3_TO_SERVER:used to transfer the internal table data as a file to other system in the character mode.
    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        HANDLE         = LV_HDL
        FNAME          = PIV_FILENAME_S          "file path of destination system
        CHARACTER_MODE = 'X'
      TABLES
        TEXT           = PIT_LINES
      EXCEPTIONS
        TCPIP_ERROR    = 1
        COMMAND_ERROR  = 2
        DATA_ERROR     = 3
        OTHERS         = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
      RAISING INVALID_OUTPUT_FILE.
    ENDIF.

  ENDIF.
* FTP_R3_TO_SERVER:used to transfer the internal table data as a file to other system in the character mode.
  CALL FUNCTION 'FTP_R3_TO_SERVER'
    EXPORTING
      HANDLE         = LV_HDL
      FNAME          = PIV_FILENAME_T          "file path of destination system
      CHARACTER_MODE = 'X'
    TABLES
      TEXT           = PIT_DATA
    EXCEPTIONS
      TCPIP_ERROR    = 1
      COMMAND_ERROR  = 2
      DATA_ERROR     = 3
      OTHERS         = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
    RAISING INVALID_OUTPUT_FILE.

  ENDIF.

*FTP_DISCONNECT: This is used to disconnect the connection between SAP and other system.
* To disconnect the FTP
  CALL FUNCTION 'FTP_DISCONNECT'
    EXPORTING
      HANDLE = LV_HDL.
**RFC_CONNECTION_CLOSE:This is used to disconnect the RFC connection between SAP and other system.
*  CALL FUNCTION 'RFC_CONNECTION_CLOSE'
*    EXPORTING
*      destination = lv_dest
*    EXCEPTIONS
*      OTHERS      = 1.

ENDFORM.                    " FTP_FILE
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_MASTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXTRACT_MASTER_DATA .

  DATA LV_HEADER(4096) TYPE C.
  DATA LT_TAB_DELIMITED TYPE TRUXS_T_TEXT_DATA .
  DATA LT_DATA TYPE TRUXS_T_TEXT_DATA .
  DATA LT_XRESPONSE TYPE STANDARD TABLE OF TY_XRESPONSE.
  DATA LV_XLENGTH TYPE I.
  DATA LV_FILENAME TYPE STRING.


  IF GT_ASSET_COMPANY IS INITIAL.

    MESSAGE 'Please maintain Company Code in ZASSETRAK_COMPNY table' TYPE 'E'.

  ELSE.

* select company code data
    SELECT T001~BUKRS NAME1 NAME2 STREET CITY1 BEZEI ADRC~POST_CODE1 LANDX TEL_NUMBER FAX_NUMBER ZASSETRAK_COMPNY~WAERS
      INTO TABLE GT_COMPANYDATA
      FROM T001 INNER JOIN ZASSETRAK_COMPNY ON T001~BUKRS = ZASSETRAK_COMPNY~BUKRS
      INNER JOIN ADRC ON T001~ADRNR = ADRC~ADDRNUMBER
      INNER JOIN T005T ON T005T~LAND1 = ADRC~COUNTRY
      INNER JOIN T005U ON T005U~LAND1 = ADRC~COUNTRY AND T005U~BLAND = ADRC~REGION
      FOR ALL ENTRIES IN GT_ASSET_COMPANY
      WHERE T001~BUKRS = GT_ASSET_COMPANY-BUKRS AND T005T~SPRAS = SY-LANGU AND T005U~SPRAS = SY-LANGU..

    IF SY-SUBRC = 0.

      CLEAR: LT_DATA, LT_TAB_DELIMITED, LV_HEADER.

      CONCATENATE 'Company_code' 'Name1' 'Name2' 'Address' 'City' 'Region' 'Postal_code' 'Country' 'Telephone' 'Fax' 'Currency'
      INTO LV_HEADER SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

      APPEND LV_HEADER TO LT_DATA.


      CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
*         I_LINE_HEADER        =
*         I_FILENAME           =
*         I_APPL_KEEP          = ' '
        TABLES
          I_TAB_SAP_DATA       = GT_COMPANYDATA
        CHANGING
          I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
        EXCEPTIONS
          CONVERSION_FAILED    = 1
          OTHERS               = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        MESSAGE 'Error in converting Company data to TAB Delimited format' TYPE 'E'.
      ELSE.

        APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

        PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_COMPANYDATA.



        PERFORM CREATE_FILE USING 'Company_data' 1 LT_DATA.


      ENDIF.
    ENDIF.

* Plant data

    SELECT T001K~BUKRS WERKS NAME1 NAME2 STRAS PSTLZ ORT01 BEZEI LANDX  FROM T001K INNER JOIN T001W ON T001K~BWKEY = T001W~BWKEY
            INNER JOIN T005T ON T005T~LAND1 = T001W~LAND1
            INNER JOIN T005U ON T005U~LAND1 = T001W~LAND1 AND T005U~BLAND = T001W~REGIO
*            inner join t001w on t001w~werks = t499s~werks and t001w~adrnr  = t499s~addrnum
      INTO CORRESPONDING FIELDS OF TABLE GT_PLANTDATA
      FOR ALL ENTRIES IN GT_ASSET_COMPANY
      WHERE
      BUKRS = GT_ASSET_COMPANY-BUKRS AND
      T005T~SPRAS = SY-LANGU AND
            T005U~SPRAS = SY-LANGU.


    LOOP AT GT_PLANTDATA INTO WA_PLANTDATA.
      WA_PLANTDATA-WERKS1 = WA_PLANTDATA-WERKS.
      MODIFY GT_PLANTDATA FROM WA_PLANTDATA TRANSPORTING WERKS1 .
    ENDLOOP.


    SELECT WERKS STAND KTEXT FROM T499S INTO TABLE IT_T499S WHERE WERKS = '1000'.

    LOOP AT IT_T499S INTO WA_T499S.
      READ TABLE GT_PLANTDATA INTO WA_PLANTDATA WITH KEY  WERKS = '1000'.
      .
      CONCATENATE WA_T499S-WERKS WA_T499S-STAND INTO WA_PLANTDATA-WERKS.
      WA_PLANTDATA-NAME1 = WA_T499S-KTEXT.
      INSERT WA_PLANTDATA INTO TABLE GT_PLANTDATA." FROM  TRANSPORTING werks.
*
    ENDLOOP.

    GT_PLANTDATA1[] = GT_PLANTDATA.
    GT_PLANTDATA3[] = GT_PLANTDATA.
    REFRESH GT_PLANTDATA.
    BREAK 1000868.
    LOOP AT GT_PLANTDATA3 INTO WA_PLANTDATA2.
      WA_PLANTDATA3-BUKRS = WA_PLANTDATA2-BUKRS.
      WA_PLANTDATA3-WERKS = WA_PLANTDATA2-WERKS.
      WA_PLANTDATA3-NAME1 = WA_PLANTDATA2-NAME1.
      WA_PLANTDATA3-NAME2 = WA_PLANTDATA2-NAME2.
      WA_PLANTDATA3-STRAS = WA_PLANTDATA2-STRAS.
      WA_PLANTDATA3-PSTLZ =  WA_PLANTDATA2-PSTLZ.
      WA_PLANTDATA3-ORT01 =  WA_PLANTDATA2-ORT01.
      WA_PLANTDATA3-BEZEI = WA_PLANTDATA2-BEZEI.
      WA_PLANTDATA3-LANDX = WA_PLANTDATA2-LANDX.
      APPEND WA_PLANTDATA3 TO GT_PLANTDATA.

    ENDLOOP.
*  break 1000868.
*      gt_plantdata[] = gt_plantdata3[].

    DELETE GT_PLANTDATA WHERE WERKS = '1000'.
    IF SY-SUBRC = 0.

      CLEAR: LT_DATA, LT_TAB_DELIMITED, LV_HEADER.

      CONCATENATE 'Company_code' 'Plant' 'Name1' 'Name2' 'Address' 'Postal_code' 'City' 'Region' 'Country'
*      'plant_storage_code' 'plant_storage_des'
      INTO LV_HEADER SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

      APPEND LV_HEADER TO LT_DATA.

      CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
        TABLES
          I_TAB_SAP_DATA       = GT_PLANTDATA
        CHANGING
          I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
        EXCEPTIONS
          CONVERSION_FAILED    = 1
          OTHERS               = 2.
      IF SY-SUBRC <> 0.

        MESSAGE 'Error in converting Plant data to TAB Delimited format' TYPE 'E'.

      ELSE.

        APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*        conver data to bin format
        PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_PLANTDATA.

        PERFORM CREATE_FILE USING 'Plant_data' 1 LT_DATA.

      ENDIF.

*      PERFORM ftp_file USING 'PLANTDATA'.

*     select Plant/location

      SELECT WERKS STAND KTEXT INTO TABLE GT_PLANTLOCATIONDATA
        FROM T499S
        FOR ALL ENTRIES IN GT_PLANTDATA1
        WHERE WERKS = GT_PLANTDATA1-WERKS1.


*        gt_plantlocationdata1[] = gt_plantlocationdata[].
*       BREAK 1000868.
*        loop at gt_plantlocationdata into wa_plantlocationdata where werks1 = '1000'.
*
**          if wa_plantlocationdata-werks = '1000'.
*            CONCATENATE wa_plantlocationdata-werks1 wa_plantlocationdata-stand INTO wa_plantlocationdata-plant_storage_code.
**            ENDIF.
**          modify wa_plantlocationdata.
*          modify gt_plantlocationdata FROM wa_plantlocationdata TRANSPORTING plant_storage_code.
*
*          ENDLOOP.


*      IF gt_plantlocationdata is NOT INITIAL.

      CLEAR: LT_DATA, LT_TAB_DELIMITED, LV_HEADER.

      CONCATENATE 'Plant' 'Sorage_Location_code' 'Storage_Location_name'
      INTO LV_HEADER SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

      APPEND LV_HEADER TO LT_DATA.


      CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
        TABLES
          I_TAB_SAP_DATA       = GT_PLANTLOCATIONDATA
        CHANGING
          I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
        EXCEPTIONS
          CONVERSION_FAILED    = 1
          OTHERS               = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        MESSAGE 'Error in converting Plant Sorage Location data to TAB Delimited format' TYPE 'E'.
      ELSE.
        APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*        conver data to bin format

        PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_PLANTSTORAGELOCATION.

        PERFORM CREATE_FILE USING 'Plant_StorageLocation_Data' 1 LT_DATA.

      ENDIF.

*      select asset class and description
      SELECT ANLKL TXK20 TXK50 INTO TABLE GT_ASSET_CLASSES
        FROM ANKT
        WHERE SPRAS = SY-LANGU.

      IF SY-SUBRC = 0.

        CLEAR: LT_DATA, LT_TAB_DELIMITED, LV_HEADER.

        CONCATENATE 'Asset_class' 'Short_desc' 'Long_desc'
        INTO LV_HEADER SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

        APPEND LV_HEADER TO LT_DATA.

        CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
          EXPORTING
            I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
          TABLES
            I_TAB_SAP_DATA       = GT_ASSET_CLASSES
          CHANGING
            I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
          EXCEPTIONS
            CONVERSION_FAILED    = 1
            OTHERS               = 2.
        IF SY-SUBRC <> 0.
*             Implement suitable error handling here
          MESSAGE 'Error in converting Asset classes data to TAB Delimited format' TYPE 'E'.
        ELSE.

          APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*        conver data to bin format

          PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_ASSETCLASS.

          PERFORM CREATE_FILE USING 'Asset_Class_Data' 1 LT_DATA.

        ENDIF.

      ENDIF.

*      ENDIF.

    ENDIF.

  ENDIF.

  IF LT_DATA IS NOT INITIAL.
    MESSAGE 'Files Succesfully Downloaded to FTP  Server' TYPE 'S' .
  ELSE.
    MESSAGE 'No data found' TYPE 'S' .
  ENDIF.

ENDFORM.                    " EXTRACT_MASTER_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_OUTPUT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_OUTPUT_TABLE CHANGING PCO_TABLE TYPE REF TO DATA
                                 PCV_FIELDLIST TYPE STRING
                                 PCV_IMPORTLIST TYPE STRING
                                 PCV_LINE1 TYPE C
                                 PCV_LINE2 TYPE C .


  DATA LS_CONFIG TYPE ZASSETRAK_CONFIG.

  DATA LT_DD03L TYPE STANDARD TABLE OF DD03L.
  DATA LS_DD03L TYPE DD03L.

  DATA LS_FIELD TYPE ABAP_COMPDESCR.
  DATA LT_FIELD TYPE ABAP_COMPDESCR_TAB.

  DATA LV_FIELD TYPE STRING.
  DATA LT_TOT_COMP    TYPE CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE.
  DATA LS_COMP TYPE ABAP_COMPONENTDESCR.

  DATA    LO_NEW_TYPE    TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA    LO_TABLE_TYPE  TYPE REF TO CL_ABAP_TABLEDESCR.

  DATA    LO_DY_LINE      TYPE REF TO DATA.

  DATA LV_FILENAME TYPE CHAR200.
  DATA LT_TAB_DELIMITED TYPE TRUXS_T_TEXT_DATA .
  DATA LT_DATA TYPE TRUXS_T_TEXT_DATA .
  DATA LV_HEADER(4096) TYPE C.
  DATA LT_LINES TYPE STANDARD TABLE OF SY-TABIX.

  SELECT * INTO TABLE GT_CONFIG FROM ZASSETRAK_CONFIG.

  SORT GT_CONFIG BY SEQUENCE.

  IF P_CDATA = GC_X AND PR_EXP = GC_X.



    CONCATENATE 'Client' 'Sequence Number' 'Field Name' 'Table Name' 'Field Name' 'Hardcoded Source'
                'Export' 'Import Field' 'Review '
          INTO LV_HEADER SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

    APPEND LV_HEADER TO LT_DATA.

    CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
      EXPORTING
        I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
      TABLES
        I_TAB_SAP_DATA       = GT_CONFIG
      CHANGING
        I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
      EXCEPTIONS
        CONVERSION_FAILED    = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.

      MESSAGE 'Error in converting Configuration data to TAB Delimited format' TYPE 'E'.

    ELSE.

      APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*        conver data to bin format
      PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_PLANTDATA.

      CLEAR LV_FILENAME.
      CONCATENATE GV_PATH 'Configuration_Data' GV_DATETIME '.txt' INTO LV_FILENAME.

*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename = lv_filename
*          filetype = 'ASC'
*        TABLES
*          data_tab = lt_data.


      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      PERFORM FTP_FILE USING  LV_FILENAME
                              LV_FILENAME
                       CHANGING LT_LINES
                                LT_DATA  .
      PERFORM TRANSFER_FILE USING  LV_FILENAME
                                   LV_FILENAME
                            CHANGING LT_LINES
                                     LT_DATA  .
    ENDIF.


  ENDIF.



  IF  GT_CONFIG IS NOT INITIAL.

    GT_EXPORT = GT_CONFIG.
*   keep exportable fields only
    DELETE GT_EXPORT WHERE EXPORT = SPACE .


    SELECT * INTO TABLE LT_DD03L FROM DD03L
      FOR ALL ENTRIES IN
      GT_CONFIG
      WHERE
      TABNAME = GT_CONFIG-TABLENAME AND
      FIELDNAME = GT_CONFIG-SAPFIELD AND
      AS4LOCAL = 'A' AND
      AS4VERS = '000'.

    IF SY-SUBRC = 0.



      CLEAR PCV_FIELDLIST.
      LOOP AT GT_CONFIG INTO LS_CONFIG.

*       for select query
        IF LS_CONFIG-TABLENAME <> 'ZASSETRAK'. " AND ls_config-export <> 'DUMMY' .

          IF LS_CONFIG-EXPORT <> SPACE.
            CLEAR LV_FIELD.
            CONCATENATE LS_CONFIG-TABLENAME '~' LS_CONFIG-SAPFIELD INTO LV_FIELD.

*            build select query
            CONCATENATE PCV_FIELDLIST  LV_FIELD INTO PCV_FIELDLIST SEPARATED BY SPACE.

          ENDIF.

*         for import field list
          IF LS_CONFIG-IMPORT <> SPACE.
            CLEAR LV_FIELD.
            CONCATENATE LS_CONFIG-TABLENAME '-' LS_CONFIG-SAPFIELD INTO LV_FIELD.

*           build import list
            CONCATENATE PCV_FIELDLIST  LV_FIELD INTO PCV_IMPORTLIST SEPARATED BY SPACE.

          ENDIF.

        ENDIF.

*       build SAP table field list for export as LINE2
        IF PCV_LINE2 IS INITIAL.
          CONCATENATE LS_CONFIG-TABLENAME '-' LS_CONFIG-SAPFIELD  CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO PCV_LINE2.
        ELSE.
          CONCATENATE PCV_LINE2 LS_CONFIG-TABLENAME '-' LS_CONFIG-SAPFIELD  CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO PCV_LINE2.
        ENDIF.



*       build assetrak field list for export as LINE1
        IF PCV_LINE1 IS INITIAL.
          CONCATENATE LS_CONFIG-ASSETRAKFIELD CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO PCV_LINE1.
        ELSE.
          CONCATENATE PCV_LINE1 LS_CONFIG-ASSETRAKFIELD CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO PCV_LINE1 .
        ENDIF.


*       to build internal table
        READ TABLE LT_DD03L INTO LS_DD03L WITH KEY TABNAME = LS_CONFIG-TABLENAME
                                                    FIELDNAME = LS_CONFIG-SAPFIELD.
        IF SY-SUBRC = 0.


          CLEAR LS_FIELD.
          LS_FIELD-NAME      = LS_DD03L-FIELDNAME.
          IF LS_FIELD-NAME = 'RAUMN'.
            LS_FIELD-LENGTH    = 30.
          ELSE.
            LS_FIELD-LENGTH    = LS_DD03L-LENG.
          ENDIF.
          LS_FIELD-TYPE_KIND = LS_DD03L-INTTYPE.         "Char field
          LS_FIELD-DECIMALS = LS_DD03L-DECIMALS.

          CLEAR LS_COMP.

          CASE LS_FIELD-TYPE_KIND.
            WHEN 'STRING'.  LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_STRING( ).
            WHEN 'XSTRING'. LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_XSTRING( ).
            WHEN 'I'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_I( ).
            WHEN 'F'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_F( ).
            WHEN 'D'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_D( ). " cl_abap_elemdescr=>get_c( p_length = 8 ). "
            WHEN 'T'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_T(  ). " cl_abap_elemdescr=>get_c( p_length = 6 ). "
            WHEN 'C'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_C( P_LENGTH = LS_FIELD-LENGTH ).
            WHEN 'N'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_N( P_LENGTH = LS_FIELD-LENGTH ).
            WHEN 'X'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_X( P_LENGTH = LS_FIELD-LENGTH ).
            WHEN 'P'.       LS_COMP-TYPE = CL_ABAP_ELEMDESCR=>GET_P( P_LENGTH = LS_FIELD-LENGTH P_DECIMALS = LS_FIELD-DECIMALS ).
          ENDCASE.


          LS_COMP-NAME = LS_DD03L-FIELDNAME.   "Field name   ex: FIELD1
          APPEND LS_COMP TO LT_TOT_COMP.       "Add entry to component table

          CLEAR LS_DD03L.
        ENDIF.

      ENDLOOP.


* Create new type from component table
      LO_NEW_TYPE = CL_ABAP_STRUCTDESCR=>CREATE( LT_TOT_COMP ).

* Create new table type
      LO_TABLE_TYPE = CL_ABAP_TABLEDESCR=>CREATE( LO_NEW_TYPE ).

* Create dynamic internal table and assign to Field Symbol
      CREATE DATA PCO_TABLE TYPE HANDLE LO_TABLE_TYPE.



    ENDIF.

  ENDIF.

ENDFORM.                    " BUILD_OUTPUT_TABLE
*&---------------------------------------------------------------------*
*&      Form  SELECT_ASSET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_ASSET_DATA .

  DATA LO_TABLE_LINE      TYPE REF TO DATA.
  DATA LT_TAB_DELIMITED TYPE TRUXS_T_TEXT_DATA .
  DATA LT_DATA TYPE TRUXS_T_TEXT_DATA .
  DATA LV_DELETE_MARKED TYPE STRING.
  DATA LV_CONTINUEFLAG TYPE CHAR1.
  DATA LV_FDATE TYPE SY-DATUM.
  DATA LV_FTIME TYPE SY-UZEIT.
  DATA LV_FILENAME TYPE STRING.
  DATA LV_TDATE TYPE SY-DATUM.
  DATA LV_TTIME TYPE SY-UZEIT.
  DATA LT_OTAB TYPE ABAP_SORTORDER_TAB.
  DATA LS_ORDER TYPE ABAP_SORTORDER.
  DATA LV_COND TYPE STRING.

  DATA LV_FIELD_01 TYPE FIELDNAME.
  DATA LV_FIELD_02 TYPE FIELDNAME.
  DATA LV_FIELD_03 TYPE FIELDNAME.
  DATA LV_FIELD_04 TYPE FIELDNAME.
  DATA LV_FIELD_05 TYPE FIELDNAME.
  DATA LV_FIELD_06 TYPE FIELDNAME.

  DATA LV_DATE TYPE DATE.
  LV_DATE = SY-DATUM.


  FIELD-SYMBOLS: <LF_TAB>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <LF_LINE>,   <LF_FIELD>, <LF_GJAHR> , <LF_AFABE>.

* get depreciation area
  PERFORM GET_REAL_DEPRECIATION_AREA.

  ASSIGN GO_TABLE->* TO <LF_TAB>.

  IF <LF_TAB> IS ASSIGNED.

*   Create dynamic work area and assign to Field Symbol
    CREATE DATA LO_TABLE_LINE LIKE LINE OF <LF_TAB>.
    ASSIGN LO_TABLE_LINE->* TO <LF_LINE>.


    IF PR_ALL = GC_X.

      GV_INDXKEY = 'ASSET_DELTA'.

*     current date and time
      CONCATENATE SY-DATUM SY-UZEIT INTO GV_EXTRACTDATETIME.
*try .
*          SELECT
**            (gv_fieldlist)
**             anla~anln1 anla~anln2 anla~aktiv anla~menge anlz~stort anla~txt50 anla~anlkl anlc~kansw anlc~knafa anla~anlar anla~ernam anla~erdat anlz~werks anlb~ndjar
*            ANLA~BUKRS
*            ANLA~ANLN1
*            ANLA~ANLN2
*            ANLA~INVNR
*            ANLA~AKTIV
*            ANLA~TXA50
*            ANLA~MENGE
*            ANLA~TXT50
*            ANLA~INKEN
*            ANLA~INVZU
*            ANLA~ANLKL
*            ANLA~LIEFE
*            ANLA~IVDAT
*            ANLA~SERNR
*            ANLA~DEAKT
*            ANLA~AIBN1
*            ANLA~MEINS
*            ANLA~ANLAR
*            ANLA~ERNAM
*            ANLA~ERDAt
*            ANLZ~STORT
*            anlz~stort
*            ANLZ~RAUMN
*            ANLZ~WERKS
*            ANLU~ZZBUILDING
*            ANLU~ZZUSER_NAME
*            ANLU~ZZUSER_EMAIL
*            ANLU~ZZINVENTORY_BY
*            ANLU~ZZNOTE
*            ANLU~ZZRESI_VALUE
*            ANLU~ZZRESI_DATE
*            ANLU~ZZPREVS_REMARK
*            ANLU~ZZVERIFIED_AT
*            ANLU~ZZINVMODE
*            ANLU~ZZINV_COMMENT
*            ANLU~ZZRETIRE_IND
*            ANLC~KANSW
*            ANLC~KNAFA
*            ANLC~NAFAG
*            ANLC~GJAHR
*            ANLC~AFABE
*            ANLC~ANSWL
*            ANLB~NDJAR
***            ZASSETRAK~WDV_DATE
***            ZASSETRAK~RETIRE_FLAG
***            ZASSETRAK~REVIEW_FLAG
***            ZASSETRAK~PARENT_FLAG
***            ZASSETRAK~CHILD_ID
***            ZASSETRAK~WDV_AMOUNT
***            ZASSETRAK~AT_AQC
***            ZASSETRAK~AT_QUANTITY
***            ZASSETRAK~AT_UNIT
***            ZASSETRAK~AT_ADDL2
***            ZASSETRAK~SPLIT_FLAG
***            ZASSETRAK~CHANGE_FLAG
*           FROM anla
*            LEFT OUTER JOIN anlz ON anla~bukrs = anlz~bukrs AND anla~anln1 = anlz~anln1 AND anla~anln2 = anlz~anln2
*            LEFT OUTER JOIN  anlc ON anla~bukrs = anlc~bukrs AND anla~anln1 = anlc~anln1 AND anla~anln2 = anlc~anln2
*            LEFT OUTER JOIN  anlu ON anla~bukrs = anlu~bukrs AND anla~anln1 = anlu~anln1 AND anla~anln2 = anlu~anln2
*            LEFT OUTER JOIN  anlb on anla~bukrs = anlb~bukrs AND anla~anln1 = anlb~anln1 and anla~ANLN2 = anlb~anln2
*            INTO CORRESPONDING FIELDS OF TABLE <lf_tab>
*            WHERE anla~bukrs IN s_bukrs.


*        by satya on 02.11.2017

*  claris code
*
*     TRY .
*          SELECT
**            (gv_fieldlist)
**             anla~anln1 anla~anln2 anla~aktiv anla~menge anlz~stort anla~txt50 anla~anlkl anlc~kansw anlc~knafa anla~anlar anla~ernam anla~erdat anlz~werks anlb~ndjar
*            anla~bukrs
*            anla~anln1
*            anla~anln2
*            anla~invnr
*            anla~aktiv
*            anla~txa50
*            anla~menge
*            anla~txt50
*            anla~inken
*            anla~invzu
*            anla~anlkl
*            anla~liefe
*            anla~ivdat
*            anla~sernr
*            anla~deakt
*            anla~aibn1
*            anla~meins
*            anla~anlar
*            anla~ernam
*            anla~erdat
*            anlz~stort
*            anlz~stort
*            anlz~raumn
*            anlz~werks
*            anlu~zzbuilding
*            anlu~zzuser_name
*            anlu~zzuser_email
*            anlu~zzinventory_by
*            anlu~zznote
*            anlu~zzresi_value
*            anlu~zzresi_date
*            anlu~zzprevs_remark
*            anlu~zzverified_at
*            anlu~zzinvmode
*            anlu~zzinv_comment
*            anlu~zzretire_ind
*            anlc~kansw
*            anlc~knafa
*            anlc~nafag
*            anlc~gjahr
*            anlc~afabe
*            anlc~answl
**            anlb~ndjar
**            ZASSETRAK~WDV_DATE
**            ZASSETRAK~RETIRE_FLAG
**            ZASSETRAK~REVIEW_FLAG
**            ZASSETRAK~PARENT_FLAG
**            ZASSETRAK~CHILD_ID
**            ZASSETRAK~WDV_AMOUNT
**            ZASSETRAK~AT_AQC
**            ZASSETRAK~AT_QUANTITY
**            ZASSETRAK~AT_UNIT
**            ZASSETRAK~AT_ADDL2
**            ZASSETRAK~SPLIT_FLAG
**            ZASSETRAK~CHANGE_FLAG
*           FROM anla
*            LEFT OUTER JOIN anlz ON anla~bukrs = anlz~bukrs AND anla~anln1 = anlz~anln1 AND anla~anln2 = anlz~anln2
*            LEFT OUTER JOIN  anlc ON anla~bukrs = anlc~bukrs AND anla~anln1 = anlc~anln1 AND anla~anln2 = anlc~anln2
*            LEFT OUTER JOIN  anlu ON anla~bukrs = anlu~bukrs AND anla~anln1 = anlu~anln1 AND anla~anln2 = anlu~anln2
*            LEFT OUTER JOIN  anlb ON anla~bukrs = anlb~bukrs AND anla~anln1 = anlb~anln1 AND anla~anln2 = anlb~anln2
*            INTO CORRESPONDING FIELDS OF TABLE <lf_tab>
*            WHERE anla~bukrs IN s_bukrs.


*  claris *

*  TBBS
*
      TRY .
          SELECT
              BUKRS
              ANLN1
              ANLN2
              ANLKL
              ANLAR
              ERNAM
              ERDAT
              AKTIV
              DEAKT
              LIEFE
              AIBN1
              MEINS
              MENGE
              INKEN
              IVDAT
              INVZU
              INVNR
              TXT50
              TXA50
              SERNR FROM ANLA INTO CORRESPONDING FIELDS OF TABLE IT_ANLA2  WHERE BUKRS IN S_BUKRS." and anln1 = '000700008376'.

          DELETE IT_ANLA2 WHERE ANLKL = 'Z100'.

          IF IT_ANLA2 IS NOT INITIAL.
            SELECT
               BUKRS
               ANLN1
               ANLN2
              KOSTL
               WERKS
               STORT
               RAUMN FROM ANLZ INTO CORRESPONDING FIELDS OF TABLE IT_ANLZ1 FOR ALL ENTRIES IN IT_ANLA2 WHERE BUKRS EQ IT_ANLA2-BUKRS
                                                                                     AND  ANLN1 = IT_ANLA2-ANLN1
                                                                                     AND  ANLN2 = IT_ANLA2-ANLN2
                                                                                     AND  ( BDATU > LV_DATE AND  ADATU <= LV_DATE ) .
          ENDIF.
          SELECT
             BUKRS
             ANLN1
             ANLN2
             ZZBUILDING
              ZZUSER_NAME
              ZZUSER_EMAIL
              ZZINVENTORY_BY
              ZZNOTE
              ZZRESI_VALUE
              ZZRESI_DATE
              ZZPREVS_REMARK
              ZZVERIFIED_AT
              ZZINVMODE
              ZZINV_COMMENT
              ZZRETIRE_IND  FROM ANLU INTO CORRESPONDING FIELDS OF TABLE IT_ANLU1
                                     FOR ALL ENTRIES IN IT_ANLA2 WHERE  BUKRS = IT_ANLA2-BUKRS
                                                                   AND  ANLN1 = IT_ANLA2-ANLN1
                                                                   AND  ANLN2 = IT_ANLA2-ANLN2.

          SELECT
            BUKRS
            ANLN1
            ANLN2
            GJAHR
            AFABE
            KANSW
            KNAFA
            NAFAG
            ANSWL FROM ANLC INTO CORRESPONDING FIELDS OF TABLE IT_ANLC1
                                   FOR ALL ENTRIES IN IT_ANLA2 WHERE  BUKRS = IT_ANLA2-BUKRS
                                                               AND  ANLN1 = IT_ANLA2-ANLN1
                                                               AND  ANLN2 = IT_ANLA2-ANLN2.

          SELECT
            BUKRS
            ANLN1
            ANLN2
            NDJAR  FROM ANLB INTO CORRESPONDING FIELDS OF TABLE IT_ANLB1
                                   FOR ALL ENTRIES IN IT_ANLC1 WHERE  BUKRS = IT_ANLC1-BUKRS
                                                               AND  ANLN1 = IT_ANLC1-ANLN1
                                                               AND  ANLN2 = IT_ANLC1-ANLN2.

          SELECT
              KOSTL
              BUKRS
              PRCTR FROM CSKS INTO CORRESPONDING FIELDS OF TABLE IT_CSKS FOR ALL ENTRIES IN IT_ANLZ1 WHERE BUKRS = IT_ANLZ1-BUKRS
                                                                                                     AND KOSTL = IT_ANLZ1-KOSTL.

          SELECT
             KOSTL
             LTEXT FROM CSKT INTO CORRESPONDING FIELDS OF TABLE IT_CSKT FOR ALL ENTRIES IN IT_CSKS WHERE KOSTL = IT_CSKS-KOSTL.

          SELECT
              PRCTR
              SEGMENT FROM CEPC INTO CORRESPONDING FIELDS OF TABLE IT_CEPC FOR ALL ENTRIES IN IT_CSKS WHERE PRCTR = IT_CSKS-PRCTR.
*break 1000868.
          LOOP AT IT_ANLA2 INTO WA_ANLA2.
            WA_FINAL1-BUKRS = WA_ANLA2-BUKRS.
            WA_FINAL1-ANLN1 = WA_ANLA2-ANLN1.
            WA_FINAL1-ANLN2 = WA_ANLA2-ANLN2.
            WA_FINAL1-ANLKL = WA_ANLA2-ANLKL .
*            wa_final1-anlar = wa_anla2-anlar.
*            wa_final1-ernam = wa_anla2-ernam.
*            wa_final1-erdat = wa_anla2-erdat.
            WA_FINAL1-AKTIV = WA_ANLA2-AKTIV.
            WA_FINAL1-DEAKT = WA_ANLA2-DEAKT.
            WA_FINAL1-LIEFE = WA_ANLA2-LIEFE.
            WA_FINAL1-AIBN1 = WA_ANLA2-AIBN1.
            WA_FINAL1-MEINS = WA_ANLA2-MEINS.
            WA_FINAL1-MENGE = WA_ANLA2-MENGE.
            WA_FINAL1-INKEN = WA_ANLA2-INKEN.
            WA_FINAL1-IVDAT = WA_ANLA2-IVDAT.
            WA_FINAL1-INVZU = WA_ANLA2-INVZU.
            WA_FINAL1-INVNR = WA_ANLA2-INVNR.
            WA_FINAL1-TXT50 = WA_ANLA2-TXT50 .
            REPLACE ALL OCCURRENCES OF '"' IN WA_FINAL1-TXT50 WITH ' '.
            CONDENSE WA_FINAL1-TXT50.
            WA_FINAL1-TXA50 = WA_ANLA2-TXA50.
            WA_FINAL1-SERNR = WA_ANLA2-SERNR.

            READ TABLE IT_ANLZ1 INTO WA_ANLZ1 WITH KEY  BUKRS = WA_FINAL1-BUKRS
                                                         ANLN1 = WA_FINAL1-ANLN1
                                                         ANLN2 = WA_FINAL1-ANLN2.
            IF SY-SUBRC = 0.
              WA_FINAL1-WERKS = WA_ANLZ1-WERKS.
              WA_FINAL1-STORT = WA_ANLZ1-STORT.
*              wa_final1-raumn = wa_anlz1-raumn.
              READ TABLE IT_CSKS INTO WA_CSKS WITH KEY KOSTL = WA_ANLZ1-KOSTL.
              IF SY-SUBRC = 0.
                READ TABLE IT_CSKT INTO WA_CSKT WITH KEY KOSTL = WA_ANLZ1-KOSTL.

                CONCATENATE WA_CSKS-KOSTL WA_CSKT-LTEXT INTO WA_FINAL1-ZZBUILDING.
              ENDIF.
              READ TABLE IT_CEPC INTO WA_CEPC WITH KEY PRCTR = WA_CSKS-PRCTR.
              IF SY-SUBRC = 0.
                WA_FINAL1-RAUMN = WA_CEPC-SEGMENT.
              ENDIF.

              IF WA_FINAL1-WERKS = '1000'.
                CONCATENATE WA_FINAL1-WERKS WA_FINAL1-STORT INTO WA_FINAL1-WERKS.
              ENDIF.
            ENDIF.
            READ TABLE IT_ANLU1 INTO WA_ANLU1 WITH KEY    BUKRS = WA_FINAL1-BUKRS
                                                          ANLN1 = WA_FINAL1-ANLN1
                                                          ANLN2 = WA_FINAL1-ANLN2.
            IF SY-SUBRC = 0.
*              wa_final1-zzbuilding = wa_anlu1-zzbuilding.

              WA_FINAL1-ZZUSER_NAME = WA_ANLU1-ZZUSER_NAME.
              WA_FINAL1-ZZUSER_EMAIL = WA_ANLU1-ZZUSER_EMAIL.
              WA_FINAL1-ZZINVENTORY_BY = WA_ANLU1-ZZINVENTORY_BY.
              WA_FINAL1-ZZNOTE = WA_ANLU1-ZZNOTE.
              WA_FINAL1-ZZRESI_VALUE = WA_ANLU1-ZZRESI_VALUE.
              WA_FINAL1-ZZRESI_DATE = WA_ANLU1-ZZRESI_DATE.
              WA_FINAL1-ZZPREVS_REMARK = WA_ANLU1-ZZPREVS_REMARK.
              WA_FINAL1-ZZVERIFIED_AT = WA_ANLU1-ZZVERIFIED_AT.
              WA_FINAL1-ZZINVMODE = WA_ANLU1-ZZINVMODE.
              WA_FINAL1-ZZINV_COMMENT = WA_ANLU1-ZZINV_COMMENT.
              WA_FINAL1-ZZRETIRE_IND   = WA_ANLU1-ZZRETIRE_IND.
            ENDIF.

            READ TABLE IT_ANLC1 INTO WA_ANLC1 WITH KEY BUKRS = WA_FINAL1-BUKRS
                                                       ANLN1 = WA_FINAL1-ANLN1
                                                       ANLN2 = WA_FINAL1-ANLN2.
            IF SY-SUBRC = 0.
              WA_FINAL1-GJAHR = WA_ANLC1-GJAHR.
              WA_FINAL1-AFABE = WA_ANLC1-AFABE.
              WA_FINAL1-KANSW = WA_ANLC1-KANSW.
              WA_FINAL1-KNAFA = WA_ANLC1-KNAFA.
              WA_FINAL1-NAFAG = WA_ANLC1-NAFAG.
              WA_FINAL1-ANSWL = WA_ANLC1-ANSWL.
            ENDIF.
            READ TABLE IT_ANLB1 INTO WA_ANLB1 WITH KEY BUKRS = WA_FINAL1-BUKRS
                                                       ANLN1 = WA_FINAL1-ANLN1
                                                       ANLN2 = WA_FINAL1-ANLN2.
            IF SY-SUBRC = 0.
              WA_FINAL1-NDJAR = WA_ANLB1-NDJAR .
            ENDIF.

            APPEND WA_FINAL1 TO IT_FINAL1.
            CLEAR: WA_FINAL1, WA_CEPC, WA_CSKS, WA_CSKT.
          ENDLOOP.



*           loop at it_final1 INTO wa_final1 where werks = '1000'.
*
*             CONCATENATE wa_final1-werks wa_final1-stort INTO wa_final1-plant_storage_code.
*             modify it_final1 from wa_final1 TRANSPORTING PLANT_STORAGE_CODE.
*             ENDLOOP.
          DELETE IT_FINAL1 WHERE WERKS = '1000'.
          IT_FINAL2[] = IT_FINAL1[].
          ASSIGN IT_FINAL1[] TO  <LF_TAB> .
***         EOC By satya
**
**<  tbbs

        CATCH CX_SY_DYNAMIC_OSQL_ERROR.
          MESSAGE 'Error in Dynamic Query excution' TYPE 'E'.

      ENDTRY.

*      IF sy-subrc = 0.
*
*      ENDIF.

    ELSEIF PR_DELTA = GC_X.

      GV_INDXKEY = 'ASSET_DELTA'.
*     get last extraction date
      IMPORT GV_EXTRACTDATETIME TO GV_EXTRACTDATETIME FROM DATABASE INDX(ZA)
                                    ID GV_INDXKEY.

      IF GV_EXTRACTDATETIME IS INITIAL.
        GV_EXTRACTDATETIME = '20000101000000'. " Default date 2000/01/01 00:00:00
      ENDIF.

      LV_FDATE = GV_EXTRACTDATETIME(8).
      LV_FTIME = GV_EXTRACTDATETIME+8(6).
      LV_TDATE = SY-DATUM.
      LV_TTIME = SY-UZEIT.

      PERFORM GET_CDHDR USING LV_FDATE LV_FTIME LV_TDATE LV_TTIME.

* download asset values
    ELSEIF PR_AVAL = GC_X.

      PERFORM ASSET_VALUE_EXTRACT.

    ENDIF.


    LOOP AT <LF_TAB> ASSIGNING <LF_LINE> .

      CLEAR: LV_CONTINUEFLAG .

*     set field value to space which contains INITIAL values like 0, 0.00 etc
      PERFORM ASSING_BLANKS USING 'GJAHR' CHANGING <LF_LINE> .
      PERFORM ASSING_BLANKS USING 'DEAKT' CHANGING <LF_LINE> .
*      PERFORM assing_blanks USING 'AKTIV' CHANGING <lf_line> .
      PERFORM ASSING_BLANKS USING 'ZZRESI_DATE' CHANGING <LF_LINE> .
      PERFORM ASSING_BLANKS USING 'IVDAT' CHANGING <LF_LINE> .


*   Clear the below fields in case of full doanload and delta download
      IF PR_AVAL NE GC_X.
        PERFORM ASSING_BLANKS USING 'KANSW' CHANGING <LF_LINE> .
        PERFORM ASSING_BLANKS USING 'KNAFA' CHANGING <LF_LINE> .
        PERFORM ASSING_BLANKS USING 'NAFAG' CHANGING <LF_LINE> .
        PERFORM ASSING_BLANKS USING 'ANSWL' CHANGING <LF_LINE> .
        PERFORM ASSING_BLANKS USING 'WDV_AMOUNT' CHANGING <LF_LINE> .
      ENDIF.

      PERFORM CHECK_FOR_YEAR CHANGING <LF_LINE> LV_CONTINUEFLAG.

      IF LV_CONTINUEFLAG = GC_X.
        CONTINUE.
      ELSE.

        PERFORM CHECK_FOR_DEPRECIATION_AREA CHANGING <LF_LINE> LV_CONTINUEFLAG.

        IF LV_CONTINUEFLAG = GC_X.
          CONTINUE.
        ELSE.

          IF PR_ALL = GC_X.
            PERFORM ASSIGN_CHANGE_FLAG USING 'F' CHANGING <LF_LINE>.
          ELSEIF PR_AVAL = GC_X.
            PERFORM ASSIGN_CHANGE_FLAG USING 'C' CHANGING <LF_LINE>.
          ELSE.
*            for Delta
*            changes or added flag already assigned in related routines
          ENDIF.

*         Add asset WDV Date only when extracting Asset Value
          IF PR_AVAL = GC_X.
            PERFORM ASSIGN_WDV_DATE CHANGING <LF_LINE>.
          ENDIF.

*         No Retire flag is required in case Asset Value Extract
          IF PR_AVAL <> GC_X.
            PERFORM ASSIGN_RETIRE_FLAG CHANGING <LF_LINE>.
          ENDIF.

*         special conversion exit routines
          PERFORM SERNR_CONVERSION_EXIT CHANGING <LF_LINE> .


        ENDIF.
      ENDIF.
    ENDLOOP.

*   ASSIGN it_final2[] TO  <lf_tab> .
*       delete records.
    LV_DELETE_MARKED = ' ANLN1 = SPACE'.
    DELETE <LF_TAB> WHERE (LV_DELETE_MARKED).

  ENDIF.


  IF <LF_TAB> IS NOT INITIAL.

*   define sort criteria
    LV_FIELD_02  = LS_ORDER-NAME = 'BUKRS'.
    APPEND LS_ORDER TO LT_OTAB.
    LV_FIELD_03  = LS_ORDER-NAME = 'ANLN1'.
    APPEND LS_ORDER TO LT_OTAB.
    LV_FIELD_04  = LS_ORDER-NAME = 'ANLN2'.
    APPEND LS_ORDER TO LT_OTAB.
    LV_FIELD_05  = LS_ORDER-NAME = 'GJAHR'.
    LS_ORDER-DESCENDING = GC_X.
    APPEND LS_ORDER TO LT_OTAB.
    CLEAR LS_ORDER.
    LV_FIELD_06  = LS_ORDER-NAME = 'AFABE'.
    APPEND LS_ORDER TO LT_OTAB.

    SORT <LF_TAB> BY (LT_OTAB).
    DELETE ADJACENT DUPLICATES FROM <LF_TAB> COMPARING (LV_FIELD_01) (LV_FIELD_02) (LV_FIELD_03) (LV_FIELD_04). " (lv_field_05). " (lv_field_06).

  ELSE.

    MESSAGE 'No data found' TYPE 'S'.

  ENDIF.

  APPEND GV_LINE1 TO LT_DATA.
  APPEND GV_LINE2 TO LT_DATA.


  CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
    TABLES
      I_TAB_SAP_DATA       = <LF_TAB>
    CHANGING
      I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
*             Implement suitable error handling here
    MESSAGE 'Error in converting Asset data to TAB Delimited format' TYPE 'E'.

  ELSE.

    APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*     conver data to bin format
    PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_ASSETDATA.

    IF PR_AVAL = GC_X.
      PERFORM CREATE_FILE USING 'Asset_Value' 2 LT_DATA.
    ELSE.
      PERFORM CREATE_FILE USING 'Asset_Data' 2 LT_DATA.
    ENDIF.


*   if FULL load is running first time set that date time AS FROM DATE for next Delta extract.
    IF PR_ALL = GC_X.

      GV_INDXKEY = 'ASSET_1STFULLLDL'.
      DATA LV_EXTRACTDATETIME TYPE CHAR14.
      IMPORT LV_EXTRACTDATETIME TO LV_EXTRACTDATETIME FROM DATABASE INDX(ZA)
                              ID GV_INDXKEY.

*     if lv_extractdatetime is initial - This indicate FULL download happening first time
      IF LV_EXTRACTDATETIME IS INITIAL.

        LV_EXTRACTDATETIME = GV_EXTRACTDATETIME.
*       Save current date time
        EXPORT LV_EXTRACTDATETIME TO DATABASE INDX(ZA) ID GV_INDXKEY.

        GV_INDXKEY = 'ASSET_DELTA'.
*       set lv_extractdatetime as starting point for DELTA
        EXPORT LV_EXTRACTDATETIME TO DATABASE INDX(ZA) ID GV_INDXKEY.

      ENDIF.


    ELSEIF PR_DELTA = GC_X.

*     set current date time for DELTA download --- delta
      GV_INDXKEY = 'ASSET_DELTA'.
*     set last extraction date
      EXPORT GV_EXTRACTDATETIME TO DATABASE INDX(ZA) ID GV_INDXKEY.

    ENDIF.

  ENDIF.
  IF <LF_TAB> IS NOT INITIAL.
    MESSAGE 'Files Succesfully Downloaded to FTP  Server' TYPE 'S' .
  ELSE.
    MESSAGE 'No data found' TYPE 'S' .
  ENDIF.
ENDFORM.                    " SELECT_ASSET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CDHDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FDATE  text
*      -->P_LV_FTIME  text
*      -->P_LV_TDATE  text
*      -->P_LV_TTIME  text
*----------------------------------------------------------------------*
FORM GET_CDHDR  USING    PIV_FDATE TYPE SY-DATUM
                         PIV_FTIME TYPE SY-UZEIT
                         PIV_TDATE TYPE SY-DATUM
                         PIV_TTIME TYPE SY-UZEIT.


  FIELD-SYMBOLS <LF_CDHDR> TYPE CDHDR.


*  read the cd-headers
*  current date and time
  CONCATENATE SY-DATUM SY-UZEIT INTO GV_EXTRACTDATETIME.

  SELECT * FROM CDHDR
    INTO TABLE GT_CDHDR
    WHERE
     ( ( ( ( UDATE = PIV_FDATE ) AND ( UTIME >= PIV_FTIME ) ) OR ( UDATE > PIV_FDATE ) )
    AND
      ( ( ( UDATE = PIV_TDATE ) AND ( UTIME =< PIV_TTIME ) ) OR ( UDATE < PIV_TDATE ) )
    )
    AND TCODE  IN GT_TCODEALL . "( 'AS01' , 'AS02' ).

  IF SY-SUBRC = 0.


    LOOP AT GT_CDHDR ASSIGNING <LF_CDHDR>.
      IF <LF_CDHDR>-OBJECTID(4) NOT IN GR_COMPANY .
        CLEAR <LF_CDHDR>-OBJECTID.
      ENDIF.

    ENDLOOP.

    DELETE GT_CDHDR WHERE OBJECTID IS INITIAL.

    IF GT_CDHDR IS NOT INITIAL.
      PERFORM GET_NEW_CHANGED_RECORDS.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_CDHDR
*&---------------------------------------------------------------------*
*&      Form  GET_NEW_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NEW_CHANGED_RECORDS .

  PERFORM GET_NEW_ASSET_DETAILS.

  PERFORM GET_CHANGED_ASSET_DETAILS.

ENDFORM.                    " GET_NEW_CHANGED_RECORDS
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_ASSET_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CHANGED_ASSET_DETAILS .

  DATA LV_BUKRS TYPE BUKRS.
  DATA LV_ANLN1 TYPE ANLN1.
  DATA LV_ANLN2 TYPE ANLN2.
  DATA LS_FINALASSET TYPE TY_FINALASSETS.
  DATA LO_TABLE_LINE      TYPE REF TO DATA.
  DATA LS_CDPOS TYPE CDPOS.
  DATA LT_CDHDR TYPE STANDARD TABLE OF CDHDR.
  FIELD-SYMBOLS: <LF_LINE>, <LF_FIELD>.
*  get only relevent changed fields.

  CLEAR   LT_CDHDR.

  LT_CDHDR = GT_CDHDR.

* keep only Changed records.
  DELETE LT_CDHDR WHERE TCODE NOT IN GT_TCODECHANGE .

  IF LT_CDHDR IS NOT INITIAL.
    GT_CHANGEDASSET = LT_CDHDR.


    SELECT * INTO TABLE GT_CDPOS FROM CDPOS
      FOR ALL ENTRIES IN LT_CDHDR
      WHERE OBJECTCLAS = LT_CDHDR-OBJECTCLAS AND
            OBJECTID = LT_CDHDR-OBJECTID AND
            CHANGENR = LT_CDHDR-CHANGENR.

    IF SY-SUBRC = 0.

      ASSIGN GO_TABLE->* TO <GF_FINALTAB>.

      GT_FINALASSET[] = <GF_FINALTAB>.

*     consider only latest changes per Table Field
      SORT GT_CDPOS BY OBJECTCLAS OBJECTID TABKEY FNAME CHANGENR DESCENDING . "chngind.
      DELETE ADJACENT DUPLICATES FROM GT_CDPOS COMPARING OBJECTCLAS OBJECTID TABKEY FNAME. "chngind.

      LOOP AT GT_CDPOS INTO LS_CDPOS.

        LV_BUKRS = LS_CDPOS-OBJECTID(4).
        LV_ANLN1 =  LS_CDPOS-OBJECTID+4(12).
        LV_ANLN2 =  LS_CDPOS-OBJECTID+16(4).

*     check if change is relevent field which is maked for export
        READ TABLE GT_EXPORT WITH KEY TABLENAME = LS_CDPOS-TABNAME SAPFIELD = LS_CDPOS-FNAME
        TRANSPORTING NO FIELDS.

        IF SY-SUBRC = 0.

*         record needs to send to AT
          READ TABLE GT_FINALASSET INTO LS_FINALASSET WITH KEY BUKRS = LS_CDPOS-OBJECTID(4)
                                                               ANLN1 = LS_CDPOS-OBJECTID+4(12)
                                                               ANLN2 = LS_CDPOS-OBJECTID+16(4).
          IF SY-SUBRC = 0.

            READ TABLE <GF_FINALTAB> ASSIGNING <LF_LINE> INDEX SY-TABIX.
            IF <LF_LINE> IS ASSIGNED.

*             assign changed value of field.
              ASSIGN COMPONENT LS_CDPOS-FNAME OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
              IF <LF_FIELD> IS ASSIGNED.

                IF LS_CDPOS-VALUE_NEW IS INITIAL.
                  <LF_FIELD> = '/'.     " this will indicate fields new value is SPACE/BLANK
                ELSE.
                  <LF_FIELD> = LS_CDPOS-VALUE_NEW.
                ENDIF.

              ENDIF.

*           check if there is change in Rack id if yes change location ID as well
              IF LS_CDPOS-FNAME = 'STORT' OR LS_CDPOS-FNAME = 'WERKS'  .
                PERFORM ASSIGN_LOCATION USING LV_BUKRS
                                              LV_ANLN1
                                              LV_ANLN2
                                    CHANGING <LF_LINE>.
              ENDIF.

              IF LS_CDPOS-FNAME = 'ANSWL' OR LS_CDPOS-FNAME = 'KANSW'.
                PERFORM ASSIGN_COST USING LV_BUKRS
                                LV_ANLN1
                                LV_ANLN2
                      CHANGING <LF_LINE>.
              ENDIF.

            ENDIF.

          ELSE.

            LS_FINALASSET-BUKRS = LS_CDPOS-OBJECTID(4).
            LS_FINALASSET-ANLN1 = LS_CDPOS-OBJECTID+4(12).
            LS_FINALASSET-ANLN2 = LS_CDPOS-OBJECTID+16(4).
            APPEND LS_FINALASSET TO GT_FINALASSET.

            UNASSIGN <LF_LINE>.
            FREE LO_TABLE_LINE.

*         Create dynamic work area and assign to Field Symbol
            CREATE DATA LO_TABLE_LINE LIKE LINE OF <GF_FINALTAB>.
            ASSIGN LO_TABLE_LINE->* TO <LF_LINE>.

*         assign keys


            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
            IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
              <LF_FIELD> = LS_CDPOS-OBJECTID(4).
            ENDIF.


            ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
            IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
              <LF_FIELD> = LS_CDPOS-OBJECTID+4(12).
            ENDIF.

            ASSIGN COMPONENT 'ANLN2' OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
            IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
              <LF_FIELD> = LS_CDPOS-OBJECTID+16(4).
            ENDIF.

*         set change flag to C - change in Asset ( Delta Mode)
            PERFORM ASSIGN_CHANGE_FLAG USING 'C' CHANGING <LF_LINE>.


*         assign changed value of field.
            ASSIGN COMPONENT LS_CDPOS-FNAME OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
            IF <LF_FIELD> IS ASSIGNED.
              IF LS_CDPOS-VALUE_NEW IS INITIAL.
                <LF_FIELD> = '/'.     " this will indicate fields new value is SPACE/BLANK
              ELSE.
                <LF_FIELD> = LS_CDPOS-VALUE_NEW.
              ENDIF.
            ENDIF.

*         check if there is change in Rack id if yes change location ID as well
            IF LS_CDPOS-FNAME = 'STORT'.
              PERFORM ASSIGN_LOCATION USING LV_BUKRS
                                            LV_ANLN1
                                            LV_ANLN2
                                  CHANGING <LF_LINE>.
            ENDIF.

            APPEND <LF_LINE> TO <GF_FINALTAB>.

            UNASSIGN <LF_LINE>.


          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.
ENDFORM.                    " GET_CHANGED_ASSET_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_NEW_ASSET_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NEW_ASSET_DETAILS .

  DATA LS_FINALASSET TYPE TY_FINALASSETS.
  DATA LT_CDHDR TYPE STANDARD TABLE OF CDHDR.
  DATA LO_TABLE_LINE      TYPE REF TO DATA.
  DATA LS_CDPOS TYPE CDPOS.

  FIELD-SYMBOLS: <LF_LINE>, <LF_FIELD>.
  LT_CDHDR = GT_CDHDR.

* keep only new records.
  DELETE LT_CDHDR WHERE TCODE NOT IN GT_TCODENEW.

  IF LT_CDHDR[] IS NOT INITIAL.
    GT_NEWASSET = LT_CDHDR.


    ASSIGN GO_TABLE->* TO <GF_FINALTAB>.

    IF <GF_FINALTAB> IS ASSIGNED.

      SELECT
*        (gv_fieldlist)
          ANLA~BUKRS
          ANLA~ANLN1
          ANLA~ANLN2
          ANLA~INVNR
          ANLA~AKTIV
          ANLA~TXA50
          ANLA~MENGE
          ANLA~TXT50
          ANLA~INKEN
          ANLA~INVZU
          ANLA~ANLKL
          ANLA~LIEFE
          ANLA~IVDAT
          ANLA~SERNR
          ANLA~DEAKT
          ANLA~AIBN1
          ANLA~MEINS
          ANLA~ANLAR
          ANLA~ERNAM
          ANLA~ERDAT
          ANLZ~STORT
          ANLZ~STORT
          ANLZ~RAUMN
          ANLZ~WERKS
          ANLU~ZZBUILDING
          ANLU~ZZUSER_NAME
          ANLU~ZZUSER_EMAIL
          ANLU~ZZINVENTORY_BY
          ANLU~ZZNOTE
          ANLU~ZZRESI_VALUE
          ANLU~ZZRESI_DATE
          ANLU~ZZPREVS_REMARK
          ANLU~ZZVERIFIED_AT
          ANLU~ZZINVMODE
          ANLU~ZZINV_COMMENT
          ANLU~ZZRETIRE_IND
          ANLC~KANSW
          ANLC~KNAFA
          ANLC~NAFAG
          ANLC~GJAHR
          ANLC~AFABE
          ANLC~ANSWL
          ANLB~NDJAR
      FROM ANLA
      LEFT OUTER JOIN  ANLZ ON ANLA~BUKRS = ANLZ~BUKRS AND ANLA~ANLN1 = ANLZ~ANLN1 AND ANLA~ANLN2 = ANLZ~ANLN2
      LEFT OUTER JOIN  ANLC ON ANLA~BUKRS = ANLC~BUKRS AND ANLA~ANLN1 = ANLC~ANLN1 AND ANLA~ANLN2 = ANLC~ANLN2
      LEFT OUTER JOIN  ANLU ON ANLA~BUKRS = ANLU~BUKRS AND ANLA~ANLN1 = ANLU~ANLN1 AND ANLA~ANLN2 = ANLU~ANLN2
      LEFT OUTER JOIN  ANLB ON ANLA~BUKRS = ANLB~BUKRS AND ANLA~ANLN1 = ANLB~ANLN1 AND ANLA~ANLN2 = ANLB~ANLN2
      APPENDING CORRESPONDING FIELDS OF TABLE  <GF_FINALTAB>
      FOR ALL ENTRIES IN GT_NEWASSET
      WHERE ANLA~BUKRS = GT_NEWASSET-BUKRS AND
            ANLA~ANLN1 = GT_NEWASSET-ANLN1 AND
            ANLA~ANLN2 = GT_NEWASSET-ANLN2.

    ENDIF.

    LOOP AT <GF_FINALTAB> ASSIGNING <LF_LINE> .


*   set change flag to A as New Asset ( running in delta mode)
      PERFORM ASSIGN_CHANGE_FLAG USING 'A' CHANGING <LF_LINE>.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_NEW_ASSET_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_REAL_DEPRECIATION_AREA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_REAL_DEPRECIATION_AREA .
* get company msater maintained for Assetrak.

  IF GT_ASSET_COMPANY IS NOT INITIAL.

    SELECT * INTO TABLE GT_T093D FROM T093D
      FOR ALL ENTRIES IN GT_ASSET_COMPANY
      WHERE BUKRS = GT_ASSET_COMPANY-BUKRS AND AFABER = '01'.

  ENDIF.
ENDFORM.                    " GET_REAL_DEPRECIATION_AREA
*&---------------------------------------------------------------------*
*&      Form  assign_CHANGE_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1010   text
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM ASSIGN_CHANGE_FLAG  USING    PIV_FLAG TYPE CHAR1
                         CHANGING PCS_LINE.

  FIELD-SYMBOLS <LF_FIELD>.
*   assign ADD flag.
  ASSIGN COMPONENT 'CHANGE_FLAG' OF STRUCTURE PCS_LINE TO <LF_FIELD>.
  IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
    IF <LF_FIELD> IS INITIAL.
      <LF_FIELD> = PIV_FLAG.
    ENDIF.
  ENDIF.


ENDFORM.                    " assign_CHANGE_FLAG
*&---------------------------------------------------------------------*
*&      Form  assign_WDV_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM ASSIGN_WDV_DATE  CHANGING PCS_LINE.

  DATA LV_WDVDATE TYPE CHAR7.
  DATA LS_T093D TYPE T093D.
  FIELD-SYMBOLS : <LF_FIELD>, <LF_BUKRS>.
  ASSIGN COMPONENT 'BUKRS' OF STRUCTURE PCS_LINE TO <LF_BUKRS>.

  IF <LF_BUKRS> IS ASSIGNED AND SY-SUBRC = 0.
    READ TABLE GT_T093D INTO LS_T093D WITH KEY BUKRS = <LF_BUKRS> AFABER = '01'.
    IF SY-SUBRC = 0.

      CONCATENATE LS_T093D-AFBLPE LS_T093D-AFBLGJ INTO LV_WDVDATE .

      ASSIGN COMPONENT 'WDV_DATE' OF STRUCTURE PCS_LINE TO <LF_FIELD>.
      IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
        IF <LF_FIELD> IS INITIAL AND LV_WDVDATE IS NOT INITIAL.
          <LF_FIELD> = LV_WDVDATE.
        ELSE.
          <LF_FIELD> = SPACE.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " assign_WDV_DATE
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_RETIRE_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM ASSIGN_RETIRE_FLAG  CHANGING PCS_LINE.

  FIELD-SYMBOLS : <LF_FIELD>, <LF_DEAKT>.

  ASSIGN COMPONENT 'DEAKT' OF STRUCTURE PCS_LINE TO <LF_DEAKT>.

  IF <LF_DEAKT> IS ASSIGNED AND SY-SUBRC = 0.

*   if date is not initial and NOT EQ SPACE and NOT EQ '00000000'
    IF <LF_DEAKT> IS NOT INITIAL AND <LF_DEAKT> <> SPACE AND <LF_DEAKT> <> '00000000'.

      ASSIGN COMPONENT 'RETIRE_FLAG' OF STRUCTURE PCS_LINE TO <LF_FIELD>.

      IF <LF_FIELD> IS ASSIGNED AND SY-SUBRC = 0.
        IF <LF_FIELD> IS INITIAL.
          <LF_FIELD> = GC_X.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.



ENDFORM.                    " ASSIGN_RETIRE_FLAG
*&---------------------------------------------------------------------*
*&      Form  IMPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPORT_DATA .

  FIELD-SYMBOLS: <LF_TAB>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <LF_LINE>,   <LF_FIELD>.
  DATA LT_IMPORT TYPE TY_ZASSETRAK_IMPORT_TAB.
  DATA LT_DIRECT TYPE TY_ZASSETRAK_IMPORT_TAB.
  DATA LT_REVIEW TYPE TY_ZASSETRAK_IMPORT_TAB.
  DATA LT_SPLIT TYPE TY_ZASSETRAK_IMPORT_TAB.
  DATA LV_FILENAME TYPE ZAT_FILENAME.
  DATA LO_TABLE_LINE      TYPE REF TO DATA.

  PERFORM READ_FILEDATA CHANGING LT_IMPORT.

  CONCATENATE 'AT_IMPORT' SY-DATUM SY-UZEIT INTO LV_FILENAME.
* categorize asset update data
*    Direct Update
*    for review

  PERFORM CATEGORIZE_ASSET_UPDATE USING   LT_IMPORT
                                  CHANGING LT_DIRECT
                                           LT_REVIEW
                                           LT_SPLIT.

* direct update.
  IF LT_DIRECT IS NOT INITIAL.
    PERFORM DIRECT_UPDATE_ASSET USING LT_DIRECT LV_FILENAME.
  ENDIF.

* Split update.
  IF LT_SPLIT IS NOT INITIAL.
    PERFORM SPLIT_UPDATE_ASSET USING LT_SPLIT LV_FILENAME.
  ENDIF.

* Review update.
  IF LT_REVIEW IS NOT INITIAL.
    PERFORM STAGE_REVIEW_ASSET USING LT_REVIEW LV_FILENAME.
  ENDIF.

  IF GT_RETURN IS NOT INITIAL.
* move data into iternal table.
    CALL FUNCTION '/SDF/DISPLAY_TABLE'
      TABLES
        TABLE = GT_RETURN.
  ENDIF.

** move data into iternal table.
*  CALL FUNCTION '/SDF/DISPLAY_TABLE'
*    TABLES
*      table = <lf_tab>.




ENDFORM.                    " IMPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_TO_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SAPFIELD  text
*      -->P_LT_STRING  text
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM MOVE_DATA_TO_STRUCTURE  USING    PIT_SAPFIELD TYPE TY_STRINGTAB
                                      PIT_STRING TYPE TY_STRINGTAB
                             CHANGING PCS_LINE.
  FIELD-SYMBOLS <LF_FIELD>.
  DATA LV_TABNAME TYPE ZASSET_TABLES.
  DATA LV_FIELDNAME TYPE  FIELDNAME.
  DATA LV_SAPFIELD TYPE STRING.
  DATA LV_FIELDVALUE TYPE STRING.
  DATA LV_INDEX TYPE SY-TABIX.

  LOOP AT PIT_SAPFIELD INTO LV_SAPFIELD.
    LV_INDEX = SY-TABIX.

    SPLIT LV_SAPFIELD AT '-' INTO LV_TABNAME LV_FIELDNAME.

    ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE PCS_LINE TO <LF_FIELD>.
    IF SY-SUBRC = 0.
*Added by Poonam
*{

      READ TABLE PIT_STRING INTO LV_FIELDVALUE INDEX LV_INDEX.
      IF SY-SUBRC = 0.
*        <lf_field> =   lv_fieldvalue.
        CATCH SYSTEM-EXCEPTIONS      ARITHMETIC_ERRORS       = 1
                                    CONVERSION_ERRORS        = 2
                                    COMPUTE_INT_ZERODIVIDE   = 3.
          <LF_FIELD> =   LV_FIELDVALUE.
        ENDCATCH.
        IF SY-SUBRC <> 0.
          DATA: LT_MAILTXT         TYPE STANDARD TABLE OF SOLI      WITH HEADER LINE.
          LT_MAILTXT = 'Error while reading data from file.'.
          PERFORM EMAIL_ERROR USING LT_MAILTXT.
        ENDIF.
*}


        IF LV_INDEX = 2 OR LV_INDEX = 3.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <LF_FIELD>
            IMPORTING
              OUTPUT = <LF_FIELD>.

        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " MOVE_DATA_TO_STRUCTURE
*&---------------------------------------------------------------------*
*&      Form  MARK_FOR_DELETION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM MARK_FOR_DELETION  CHANGING PCS_LINE.

  FIELD-SYMBOLS <LF_FIELD>.


  ASSIGN COMPONENT 'ANLN1' OF STRUCTURE PCS_LINE TO <LF_FIELD>.
  IF SY-SUBRC = 0.

    CLEAR <LF_FIELD> .

  ENDIF.



ENDFORM.                    " MARK_FOR_DELETION
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM CHECK_FOR_YEAR  CHANGING PCS_LINE PCV_CONTINUE TYPE CHAR1.

  FIELD-SYMBOLS <LF_GJAHR>.

  ASSIGN COMPONENT 'GJAHR' OF STRUCTURE PCS_LINE TO <LF_GJAHR>.
  IF SY-SUBRC = 0.

    IF <LF_GJAHR> NOT IN S_GJAHR AND <LF_GJAHR> <> SPACE.
      PCV_CONTINUE = GC_X.
      PERFORM MARK_FOR_DELETION CHANGING PCS_LINE.
    ENDIF.

  ENDIF.
ENDFORM.                    " CHECK_FOR_YEAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_DEPRECIATION_AREA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM CHECK_FOR_DEPRECIATION_AREA  CHANGING PCS_LINE PCV_CONTINUE TYPE CHAR1.

  FIELD-SYMBOLS <LF_AFABE>.

  ASSIGN COMPONENT 'AFABE' OF STRUCTURE PCS_LINE TO <LF_AFABE>.
  IF SY-SUBRC = 0.
    IF <LF_AFABE> NOT IN S_AFABE AND <LF_AFABE> <> SPACE .
      PCV_CONTINUE = GC_X.
      PERFORM MARK_FOR_DELETION CHANGING PCS_LINE.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_FOR_DEPRECIATION_AREA
*&---------------------------------------------------------------------*
*&      Form  READ_FILEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FILEDATA CHANGING  PCT_IMPORT TYPE TY_ZASSETRAK_IMPORT_TAB..


  DATA LV_FILESTRING TYPE STRING.
  DATA LS_FILEROWDATA TYPE TY_FILEROWDATA.

  DATA LV_INFILE  LIKE RLGRAP-FILENAME.
  DATA LT_STRING TYPE STANDARD TABLE OF STRING.

  DATA LT_SAPFIELD TYPE STANDARD TABLE OF STRING.
  DATA LT_ATFIELD TYPE STANDARD TABLE OF STRING.
  DATA LS_IMPORT TYPE ZASSETRAK_IMPORT.
  DATA BLOB_LENGTH TYPE I.
  DATA BLOB TYPE SDOKCNTBIN OCCURS 0 WITH HEADER LINE.
***
  DATA:  LV_USER(30) TYPE C VALUE 'tirupaticor\arssc', "user name of ftp server
         LV_PWD(30) TYPE C VALUE 'arbl@123', "password of ftp server
         LV_HOST(64) TYPE C VALUE '10.111.0.21', "ip address of FTP server
         LV_DEST LIKE RFCDES-RFCDEST VALUE 'SAPFTPA'."Background RFC destination
  DATA: LV_HDL TYPE I,
        GC_KEY TYPE I VALUE 26101957,
        LV_SLEN TYPE I.
  DATA : RESULT TYPE TRUXS_T_TEXT_DATA.
*  CONCATENATE piv_filename '.TXT' INTO gv_file .

****
  DATA: L_DATA_FILE TYPE STRING,
          LWA_LINES TYPE CHAR10,
             L_FILE TYPE AUTHB-FILENAME.  "filename

  FIELD-SYMBOLS   : <L1_DATA>.
  FIELD-SYMBOLS: <LF_TAB>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <LF_LINE>,   <LF_FIELD>, <LF_TMP>.

  DATA LO_TABLE_LINE      TYPE REF TO DATA.
  ASSIGN GO_TABLE->* TO <LF_TAB>.

  IF <LF_TAB> IS ASSIGNED.
*   Create dynamic work area and assign to Field Symbol
    CREATE DATA LO_TABLE_LINE LIKE LINE OF <LF_TAB>.
    ASSIGN LO_TABLE_LINE->* TO <LF_LINE>.
  ENDIF.

* read file from FTP
*HTTP_SCRAMBLE: used to scramble the password provided in a format recognized by SAP.
  SET EXTENDED CHECK OFF.
  LV_SLEN = STRLEN( LV_PWD ).
  SET EXTENDED CHECK OFF.
  LV_SLEN = STRLEN( LV_PWD ).

  LV_USER = 'tirupaticor\arssc'.
  LV_PWD = 'arbl@123'.
  LV_HOST = '10.111.0.21'.
  SET EXTENDED CHECK OFF.
  LV_SLEN = STRLEN( LV_PWD ).


  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE      = LV_PWD
      SOURCELEN   = LV_SLEN
      KEY         = GC_KEY
    IMPORTING
      DESTINATION = LV_PWD.

* SELECT single low from tvarvc
*        into lv_user where name = 'ZFTP_USER'.
*
* SELECT single low from tvarvc
*        into lv_pwd where name = 'ZFTP_SCRAMBLE_PWD'.
* if sy-subrc NE 0.
*   lv_pwd = '87C80B5732193F5B'.
* endif.
*
* SELECT single low from tvarvc
*        into lv_host where name = 'ZFTP_HOST'.
*
** To Connect to the Server using FTP
  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      USER            = LV_USER
      PASSWORD        = LV_PWD
      HOST            = LV_HOST
      RFC_DESTINATION = LV_DEST
    IMPORTING
      HANDLE          = LV_HDL
    EXCEPTIONS
      OTHERS          = 1.
*
  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      HANDLE        = LV_HDL
      COMMAND       = 'set passive on'
    TABLES
      DATA          = RESULT
    EXCEPTIONS
      COMMAND_ERROR = 1
      TCPIP_ERROR   = 2.

  SELECT SINGLE LOW FROM TVARVC INTO LV_INFILE
         WHERE NAME = 'ZASSETRAK_IMPORT_FILEPATH' .

  IF SY-SUBRC = 0.
    L_DATA_FILE = LV_INFILE.
    L_FILE = LV_INFILE.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
*      PROGRAM                =
        ACTIVITY               = SABC_ACT_READ
        FILENAME               = L_FILE
*     EXCEPTIONS
*     NO_AUTHORITY           = 1
*     ACTIVITY_UNKNOWN       = 2
*     OTHERS                 = 3
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ELSE.
*   read file lines
      OPEN DATASET L_DATA_FILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF SY-SUBRC = 0.
        WHILE SY-SUBRC = 0.

          READ DATASET L_DATA_FILE INTO LS_FILEROWDATA.
          IF SY-SUBRC = 0 AND LS_FILEROWDATA IS NOT INITIAL.
            APPEND LS_FILEROWDATA TO GT_FILEROWDATA.
          ENDIF.
        ENDWHILE.
        CLOSE DATASET L_DATA_FILE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        HANDLE         = LV_HDL
        FNAME          = LV_INFILE
        CHARACTER_MODE = 'X'
      IMPORTING
        BLOB_LENGTH    = BLOB_LENGTH
      TABLES
        BLOB           = BLOB
        TEXT           = GT_FILEROWDATA
      EXCEPTIONS
        TCPIP_ERROR    = 1
        COMMAND_ERROR  = 2
        DATA_ERROR     = 3.
  ENDIF.

* read file
  LOOP AT GT_FILEROWDATA INTO LS_FILEROWDATA.
*       header1
    IF SY-TABIX = 1.
      CLEAR LT_ATFIELD.
      SPLIT LS_FILEROWDATA AT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO TABLE LT_ATFIELD.

*       header2
    ELSEIF SY-TABIX = 2.

      CLEAR LT_SAPFIELD.
      SPLIT LS_FILEROWDATA AT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO TABLE LT_SAPFIELD.

*       Actual Data
    ELSE.
*
      CLEAR LT_STRING.
      SPLIT LS_FILEROWDATA AT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO TABLE LT_STRING.

      PERFORM MOVE_DATA_TO_STRUCTURE USING LT_SAPFIELD LT_STRING
                                     CHANGING <LF_LINE>.
      APPEND <LF_LINE> TO <LF_TAB>.
      MOVE-CORRESPONDING <LF_LINE> TO LS_IMPORT.

*         move room RAUMN value to room. First AT file has RAUMN for room
      ASSIGN COMPONENT 'RAUMN' OF STRUCTURE <LF_LINE> TO <LF_TMP>.
      IF SY-SUBRC = 0.
        LS_IMPORT-ZZROOM = <LF_TMP>.
      ENDIF.

      APPEND LS_IMPORT TO PCT_IMPORT.
    ENDIF.
  ENDLOOP.
*send email****************************
  IF PCT_IMPORT[] IS INITIAL.
*    DATA: lt_mailtxt         TYPE STANDARD TABLE OF soli      WITH HEADER LINE.
*    lt_mailtxt = 'Import file is empty.'.
*    PERFORM email_error USING lt_mailtxt.
    PERFORM SEND_EMAIL_FOR_IMPORT.
  ENDIF.
*  *************************************
ENDFORM.                    " READ_FILEDATA
*&---------------------------------------------------------------------*
*&      Form  CATEGORIZE_ASSET_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_IMPORT  text
*      <--P_LT_DIRECT  text
*      <--P_LT_REVIEW  text
*----------------------------------------------------------------------*
FORM CATEGORIZE_ASSET_UPDATE  USING    PIT_IMPORT TYPE TY_ZASSETRAK_IMPORT_TAB
                              CHANGING PCT_DIRECT TYPE TY_ZASSETRAK_IMPORT_TAB
                                       PCT_REVIEW TYPE TY_ZASSETRAK_IMPORT_TAB
                                       PCT_SPLIT TYPE TY_ZASSETRAK_IMPORT_TAB.

*collect direct update records only ( no review, no split records)
  PCT_DIRECT = PIT_IMPORT.

* delete review records
  DELETE PCT_DIRECT WHERE REVIEW_FLAG = GC_Y.

* delte split records
  DELETE PCT_DIRECT WHERE SPLIT_FLAG = GC_S.

* delete records which don't require review.
* collect review records only ( here we can have split/indivisual records records as well)
  PCT_REVIEW = PIT_IMPORT.
  DELETE PCT_REVIEW WHERE REVIEW_FLAG <> GC_Y.



* collect split records only ( no review, no indivisual records)
  PCT_SPLIT = PIT_IMPORT.
  DELETE PCT_SPLIT WHERE SPLIT_FLAG <> GC_S.
  DELETE PCT_SPLIT WHERE REVIEW_FLAG = GC_Y.

ENDFORM.                    " CATEGORIZE_ASSET_UPDATE
*&---------------------------------------------------------------------*
*&      Form  DIRECT_UPDATE_ASSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DIRECT  text
*----------------------------------------------------------------------*
FORM DIRECT_UPDATE_ASSET  USING    PIT_DIRECT TYPE TY_ZASSETRAK_IMPORT_TAB
                                   PIV_FILENAME TYPE ZAT_FILENAME.


  DATA LS_DIRECT TYPE ZASSETRAK_IMPORT.
  DATA LV_SUCCESS TYPE CHAR1.

  LOOP AT  PIT_DIRECT INTO LS_DIRECT.

    PERFORM UPDATE_ASSET USING PIV_FILENAME 'N' SPACE CHANGING LS_DIRECT  LV_SUCCESS.

  ENDLOOP.

ENDFORM.                    " DIRECT_UPDATE_ASSET

*&---------------------------------------------------------------------*
*&      Form  SPLIT_UPDATE_ASSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SPLIT  text
*      -->P_LV_FILENAME  text
*----------------------------------------------------------------------*
FORM SPLIT_UPDATE_ASSET  USING   PIT_SPLIT    TYPE TY_ZASSETRAK_IMPORT_TAB
                                 PIV_FILENAME TYPE ZAT_FILENAME.

  DATA LS_SPLIT TYPE ZASSETRAK_IMPORT.
  DATA LS_SPLITTMP TYPE ZASSETRAK_IMPORT.
  DATA LS_SPLITUPDATE TYPE ZASSETRAK_IMPORT.

  DATA LT_SPLITASSET TYPE STANDARD TABLE OF ZASSETRAK_SPLIT.
  DATA LS_SPLITASSET TYPE ZASSETRAK_SPLIT.
  DATA LV_SUCCESS TYPE CHAR1.
  DATA LV_COUNTER TYPE ZAT_COUNTER.
  FIELD-SYMBOLS <FS_SPLITASSET> TYPE ZASSETRAK_SPLIT.


  SORT PIT_SPLIT BY BUKRS ANLN1 ANLN2 CHILD_ID PARENT_FLAG DESCENDING.

  LOOP AT  PIT_SPLIT INTO LS_SPLIT.

    LS_SPLITTMP = LS_SPLIT.

    AT NEW ANLN1.
      CLEAR LS_SPLITUPDATE.
      CLEAR LV_COUNTER .
      CLEAR LT_SPLITASSET.

    ENDAT.

    MOVE-CORRESPONDING LS_SPLIT TO LS_SPLITASSET.
*    manual move of few fields as Name are differents
    LS_SPLITTMP-ZZRETIRE_DATE =  LS_SPLITASSET-ZZRETIRE_DATE = LS_SPLIT-DEAKT.
    LS_SPLITTMP-ZZLOCATION = LS_SPLITASSET-ZZLOCATION = LS_SPLIT-WERKS.
    LS_SPLITTMP-ZZRACK = LS_SPLITASSET-ZZRACK = LS_SPLIT-STORT.
*    ls_splittmp-zzroom = ls_splitasset-zzroom = ls_split-raumn.
*WDV_AMOUNT
*AT_AQC
*AT_QUANTITY
*AT_UNIT
*AT_ADDL2
*PRO_STATUS
*ZZRETIRE_IND

    IF LS_SPLITTMP-PARENT_FLAG = 'P'.  " Send this structure values on Additional Tab ( ANLU update)
      LS_SPLITUPDATE = LS_SPLITTMP.
    ENDIF.

*    lv_counter = lv_counter + 1.
*    ls_splitasset-counter  = lv_counter.
    APPEND LS_SPLITASSET TO LT_SPLITASSET.





    AT END OF ANLN1.

*     if call is coming from review for SPLIT asset for NON-PARENT item
      IF LS_SPLITUPDATE IS NOT INITIAL.
        PERFORM UPDATE_ASSET USING PIV_FILENAME 'N' SPACE CHANGING LS_SPLITUPDATE LV_SUCCESS .
      ELSE.
        LV_SUCCESS = GC_X.
      ENDIF.

      IF LV_SUCCESS = GC_X.

        LOOP AT LT_SPLITASSET INTO LS_SPLITASSET.

*   delete split assets
          DELETE FROM ZASSETRAK_SPLIT WHERE    BUKRS = LS_SPLITASSET-BUKRS AND
              ANLN1 = LS_SPLITASSET-ANLN1 AND
              ANLN2 = LS_SPLITASSET-ANLN2 AND
              CHILD_ID = LS_SPLITASSET-CHILD_ID.

        ENDLOOP.
        COMMIT WORK AND WAIT.

        SORT LT_SPLITASSET BY BUKRS ANLN1 ANLN2 CHILD_ID.
        DELETE ADJACENT DUPLICATES FROM LT_SPLITASSET COMPARING CHILD_ID.

*        update split table with success flag.

        LOOP AT LT_SPLITASSET ASSIGNING <FS_SPLITASSET> .
          <FS_SPLITASSET>-MANDT = SY-MANDT.
          <FS_SPLITASSET>-PRO_STATUS = GC_P.
        ENDLOOP.

*       insert new data
        INSERT ZASSETRAK_SPLIT FROM TABLE LT_SPLITASSET ACCEPTING DUPLICATE KEYS.

*        IF sy-subrc = 0.
*        IF sy-dbcnt is not initial.
        COMMIT WORK AND WAIT.
*       ENDIF.

      ELSE.

*        CLEAR ls_splitasset-counter.

*        update split table but with error flag.

        LOOP AT LT_SPLITASSET ASSIGNING <FS_SPLITASSET> .
          <FS_SPLITASSET>-MANDT = SY-MANDT.
          <FS_SPLITASSET>-PRO_STATUS = GC_E.
        ENDLOOP.

        INSERT ZASSETRAK_SPLIT FROM TABLE LT_SPLITASSET ACCEPTING DUPLICATE KEYS.
*        IF sy-subrc = 0.
*       if sy-dbcnt is not INITIAL.
        COMMIT WORK AND WAIT.
*       ENDIF.

      ENDIF.

    ENDAT.


  ENDLOOP.

ENDFORM.                    " SPLIT_UPDATE_ASSET

*&---------------------------------------------------------------------*
*&      Form  stage_review_asset
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIT_REVIEW    text
*      -->PIV_FILENAME  text
*----------------------------------------------------------------------*
FORM STAGE_REVIEW_ASSET  USING     PIT_REVIEW TYPE TY_ZASSETRAK_IMPORT_TAB
                                   PIV_FILENAME TYPE ZAT_FILENAME.


  DATA LS_REVIEW TYPE ZASSETRAK_IMPORT.
  DATA LT_REVIEW TYPE STANDARD TABLE OF ZASSETRAK_IMPORT.

  DATA LS_REVIEW_TMP TYPE ZASSETRAK_IMPORT.
  DATA LV_SUCCESS TYPE CHAR1.
  DATA LV_COUNTER TYPE ZAT_COUNTER.

  IF PIT_REVIEW IS NOT INITIAL.

    SORT PIT_REVIEW BY BUKRS ANLN1 ANLN2 CHILD_ID PARENT_FLAG DESCENDING.
    .
    LOOP AT  PIT_REVIEW INTO LS_REVIEW_TMP.



      LS_REVIEW = LS_REVIEW_TMP.

      IF LS_REVIEW_TMP-SPLIT_FLAG = GC_S.
        AT NEW ANLN1.
          CLEAR LV_COUNTER .
        ENDAT.
*        lv_counter = lv_counter + 1.
*        ls_review-counter = lv_counter.
        LS_REVIEW-IMP_FILENAME = PIV_FILENAME.
        LS_REVIEW-PRO_STATUS = GC_N.
        APPEND LS_REVIEW TO LT_REVIEW.

        AT END OF ANLN1.
          INSERT ZASSETRAK_IMPORT FROM TABLE LT_REVIEW ACCEPTING DUPLICATE KEYS.
          CLEAR: LT_REVIEW, LV_COUNTER.
        ENDAT.

      ELSE.

        LS_REVIEW-IMP_FILENAME = PIV_FILENAME.
        LS_REVIEW-PRO_STATUS = GC_N.
        INSERT INTO ZASSETRAK_IMPORT VALUES LS_REVIEW.

      ENDIF.

    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDIF.
ENDFORM.                    " DIRECT_UPDATE_ASSET
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DIRECT  text
*----------------------------------------------------------------------*
FORM UPDATE_ASSET  USING
                            PIV_FILENAME TYPE ZAT_FILENAME
                            PIV_MODE TYPE CHAR1
                            PIV_RESON TYPE ZAT_REASON
                   CHANGING PCS_DIRECT TYPE ZASSETRAK_IMPORT
                            PCV_SUCCESS TYPE CHAR1.

  DATA LS_CONFIG TYPE ZASSETRAK_CONFIG.
  DATA LV_COMPANYCODE TYPE BAPI1022_1-COMP_CODE.
  DATA LV_ASSET TYPE BAPI1022_1-ASSETMAINO.
  DATA LV_SUBNUMBER TYPE BAPI1022_1-ASSETSUBNO.
  DATA LV_CHILD_ID TYPE ZAT_COUNTER.

*
  DATA LS_POSTINGINFORMATION    TYPE BAPI1022_FEGLG002.
  DATA LS_GENERALDATA   TYPE BAPI1022_FEGLG001.
  DATA LS_INVENTORY   TYPE BAPI1022_FEGLG011.
  DATA LS_ORIGIN    TYPE BAPI1022_FEGLG009.
  DATA LS_TIMEDEPENDENTDATA   TYPE BAPI1022_FEGLG003.

  DATA LS_POSTINGINFORMATIONX    TYPE BAPI1022_FEGLG002X.
  DATA LS_GENERALDATAX   TYPE BAPI1022_FEGLG001X.
  DATA LS_INVENTORYX   TYPE BAPI1022_FEGLG011X.
  DATA LS_ORIGINX    TYPE BAPI1022_FEGLG009X.
  DATA LS_TIMEDEPENDENTDATAX   TYPE BAPI1022_FEGLG003X.

  DATA LS_EXTENSION TYPE BAPIPAREX.
  DATA LT_EXTENSION TYPE STANDARD TABLE OF BAPIPAREX.

  DATA LS_RETURN TYPE BAPIRET2.

  LV_COMPANYCODE = PCS_DIRECT-BUKRS.
  LV_ASSET = PCS_DIRECT-ANLN1.
  LV_SUBNUMBER = PCS_DIRECT-ANLN2.
  LV_CHILD_ID = PCS_DIRECT-CHILD_ID.

* reject mode
  IF PIV_MODE = 'R'.

    PCS_DIRECT-PROCESSED_BY = SY-UNAME.
    PCS_DIRECT-PROCESSED_ON = SY-DATUM.
    PCS_DIRECT-IMP_FILENAME = PIV_FILENAME.

    PCS_DIRECT-PRO_STATUS = GC_R.

    PCS_DIRECT-MESSAGE = PIV_RESON.

    MODIFY ZASSETRAK_IMPORT FROM PCS_DIRECT." ACCEPTING DUPLICATE KEYS.


  ELSE.

    PERFORM UPDATE_EXTENSION USING PCS_DIRECT CHANGING LS_EXTENSION.

    APPEND LS_EXTENSION TO LT_EXTENSION.
*  loop through configuration and populate data for fields
*  which are marked as IMPORT.
*  LOOP AT gt_config INTO ls_config.

    PERFORM POPULATE_GENERALDATA USING    PCS_DIRECT
                                 CHANGING LS_GENERALDATA
                                          LS_GENERALDATAX.

    PERFORM POPULATE_INVENTORY USING    PCS_DIRECT
                                 CHANGING LS_INVENTORY
                                          LS_INVENTORYX.
    PERFORM POPULATE_ORIGIN  USING    PCS_DIRECT
                            CHANGING LS_ORIGIN
                                     LS_ORIGINX    .

    PERFORM POPULATE_TIMEDEPENDENTDATA  USING    PCS_DIRECT
                             CHANGING LS_TIMEDEPENDENTDATA
                                      LS_TIMEDEPENDENTDATAX .

    PERFORM POPULATE_POSTINGINFORMATION USING PCS_DIRECT
                CHANGING LS_POSTINGINFORMATION
                          LS_POSTINGINFORMATIONX.

    CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
      EXPORTING
        COMPANYCODE         = LV_COMPANYCODE
        ASSET               = LV_ASSET
        SUBNUMBER           = LV_SUBNUMBER
*       GROUPASSET          =
        GENERALDATA         = LS_GENERALDATA
        GENERALDATAX        = LS_GENERALDATAX
        INVENTORY           = LS_INVENTORY
        INVENTORYX          = LS_INVENTORYX
        POSTINGINFORMATION  = LS_POSTINGINFORMATION
        POSTINGINFORMATIONX = LS_POSTINGINFORMATIONX
        TIMEDEPENDENTDATA   = LS_TIMEDEPENDENTDATA
        TIMEDEPENDENTDATAX  = LS_TIMEDEPENDENTDATAX
        ORIGIN              = LS_ORIGIN
        ORIGINX             = LS_ORIGINX
      IMPORTING
        RETURN              = LS_RETURN
      TABLES
        EXTENSIONIN         = LT_EXTENSION.

    PCS_DIRECT-PROCESSED_BY = SY-UNAME.
    PCS_DIRECT-PROCESSED_ON = SY-DATUM.
    PCS_DIRECT-IMP_FILENAME = PIV_FILENAME.

    IF LS_RETURN-TYPE = GC_E.
      PCS_DIRECT-PRO_STATUS = GC_E.
    ELSE.
      PCS_DIRECT-PRO_STATUS = GC_P.
      PCV_SUCCESS = GC_X.

    ENDIF.

    PCS_DIRECT-MESSAGE = LS_RETURN-MESSAGE.

    IF PIV_MODE = 'N'.
      INSERT INTO ZASSETRAK_IMPORT VALUES PCS_DIRECT." ACCEPTING DUPLICATE KEYS.
    ELSE.
      MODIFY ZASSETRAK_IMPORT FROM PCS_DIRECT." ACCEPTING DUPLICATE KEYS.
    ENDIF.

    APPEND LS_RETURN TO GT_RETURN..

  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = GC_X.


ENDFORM.                    " UPDATE_ASSET
*&---------------------------------------------------------------------*
*&      Form  POPULATE_GENERALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PIS_DIRECT  text
*      <--P_LS_GENERALDATA  text
*      <--P_LS_GENERALDATAX  text
*----------------------------------------------------------------------*
FORM POPULATE_GENERALDATA  USING    PIS_DIRECT TYPE ZASSETRAK_IMPORT
                           CHANGING PCS_GENERALDATA TYPE BAPI1022_FEGLG001
                                    PCS_GENERALDATAX TYPE BAPI1022_FEGLG001X.

  DATA LV_IMPORT TYPE CHAR1.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'INVNR' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
    PCS_GENERALDATA-INVENT_NO = PIS_DIRECT-INVNR.
    PCS_GENERALDATAX-INVENT_NO = GC_X.
  ENDIF.
*  pcs_generaldata-assetclass = pis_direct-anlkl.
*  pcs_generaldata-quantity = pis_direct-menge.
*  pcs_generaldata-base_uom = 'EA'.


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'SERNR' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
    PCS_GENERALDATA-SERIAL_NO = PIS_DIRECT-SERNR.
    PCS_GENERALDATAX-SERIAL_NO = GC_X.
  ENDIF.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'TXA50' CHANGING LV_IMPORT.
  IF LV_IMPORT IS NOT INITIAL.
    PCS_GENERALDATA-DESCRIPT2 = PIS_DIRECT-TXA50.
    PCS_GENERALDATAX-DESCRIPT2 = GC_X.
  ENDIF.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'TXT50' CHANGING LV_IMPORT.
  IF LV_IMPORT IS NOT INITIAL.
    PCS_GENERALDATA-DESCRIPT = PIS_DIRECT-TXT50.
    PCS_GENERALDATAX-DESCRIPT = GC_X.
  ENDIF.



*  pcs_generaldatax-assetclass = gc_x.

*  pcs_generaldatax-quantity = gc_x.
*  pcs_generaldata-base_uom = gc_x.



ENDFORM.                    " POPULATE_GENERALDATA
*&---------------------------------------------------------------------*
*&      Form  POPULATE_INVENTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PIS_DIRECT  text
*      <--P_LS_INVENTORY  text
*      <--P_LS_INVENTORYX  text
*----------------------------------------------------------------------*
FORM POPULATE_INVENTORY  USING    PIS_DIRECT TYPE ZASSETRAK_IMPORT
                         CHANGING PCS_INVENTORY TYPE BAPI1022_FEGLG011
                                  PCS_INVENTORYX TYPE BAPI1022_FEGLG011X.

  DATA LV_IMPORT TYPE CHAR1.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'INKEN' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
    PCS_INVENTORY-INCLUDE_IN_LIST = PIS_DIRECT-INKEN    .
    PCS_INVENTORYX-INCLUDE_IN_LIST = GC_X   .

  ENDIF.


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'INVZU' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
    PCS_INVENTORY-NOTE = PIS_DIRECT-INVZU   .
    PCS_INVENTORYX-NOTE = GC_X   .
  ENDIF.


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'IVDAT' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
    PCS_INVENTORY-DATE = PIS_DIRECT-IVDAT   .
    PCS_INVENTORYX-DATE = GC_X .
  ENDIF.

ENDFORM.                    " POPULATE_INVENTORY

*&---------------------------------------------------------------------*
*&      Form  populate_origin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIS_DIRECT   text
*      <--PCS_ORIGIN   text
*      <--PCS_ORIGINX  text
*----------------------------------------------------------------------*
FORM POPULATE_ORIGIN  USING    PIS_DIRECT TYPE ZASSETRAK_IMPORT
                         CHANGING PCS_ORIGIN    TYPE BAPI1022_FEGLG009
                                  PCS_ORIGINX    TYPE BAPI1022_FEGLG009X.

  DATA LV_IMPORT TYPE CHAR1.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'LIEFE' CHANGING LV_IMPORT.


  IF LV_IMPORT IS NOT INITIAL.
    PCS_ORIGIN-VENDOR = PIS_DIRECT-LIEFE.

    PCS_ORIGINX-VENDOR = GC_X.

  ENDIF.

ENDFORM.                    " POPULATE_INVENTORY


*&---------------------------------------------------------------------*
*&      Form  populate_timedependentdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIS_DIRECT              text
*      <--PCS_TIMEDEPENDENTDATA   text
*      <--PCS_TIMEDEPENDENTDATAX  text
*----------------------------------------------------------------------*
FORM POPULATE_TIMEDEPENDENTDATA  USING    PIS_DIRECT TYPE ZASSETRAK_IMPORT
                         CHANGING PCS_TIMEDEPENDENTDATA   TYPE BAPI1022_FEGLG003
                                  PCS_TIMEDEPENDENTDATAX   TYPE BAPI1022_FEGLG003X.

*  As per new change these fields will be updated as ZZ
*  pcs_timedependentdata-location = pis_direct-stort.
*  pcs_timedependentdata-plant = pis_direct-werks.
*
*  pcs_timedependentdatax-location = gc_x.
*  pcs_timedependentdatax-plant = gc_x.

ENDFORM.                    " POPULATE_INVENTORY
*&---------------------------------------------------------------------*
*&      Form  POPULATE_POSTINGINFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PIS_DIRECT  text
*      <--P_LS_POSTINGINFORMATION  text
*      <--P_LS_POSTINGINFORMATIONX  text
*----------------------------------------------------------------------*
FORM POPULATE_POSTINGINFORMATION  USING    PIS_DIRECT TYPE ZASSETRAK_IMPORT
                                  CHANGING PCS_POSTINGINFORMATION TYPE BAPI1022_FEGLG002
                                           PCS_POSTINGINFORMATIONX TYPE BAPI1022_FEGLG002X.

  DATA LV_IMPORT TYPE CHAR1.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'AKTIV' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL..
    PCS_POSTINGINFORMATION-CAP_DATE = PIS_DIRECT-AKTIV.
    PCS_POSTINGINFORMATIONX-CAP_DATE = GC_X.
  ENDIF.


*  CLEAR lv_import.
*  PERFORM checkimport USING 'ANLA' 'DEAKT' CHANGING lv_import.
*
*  IF lv_import IS NOT INITIAL.
*    pcs_postinginformation-deact_date = pis_direct-deakt.
*    pcs_postinginformationx-deact_date = gc_x.
*  ENDIF.




ENDFORM.                    " POPULATE_POSTINGINFORMATION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_EXTENSION  text
*----------------------------------------------------------------------*
FORM UPDATE_EXTENSION  USING PIS_DIRECT TYPE ZASSETRAK_IMPORT
                    CHANGING PCS_EXTENSION TYPE BAPIPAREX.

  DATA LS_ANLU TYPE ANLU.

  DATA:
    LD_OFFSET_ANLU   TYPE I VALUE 250,
    LD_OFFSET_VALUE   TYPE I VALUE 250,
    LS_TE_ANLU       TYPE BAPI_TE_ANLU.                  "<< note 760748
  FIELD-SYMBOLS: <LS_TE_ANLU>           TYPE X.          "<< note 760748
  FIELD-SYMBOLS: <LS_EXTENSION>         TYPE X.          "<< note 760748
  CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.
  CONSTANTS:      CON_BYTE            TYPE I VALUE
                                      CL_ABAP_CHAR_UTILITIES=>CHARSIZE.
  FIELD-SYMBOLS: <LS_ANLU>.
  FIELD-SYMBOLS: <LS_EXT> TYPE CHAR250.

  DATA LV_IMPORT TYPE CHAR1.

* select custom data
  SELECT SINGLE * INTO LS_ANLU FROM ANLU
    WHERE BUKRS = PIS_DIRECT-BUKRS AND
          ANLN1 = PIS_DIRECT-ANLN1 AND
          ANLN2 = PIS_DIRECT-ANLN2.

  PCS_EXTENSION-STRUCTURE = 'BAPI_TE_ANLU'.
  DESCRIBE FIELD PCS_EXTENSION-STRUCTURE LENGTH LD_OFFSET_ANLU
                                        IN CHARACTER MODE. "Unicode
*  keyfields
  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 20. "Add length of previous filed
*  MOVE  ls_anlu+23 TO ls_extension+ld_offset_anlu.



  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZBUILDING' CHANGING LV_IMPORT.


  IF LV_IMPORT IS NOT INITIAL.

    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZBUILDING.

*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzbuilding.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 30.

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZUSER_NAME' CHANGING LV_IMPORT.


  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzuser_name = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZUSER_NAME.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzuser_name.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 40.   "Add length of previous filed


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZUSER_EMAIL' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzuser_email = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZUSER_EMAIL.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzuser_email.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 70.


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZINVENTORY_BY' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzinventory_by = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZINVENTORY_BY.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzinventory_by.
  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 40.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZNOTE' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zznote = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZNOTE.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zznote.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 50.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZRESI_VALUE' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.

*    IF pis_direct-zzresi_value = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZRESI_VALUE.
*    ENDIF.
*  ELSE.
**      WRITE ls_anlu-zzresi_value TO pcs_extension+ld_offset_anlu.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzresi_value.
  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 15.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZRESI_DATE' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.

*    IF pis_direct-zzresi_date = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZRESI_DATE.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzresi_date .
  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 8.    "Add length of previous filed



  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZPREVS_REMARK' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*   IF pis_direct-zzprevs_remark = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZPREVS_REMARK.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzprevs_remark.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 250.    "Add length of previous filed

*_______________________________________________________________________________________
  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZVERIFIED_AT' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.

*    IF pis_direct-zzverified_at = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZVERIFIED_AT.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzverified_at.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 4.    "Add length of previous filed


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZINVMODE' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzinvmode = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZINVMODE.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzinvmode.
  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 10.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZINV_COMMENT' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzinv_comment = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZINV_COMMENT.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzinv_comment.

  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 50. "Add length of previous filed


  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLZ' 'RAUMN' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-zzroom = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZROOM.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzroom.
  ENDIF.

  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 30. "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLA' 'DEAKT' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-deakt = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-DEAKT.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzretire_date.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 8.  "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLZ' 'WERKS' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-werks = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-WERKS.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzlocation.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 10.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLZ' 'STORT' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-stort = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-STORT.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-zzrack.
  ENDIF.


  LD_OFFSET_ANLU = LD_OFFSET_ANLU + 40.   "Add length of previous filed

  CLEAR LV_IMPORT.
  PERFORM CHECKIMPORT USING 'ANLU' 'ZZRETIRE_IND' CHANGING LV_IMPORT.

  IF LV_IMPORT IS NOT INITIAL.
*    IF pis_direct-stort = '/'.
*      pcs_extension+ld_offset_anlu = space.
*    ELSE.
    PCS_EXTENSION+LD_OFFSET_ANLU = PIS_DIRECT-ZZRETIRE_IND.
*    ENDIF.
*  ELSE.
*    pcs_extension+ld_offset_anlu = ls_anlu-ZZRETIRE_IND.
  ENDIF.


ENDFORM.                    " UPDATE_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  REVIEW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REVIEW_DATA .

  DATA LT_REVIEW TYPE STANDARD TABLE OF ZASSETRAK_IMPORT.


*     call ALV to display .
  PERFORM CALL_ALV .

ENDFORM.                    " REVIEW_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV .

*
  DATA LV_TITLE TYPE LVC_TITLE.
  DATA LS_ASSETLIST_ALV TYPE ZASSETRAK_IMPORT_ALV.
  DATA LS_ASSETLIST TYPE ZASSETRAK_IMPORT.
* set ALV related parameters.
  LV_TITLE = 'Review and Update Asset'.

  CLEAR GT_ASSETLIST_ALV.
  LOOP AT GT_ASSETLIST INTO LS_ASSETLIST.
    MOVE-CORRESPONDING LS_ASSETLIST TO LS_ASSETLIST_ALV.
    APPEND LS_ASSETLIST_ALV TO GT_ASSETLIST_ALV.
  ENDLOOP.


* set color
  PERFORM SET_COLOR.

* populate field catalog
  PERFORM BUILD_FIELDCATALOG .

* set ALV layout
  PERFORM BUILD_LAYOUT.

* set event
  PERFORM SET_EVENTEXIT.

** set Date / Time
*  GET TIME.
*  gv_ttime = sy-uzeit.
*  gv_tdate = sy-datum.


* call ALV function module
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'
      I_STRUCTURE_NAME         = 'ZASSETRAK_IMPORT'
      I_GRID_TITLE             = LV_TITLE
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT
      IS_VARIANT               = GS_VARIANT
      IT_EVENT_EXIT            = GT_EVENTEXITS
    TABLES
      T_OUTTAB                 = GT_ASSETLIST_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
*   display error message
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.


*
ENDFORM.                    " call_alv
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIT_EXTAB  text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING PIT_EXTAB TYPE SLIS_T_EXTAB.       "#EC CALLED

  DATA LT_EXCL TYPE SLIS_T_EXTAB.
  DATA LS_EXCL TYPE SLIS_EXTAB.

* set ALV title
  SET TITLEBAR 'ALVLIST'.


* set ALV status excluding commands
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING LT_EXCL.

ENDFORM. "Set_pf_status


*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
*

  DATA LO_STRUREF TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA LT_DFIES TYPE DDFIELDS.
  DATA LS_DFIES TYPE DFIES.
  DATA LV_REPORT TYPE CHAR100.
  DATA LV_FIELDNAME TYPE CHAR30.


  FIELD-SYMBOLS <FS_TARGET> TYPE ANY.
  FIELD-SYMBOLS: <FS_FCAT> TYPE LVC_S_FCAT.


* create fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZASSETRAK_IMPORT_ALV'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

** loop field catalog and set some additional attributes
  LOOP AT GT_FIELDCAT ASSIGNING <FS_FCAT>.

    IF <FS_FCAT>-FIELDNAME <> 'IMP_FILENAME'
    AND <FS_FCAT>-FIELDNAME <>  'BUKRS'
    AND <FS_FCAT>-FIELDNAME <> 'ANLN1'
    AND <FS_FCAT>-FIELDNAME <> 'ANLN2'
    AND <FS_FCAT>-FIELDNAME <>  'COUNTER'
    AND <FS_FCAT>-FIELDNAME <>  'PROCESSED_ON'
    AND <FS_FCAT>-FIELDNAME <>  'PROCESSED_BY'
    AND <FS_FCAT>-FIELDNAME <>  'PRO_STATUS'
    AND <FS_FCAT>-FIELDNAME <>  'MESSAGE'.

      READ TABLE GT_CONFIG  WITH KEY SAPFIELD = <FS_FCAT>-FIELDNAME IMPORT = GC_X TRANSPORTING NO FIELDS.
      IF SY-SUBRC <> 0.
        <FS_FCAT>-NO_OUT = GC_X.
      ENDIF.
    ENDIF.
    IF <FS_FCAT>-FIELDNAME =  'PRO_STATUS'.
      <FS_FCAT>-COL_POS = 2.
    ENDIF.
    IF <FS_FCAT>-FIELDNAME =  'MESSAGE'.
      <FS_FCAT>-COL_POS = 2.
      <FS_FCAT>-OUTPUTLEN = 30.
    ENDIF.
*
*
*    CASE <fs_fcat>-fieldname .
*
*      WHEN 'MARK'.
*        <fs_fcat>-checkbox = gc_true.
*        <fs_fcat>-edit     = gc_true.
*        <fs_fcat>-seltext  = 'Select'.
*        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l    = 'Select'.
*        <fs_fcat>-outputlen = 5.
*        <fs_fcat>-fix_column =  gc_true.
*
*      WHEN 'FORMULA_ID'.
*        <fs_fcat>-scrtext_s = <fs_fcat>-seltext    = 'Formula ID'.
*
*      WHEN 'RULE_TEXT'.
*        <fs_fcat>-outputlen = 100.
*        <fs_fcat>-hotspot  = gc_true.
*
*      WHEN 'DESCRIPTION'.
*        <fs_fcat>-outputlen = 20.
*
*      WHEN 'FUNCTION_NAME'.
*        <fs_fcat>-outputlen = 20.
*
*      WHEN 'CHANGE_TIME'.
*        <fs_fcat>-no_zero = gc_true.
*
*      WHEN 'CREATE_TIME'.
*        <fs_fcat>-no_zero = gc_true.
*
*    ENDCASE.
*
  ENDLOOP.



ENDFORM.                    " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
*
  GS_LAYOUT-NO_ROWMARK = GC_X.
  GS_LAYOUT-SEL_MODE = 'B'.
  GS_LAYOUT-CWIDTH_OPT = GC_X.
  GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.
  GS_LAYOUT-ZEBRA = GC_X..

  CLEAR GS_VARIANT .

  GS_VARIANT-REPORT = SY-REPID.
*  gs_variant-variant = gs_ruleset-alv_layout.

ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIV_UCOMM     text
*      -->PIS_SELFIELD  text
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING PIV_UCOMM TYPE SY-UCOMM
                            PIS_SELFIELD TYPE SLIS_SELFIELD. "#EC CALLED)

  DATA LV_TITLE TYPE LVC_TITLE.
  DATA LO_GUID TYPE REF TO CL_GUI_ALV_GRID.
*  DATA ls_alvlist TYPE ty_alv.
  DATA LT_INDEX_ROWS  TYPE LVC_T_ROW.
  DATA LS_INDEX_ROW TYPE LVC_S_ROW.
  DATA LT_ROW_NO  TYPE LVC_T_ROID.
  DATA LS_ASSETLIST TYPE ZASSETRAK_IMPORT.
  DATA LV_SUCCESS TYPE CHAR1.
  DATA LS_ASSETLIST_ALV TYPE ZASSETRAK_IMPORT_ALV.
  DATA LT_FIELDS TYPE TTY_FIELDS.
  DATA LS_FIELDS TYPE TY_FIELDS.
  DATA LT_SPLIT TYPE TY_ZASSETRAK_IMPORT_TAB.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = LO_GUID.

  CALL METHOD LO_GUID->CHECK_CHANGED_DATA.

  CALL METHOD LO_GUID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_INDEX_ROWS
      ET_ROW_NO     = LT_ROW_NO.

  READ TABLE LT_INDEX_ROWS INTO LS_INDEX_ROW INDEX 1.
  CLEAR: LS_ASSETLIST, LS_ASSETLIST_ALV.
  READ TABLE GT_ASSETLIST_ALV INTO LS_ASSETLIST_ALV INDEX LS_INDEX_ROW-INDEX.
  MOVE-CORRESPONDING LS_ASSETLIST_ALV TO LS_ASSETLIST.
* set flag to refresh data.
  PIS_SELFIELD-REFRESH = 'X'.


* user command?
  CASE PIV_UCOMM.

*   update asset
    WHEN 'UPDATE'.

      IF LS_ASSETLIST IS NOT INITIAL.
        IF LS_ASSETLIST-SPLIT_FLAG = 'S'.
          CLEAR LT_SPLIT[].
          APPEND LS_ASSETLIST TO LT_SPLIT.
          PERFORM SPLIT_UPDATE_ASSET USING LT_SPLIT LS_ASSETLIST-IMP_FILENAME.
          LS_ASSETLIST-PROCESSED_ON = SY-DATUM.
          LS_ASSETLIST-PROCESSED_BY = SY-UNAME.
          LS_ASSETLIST-PRO_STATUS = GC_P.
          LV_SUCCESS = GC_X.
          MODIFY ZASSETRAK_IMPORT FROM LS_ASSETLIST." ACCEPTING DUPLICATE KEYS.
          COMMIT WORK AND WAIT.
        ELSE.
          PERFORM UPDATE_ASSET USING LS_ASSETLIST-IMP_FILENAME 'M' SPACE CHANGING LS_ASSETLIST LV_SUCCESS.
        ENDIF.

        MOVE-CORRESPONDING LS_ASSETLIST TO LS_ASSETLIST_ALV.
        MODIFY GT_ASSETLIST_ALV FROM LS_ASSETLIST_ALV INDEX LS_INDEX_ROW-INDEX TRANSPORTING PROCESSED_BY PROCESSED_ON PROCESSED_BY PRO_STATUS MESSAGE.

        IF LV_SUCCESS = GC_X.
          MESSAGE 'Asset Updated Successfully' TYPE 'S'.
        ELSE.
          MESSAGE 'Error while updating Asset' TYPE 'E'.
        ENDIF.

      ENDIF.


    WHEN 'REJECT'.

      IF LS_ASSETLIST_ALV-PRO_STATUS <> 'R'.
        CLEAR LS_FIELDS.
        LS_FIELDS-TABNAME    = 'ZASSETRAK'.
        LS_FIELDS-FIELDNAME  = 'MESSAGE'.
        LS_FIELDS-FIELD_OBL  = 'X'.
        APPEND LS_FIELDS TO LT_FIELDS.

        CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
          EXPORTING
            FORMNAME        = 'REASON_FOR_REJECTION'
            POPUP_TITLE     = 'Please enter Reason for Rejection'
            PROGRAMNAME     = 'ZASSETRAK_CONTROLLER'
*           START_COLUMN    = '5'
*           START_ROW       = '5'
*           NO_CHECK_FOR_FIXED_VALUES       = ' '
*         IMPORTING
*           RETURNCODE      =
          TABLES
            FIELDS          = LT_FIELDS
          EXCEPTIONS
            ERROR_IN_FIELDS = 1
            OTHERS          = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ELSE.
          READ TABLE LT_FIELDS INTO LS_FIELDS INDEX 1.
          PERFORM UPDATE_ASSET USING LS_ASSETLIST-IMP_FILENAME 'R' LS_FIELDS-VALUE CHANGING LS_ASSETLIST LV_SUCCESS.
          MOVE-CORRESPONDING LS_ASSETLIST TO LS_ASSETLIST_ALV.
          MODIFY GT_ASSETLIST_ALV FROM LS_ASSETLIST_ALV INDEX LS_INDEX_ROW-INDEX TRANSPORTING PROCESSED_BY PROCESSED_ON PRO_STATUS MESSAGE.
        ENDIF.
      ELSE.
        MESSAGE I000(ZASSETRAK) WITH 'Record is already rejected with Reason:' LS_ASSETLIST_ALV-MESSAGE.
      ENDIF.
*
*
**   Download as Excel
*    WHEN 'DOWNLOAD'.
*      PERFORM download.
*      EXIT.



  ENDCASE.

  CLEAR GT_ASSETLIST.
  LOOP AT GT_ASSETLIST_ALV INTO LS_ASSETLIST_ALV.
    MOVE-CORRESPONDING LS_ASSETLIST_ALV TO LS_ASSETLIST.
    APPEND LS_ASSETLIST TO GT_ASSETLIST.
  ENDLOOP.

* refresh data.
  PIS_SELFIELD-REFRESH = 'X'.


ENDFORM.                    "alv_user_command

*&---------------------------------------------------------------------*
*&      Form  reason_for_rejection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIT_FIELDS text
*      -->ERROR      text
*----------------------------------------------------------------------*
FORM REASON_FOR_REJECTION TABLES PIT_FIELDS TYPE TTY_FIELDS
                             USING  ERROR TYPE SVALE.

  DATA LS_FIELDS TYPE TY_FIELDS.

*  READ TABLE pit_fields INTO ls_fields INDEX 1.
*  IF sy-subrc = 0.
*    IF  strlen( ls_fields-value ) < 2.
*
*      error-msgid = 'ZASSETRAK'.
**error-MSGTY =
*      error-msgno = '001'.
*
*    ENDIF.
*  ELSE.
*
*  ENDIF.

ENDFORM.                    "reason_for_rejection

*&---------------------------------------------------------------------*
*&      Form  set_eventexit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_EVENTEXIT .
*

  GS_EVENTEXIT-UCOMM = '&AVE'.
  GS_EVENTEXIT-AFTER = 'X'.
  APPEND GS_EVENTEXIT TO GT_EVENTEXITS.

ENDFORM.                    " set_eventexit
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_BUKRS_LOW  text
*----------------------------------------------------------------------*
FORM GET_COMPANY_F4  CHANGING PCS_BUKRS.

  DATA: LT_RET TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD   = 'BUKRS' "field name in your value tab i.e it_tab
      VALUE_ORG  = 'S'
    TABLES
      VALUE_TAB  = GT_ASSET_COMPANY[]
      RETURN_TAB = LT_RET[].
  IF SY-SUBRC EQ 0.
    READ TABLE LT_RET INDEX 1.
    PCS_BUKRS = LT_RET-FIELDVAL.
  ENDIF.
ENDFORM.                    " GET_COMPANY_F4
*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDATION .


  DATA LS_COMPANY TYPE TY_COMP.

  IF S_BUKRS IS INITIAL.
    LOOP AT GT_ASSET_COMPANY INTO LS_COMPANY.
      S_BUKRS-SIGN = 'I'.
      S_BUKRS-OPTION = 'EQ'.
      S_BUKRS-LOW = LS_COMPANY-BUKRS.
      APPEND S_BUKRS.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " VALIDATION
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_COLOR .

* define local data
  DATA: LS_COLOR    TYPE LVC_S_SCOL,
        LD_IDX      TYPE I.

  DATA LS_ASSETLIST_ALV TYPE ZASSETRAK_IMPORT_ALV.

  LOOP AT GT_ASSETLIST_ALV INTO LS_ASSETLIST_ALV.


    LD_IDX = SYST-TABIX.
    REFRESH: LS_ASSETLIST_ALV-CELLCOLOR.
    CLEAR: LS_COLOR.

    IF ( LS_ASSETLIST_ALV-PRO_STATUS = 'R' OR LS_ASSETLIST_ALV-PRO_STATUS = 'E' ).

      IF LS_ASSETLIST_ALV-PRO_STATUS = 'R'.

        LS_COLOR-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_POSITIVE.

      ELSEIF LS_ASSETLIST_ALV-PRO_STATUS = 'E'..

        LS_COLOR-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_NEGATIVE.

      ENDIF.

      LS_COLOR-FNAME = 'PRO_STATUS'.
      LS_COLOR-COLOR-INT = 0.
      LS_COLOR-COLOR-INV = 0.
      APPEND LS_COLOR TO LS_ASSETLIST_ALV-CELLCOLOR.
    ENDIF.

    MODIFY GT_ASSETLIST_ALV FROM LS_ASSETLIST_ALV INDEX LD_IDX.


  ENDLOOP.

ENDFORM.                    " SET_COLOR


*&---------------------------------------------------------------------*
*&      Form  read_tvarvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_TVARVC.
  DATA LS_TVARVC TYPE TVARVC.
  DATA LS_RANGE LIKE LINE OF GT_TCODENEW.

  SELECT * INTO TABLE GT_TVARVC FROM TVARVC WHERE NAME LIKE 'ZASSETRAK_%'.


* read file path details.
*  READ TABLE gt_tvarvc INTO ls_tvarvc WITH KEY name = 'ZASSETRAK_FILEPATH'.
*  IF sy-subrc = 0.
*    gv_path = ls_tvarvc-low.
*  ELSE.
  GV_PATH = '/1500/'.
*  ENDIF.

*  IF sy-uname = 'ABAPUSER1'.
*    gv_path =  'C:\USERS\ABAPUSER1\AT\'.
*  ELSEIF sy-uname = 'ABAPUSER2'.
*    gv_path =  'C:\USERS\ABAPUSER2\AT\'.
*  ELSEIF sy-uname = 'ABAPUSER3'.
*    gv_path =  'C:\USERS\ABAPUSER3\AT\'.
*  ENDIF.

  IF GV_PATH IS INITIAL.
    GV_PATH = '/Assetrak Out/'.
  ENDIF.

  CONCATENATE SY-DATUM SY-UZEIT INTO GV_DATETIME.

* collect t-code for new and change.
  LOOP AT GT_TVARVC INTO LS_TVARVC.

    CLEAR LS_RANGE.
    LS_RANGE-SIGN	    = LS_TVARVC-SIGN.
    LS_RANGE-OPTION	  = LS_TVARVC-OPTI.
    LS_RANGE-LOW      = LS_TVARVC-LOW.
    LS_RANGE-HIGH	    = LS_TVARVC-HIGH.


    IF LS_TVARVC-NAME = 'ZASSETRAK_TCODENEW'.
      APPEND LS_RANGE TO GT_TCODENEW.
    ENDIF.


    IF LS_TVARVC-NAME = 'ZASSETRAK_TCODECHANGE'.
      APPEND LS_RANGE TO GT_TCODECHANGE.
    ENDIF.

  ENDLOOP.

* set default if not set in TVARVC
  IF GT_TCODENEW IS INITIAL.
    CLEAR LS_RANGE.
    LS_RANGE-SIGN	    = 'I'.
    LS_RANGE-OPTION	  = 'EQ'.
    LS_RANGE-LOW      = 'AS01'.

    APPEND LS_RANGE TO GT_TCODENEW.

  ENDIF.
  IF GT_TCODECHANGE IS INITIAL.
    CLEAR LS_RANGE.
    LS_RANGE-SIGN	    = 'I'.
    LS_RANGE-OPTION	  = 'EQ'.
    LS_RANGE-LOW      = 'AS02'.
    APPEND LS_RANGE TO GT_TCODECHANGE.
  ENDIF.


  APPEND LINES OF GT_TCODENEW TO GT_TCODEALL.
  APPEND LINES OF GT_TCODECHANGE TO GT_TCODEALL.

ENDFORM.                    "read_tvarvc


*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.

  DATA : T_HEXTAB TYPE TABLE OF STRING,

             LV_COUNT  TYPE I,
             GV_EMAIL         TYPE ADR6-SMTP_ADDR,
             T_BODY    TYPE TABLE OF SOLISTI1,
             GR_RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
             LT_CONTENTS TYPE SOLI_TAB,
             LV_ATTACH_NAME TYPE SOOD-OBJDES,
             LV_SUBJECT TYPE SO_OBJ_DES,
             LO_DOCUMENT TYPE REF TO CL_DOCUMENT_BCS,
             LO_MAIL TYPE REF TO CL_BCS,
             LS_CONTENTS LIKE LINE OF LT_CONTENTS,
             LV_FILENAME TYPE STRING,
             LV_RESULT TYPE OS_BOOLEAN,
            LV_STRING TYPE STRING.

  DATA LS_TVARVC TYPE TVARVC.


* subject
  LV_SUBJECT = 'Asset Audit : Data sent to Assetrak'.

* mail body
  LS_CONTENTS-LINE = 'Hello,'.
  APPEND LS_CONTENTS TO LT_CONTENTS.
  LS_CONTENTS-LINE = 'Attached data will be sent to Assetrak for Audit.'.
  APPEND LS_CONTENTS TO LT_CONTENTS.

* CREATE THE DOCUMENT WITH CONTENTS
  CREATE OBJECT LO_DOCUMENT.
  LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE       = 'HTM'
      I_SUBJECT    = LV_SUBJECT
      I_LENGTH     = '1000'
      I_LANGUAGE   = SY-LANGU
      I_IMPORTANCE = '1'
      I_TEXT       = LT_CONTENTS ).

* ATTACH THE FILE, THE ATTACHMENT TYPE SHOULD BE BIN TO ACCEPT ANY KIND OF ATTACHMENT,


  PERFORM ADD_ATTACHMENT USING 'ComapanyData.txt' GX_COMPANYDATA CHANGING LO_DOCUMENT.
  PERFORM ADD_ATTACHMENT USING 'PlantData.txt' GX_PLANTDATA CHANGING LO_DOCUMENT.
  PERFORM ADD_ATTACHMENT USING 'PlantStorageLocationData.txt' GX_PLANTSTORAGELOCATION CHANGING LO_DOCUMENT.
  PERFORM ADD_ATTACHMENT USING 'AssetClassData.txt' GX_ASSETCLASS CHANGING LO_DOCUMENT.
  PERFORM ADD_ATTACHMENT USING 'AssetData.txt' GX_ASSETDATA CHANGING LO_DOCUMENT.

*gx_plantdata
*gx_plantstoragelocation
*gx_assetclass
*gx_assetdata
*


* CREATING PERSISTENT OBJECT WILL ALLOW YOU TO SET THE DOCUMENT IN THE MAIL
  LO_MAIL = CL_BCS=>CREATE_PERSISTENT( ).
  CALL METHOD LO_MAIL->SET_DOCUMENT( LO_DOCUMENT ).

  LOOP AT GT_TVARVC INTO LS_TVARVC WHERE NAME = 'ZASSETRAK_MAILID' .

*   EMAIL AS GIVEN IN THE TVARVC.
    GV_EMAIL = LS_TVARVC-LOW.

    GR_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( GV_EMAIL ).

*   Add recipient to send request
    CALL METHOD LO_MAIL->ADD_RECIPIENT
      EXPORTING
        I_RECIPIENT = GR_RECIPIENT
        I_EXPRESS   = 'X'.


  ENDLOOP.


* SEND THE MAIL
  CALL METHOD LO_MAIL->SEND(
    EXPORTING
      I_WITH_ERROR_SCREEN = 'X'
    RECEIVING
      RESULT              = LV_RESULT ).
* YOU CAN VERIFY THE STATUS IN THE LIST, YOU CAN ALSO SUBMIT THIS AS A BACKGROUND JOB.
  IF LV_RESULT = 'X'.
*    WRITE :/ , 1(9) lv_count, 10(55) file_table-pathname.
*    WRITE : 56(1) '-', 58(20) 'Mail sent'(003).
    COMMIT WORK.
  ELSE.
*    WRITE :/ , 1(9) lv_count, 10(55) file_table-pathname.
*    WRITE : 56(1) '-', 58(30) 'Error in sending Mail'(004).
    ROLLBACK WORK.
  ENDIF.
  REFRESH LT_CONTENTS[].




ENDFORM.                    "send_email
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_XSTRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DATA  text
*      <--P_GX_COMPANYDATA  text
*----------------------------------------------------------------------*
FORM CONVERT_TO_XSTRING  USING    PIT_DATA TYPE TRUXS_T_TEXT_DATA
                         CHANGING PCX_XSTRING TYPE XSTRING.


  DATA LV_STRING TYPE STRING.
  DATA LV_DATALINE(4096) TYPE C.

  LOOP AT PIT_DATA INTO LV_DATALINE.
    IF SY-TABIX = 1.
      LV_STRING = LV_DATALINE.
    ELSE.
      CONCATENATE LV_STRING LV_DATALINE INTO LV_STRING SEPARATED BY CL_ABAP_CHAR_UTILITIES=>CR_LF.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      TEXT     = LV_STRING
*     MIMETYPE = ' '
*     ENCODING =
    IMPORTING
      BUFFER   = PCX_XSTRING
    EXCEPTIONS
      FAILED   = 1
      OTHERS   = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " CONVERT_TO_XSTRING
*&---------------------------------------------------------------------*
*&      Form  ADD_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ATTACH_NAME  text
*      -->P_GX_COMPANYDATA  text
*      <--P_LO_DOCUMENT  text
*----------------------------------------------------------------------*
FORM ADD_ATTACHMENT  USING    PIV_ATTACH_NAME TYPE SOOD-OBJDES
                              PIV_XSTRING TYPE XSTRING
                     CHANGING PCO_DOCUMENT TYPE REF TO CL_DOCUMENT_BCS.

  DATA LT_MAILHEX TYPE SOLIX_TAB.

  IF PIV_XSTRING IS NOT INITIAL.

    CALL METHOD CL_BCS_CONVERT=>XSTRING_TO_SOLIX
      EXPORTING
        IV_XSTRING = PIV_XSTRING
      RECEIVING
        ET_SOLIX   = LT_MAILHEX.



    CALL METHOD PCO_DOCUMENT->ADD_ATTACHMENT
      EXPORTING
        I_ATTACHMENT_TYPE    = 'TXT'
        I_ATTACHMENT_SUBJECT = PIV_ATTACH_NAME
        I_ATT_CONTENT_HEX    = LT_MAILHEX.

  ENDIF.


ENDFORM.                    " ADD_ATTACHMENT
*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FILENAME  text
*      -->P_LT_DATA  text
*----------------------------------------------------------------------*
FORM CREATE_FILE  USING    PIV_NAME TYPE STRING
                           PIV_HDRLNCNT TYPE SY-TABIX
                           PIT_DATA TYPE TRUXS_T_TEXT_DATA.

  DATA LV_FILENAME_S TYPE CHAR200.  " .ack file name
  DATA LV_FILENAME_T TYPE CHAR200.  " data file name
  DATA LV_LINES TYPE SY-TABIX.
  DATA LT_LINES TYPE STANDARD TABLE OF SY-TABIX.

* get record count for ACK file
  DESCRIBE TABLE PIT_DATA LINES LV_LINES.

  LV_LINES = LV_LINES - PIV_HDRLNCNT.
  APPEND LV_LINES TO LT_LINES.

* ack file

  CLEAR LV_FILENAME_S.
  CONCATENATE GV_PATH PIV_NAME GV_DATETIME '.ack' INTO LV_FILENAME_S.
* '.ack'
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename = lv_filename_s
*      filetype = 'ASC'
*    TABLES
*      data_tab = lt_lines.
*  IF sy-subrc <> 0.
**         Implement suitable error handling here
*    MESSAGE 'Error in downloading file' TYPE 'E'.
*  ENDIF.

*  data file

  CLEAR LV_FILENAME_T.
  CONCATENATE  GV_PATH PIV_NAME GV_DATETIME '.txt' INTO LV_FILENAME_T.

*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename = lv_filename_t
*      filetype = 'ASC'
*    TABLES
*      data_tab = pit_data.
*  IF sy-subrc <> 0.
**         Implement suitable error handling here
*    MESSAGE 'Error in downloading file' TYPE 'E'.
*  ENDIF.

  PERFORM FTP_FILE USING  LV_FILENAME_S
                          LV_FILENAME_T
                   CHANGING LT_LINES
                            PIT_DATA  .
  PERFORM TRANSFER_FILE USING  LV_FILENAME_S
                                LV_FILENAME_T
                         CHANGING LT_LINES
                                  PIT_DATA  .

ENDFORM.                    " CREATE_FILE
*&---------------------------------------------------------------------*
*&      Form  ASSING_BLANKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1160   text
*----------------------------------------------------------------------*
FORM ASSING_BLANKS  USING    PIV_FIELDNAME TYPE FIELDNAME CHANGING PCS_LINE.

  FIELD-SYMBOLS <LF_FIELD>.

  ASSIGN COMPONENT PIV_FIELDNAME OF STRUCTURE PCS_LINE TO <LF_FIELD>.
  IF SY-SUBRC = 0 AND PIV_FIELDNAME NE 'KANSW' AND PIV_FIELDNAME NE 'KNAFA' AND PIV_FIELDNAME NE 'NAFAG' AND
                                                PIV_FIELDNAME NE 'ANSWL' AND PIV_FIELDNAME EQ 'WDV_AMOUNT'.
    IF <LF_FIELD> IS INITIAL .
      <LF_FIELD> = SPACE.
    ENDIF.
  ELSE.
    <LF_FIELD> = SPACE.
  ENDIF.

ENDFORM.                    " ASSING_BLANKS
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM ASSIGN_LOCATION USING PIV_BUKRS TYPE BUKRS
                           PIV_ANLN1 TYPE ANLN1
                           PIV_ANLN2 TYPE ANLN2
                  CHANGING PIS_LINE.

  DATA LS_WERKS TYPE WERKS_D.
  DATA LS_STORT TYPE STORT.
  FIELD-SYMBOLS: <LF_WERKS>, <LF_STORT>.

* select location from ANLA-WERK
  SELECT SINGLE STORT WERKS INTO (LS_STORT, LS_WERKS) FROM ANLZ WHERE BUKRS = PIV_BUKRS AND
  ANLN1 = PIV_ANLN1 AND
  ANLN2 = PIV_ANLN2.

  IF SY-SUBRC = 0.
*   assign changed value of field.
    ASSIGN COMPONENT 'WERKS' OF STRUCTURE PIS_LINE TO <LF_WERKS>.
    IF <LF_WERKS> IS ASSIGNED.
      IF LS_WERKS IS INITIAL.
        <LF_WERKS> = '/'.
      ELSE.
        <LF_WERKS> = LS_WERKS.
      ENDIF.
    ENDIF.

*   assign changed value of field.
    ASSIGN COMPONENT 'STORT' OF STRUCTURE PIS_LINE TO <LF_STORT>.
    IF <LF_STORT> IS ASSIGNED.
      IF LS_STORT IS INITIAL.
        <LF_STORT> = '/'.
      ELSE.
        <LF_STORT> = LS_STORT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " ASSIGN_LOCATION

*&---------------------------------------------------------------------*
*&      Form  assign_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIV_BUKRS  text
*      -->PIV_ANLN1  text
*      -->PIV_ANLN2  text
*      <--PIS_LINE   text
*----------------------------------------------------------------------*
FORM ASSIGN_COST USING     PIV_BUKRS TYPE BUKRS
                           PIV_ANLN1 TYPE ANLN1
                           PIV_ANLN2 TYPE ANLN2
                  CHANGING PIS_LINE.

  DATA LS_ANSWL TYPE ANSWL.
  DATA LS_KANSW TYPE KANSW.
  FIELD-SYMBOLS: <LF_KANSW>, <LF_ANSWL>.

* select location from ANLA-WERK
  SELECT SINGLE ANSWL KANSW INTO (LS_ANSWL, LS_KANSW) FROM ANLC WHERE BUKRS = PIV_BUKRS AND
  ANLN1 = PIV_ANLN1 AND
  ANLN2 = PIV_ANLN2 AND
  GJAHR IN S_GJAHR AND
  AFABE IN S_AFABE  .

  IF SY-SUBRC = 0.
*   assign changed value of field.
    ASSIGN COMPONENT 'KANSW' OF STRUCTURE PIS_LINE TO <LF_KANSW>.
    IF <LF_KANSW> IS ASSIGNED.
      <LF_KANSW> = LS_KANSW.
    ENDIF.

*   assign changed value of field.
    ASSIGN COMPONENT 'ANSWL' OF STRUCTURE PIS_LINE TO <LF_ANSWL>.
    IF <LF_ANSWL> IS ASSIGNED.
      <LF_ANSWL> = LS_ANSWL.
    ENDIF.

  ENDIF.

ENDFORM.                    " ASSIGN_COST
*&---------------------------------------------------------------------*
*&      Form  CHECKIMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3732   text
*      -->P_3733   text
*      <--P_LV_IMPORT  text
*----------------------------------------------------------------------*
FORM CHECKIMPORT  USING    PIV_TABLE TYPE ZASSET_TABLES
                           PIV_FIELD TYPE FIELDNAME
                  CHANGING PCV_IMPORT TYPE CHAR1.

  DATA LS_CONFIG TYPE ZASSETRAK_CONFIG.

  CLEAR LS_CONFIG.
  READ TABLE GT_CONFIG INTO LS_CONFIG WITH KEY TABLENAME = PIV_TABLE
                                                SAPFIELD = PIV_FIELD.
  IF SY-SUBRC = 0 AND LS_CONFIG-IMPORT = GC_X.
    PCV_IMPORT = GC_X.

  ENDIF.

ENDFORM.                    " CHECKIMPORT
*&---------------------------------------------------------------------*
*&      Form  ASSET_VALUE_EXTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSET_VALUE_EXTRACT .

  TYPES : BEGIN OF T_ANLA,
          ANLN1 TYPE ANLA-ANLN1,
          ANLN2 TYPE ANLA-ANLN2,
          ANLKL TYPE ANLA-ANLKL,
          END OF T_ANLA.

  DATA : TM_ANLA TYPE TABLE OF T_ANLA,
         WM_ANLA TYPE T_ANLA.

  DATA : IT_ASST TYPE TABLE OF T_ASSET,
         WA_ASST TYPE T_ASSET.

  DATA : ANLB_IT TYPE TABLE OF ANLB_T,
         ANLB_WA TYPE ANLB_T.

  DATA LT_ASSETVALUE_BASE TYPE STANDARD TABLE OF TY_ASSETVALUES_BASE.
  DATA LS_ASSETVALUE_BASE TYPE TY_ASSETVALUES_BASE.
  DATA LO_TABLE_LINE      TYPE REF TO DATA.
  FIELD-SYMBOLS: <LF_TAB>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <LF_LINE>,   <LF_FIELD>.

  TYPES : BEGIN OF TY_DATA,
      FIELD1 TYPE ANLA-ANLN1,"(050),
      FIELD2 TYPE ANLA-ANLN2,"(050),
      FIELD3(050),
      FIELD4(050),
      FIELD5(050),
      FIELD6(050),
      FIELD7(050),
      FIELD8(050),
      FIELD9(050),
      FIELD10(050),
      FIELD11(050),
      FIELD12(050),
      FIELD13(050),
      FIELD14(050),
      FIELD15(050),
      FIELD16(050),
      END OF TY_DATA.

  DATA : IT_DATA  TYPE TABLE OF TY_DATA,
         WA_DATA  TYPE TY_DATA.

  DATA : LV_DATE1 TYPE SY-DATUM,
         LV_DATE2 TYPE SY-DATUM.

  DATA : ITAB_LIST TYPE TABLE OF ABAPLIST WITH HEADER LINE.

  DATA: BEGIN OF VLIST OCCURS 0,
        FIELD1(1) TYPE C,
        FIELD2(1000) TYPE C,
        END OF VLIST.

  DATA : VSAID TYPE I.
  DATA : VSAI1 TYPE STRING.


* get ref of global table
  ASSIGN GO_TABLE->* TO <LF_TAB>.

* Create dynamic work area and assign to Field Symbol
  CREATE DATA LO_TABLE_LINE LIKE LINE OF <LF_TAB>.
  ASSIGN LO_TABLE_LINE->* TO <LF_LINE>.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = SY-DATUM
    IMPORTING
      LAST_DAY_OF_MONTH = LV_DATE2.

  SUBMIT RAGITT_ALV01  USING SELECTION-SCREEN '1000'
    WITH BUKRS IN S_BUKRS
    WITH BERDATUM EQ LV_DATE2
    WITH BEREICH1 EQ '01'
    WITH XEINZEL EQ 'X'
    WITH PA_GITVS EQ '0001'
    WITH PA_XGBAF EQ 'X'
    EXPORTING LIST TO MEMORY AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      LISTOBJECT = ITAB_LIST
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      LISTASCI   = VLIST
      LISTOBJECT = ITAB_LIST.

  LOOP AT VLIST.

    SPLIT VLIST-FIELD2 AT '|' INTO WA_DATA-FIELD1
                                   WA_DATA-FIELD2
                                   WA_DATA-FIELD3
                                   WA_DATA-FIELD4
                                   WA_DATA-FIELD5
                                   WA_DATA-FIELD6
                                   WA_DATA-FIELD7
                                   WA_DATA-FIELD8
                                   WA_DATA-FIELD9
                                   WA_DATA-FIELD10
                                   WA_DATA-FIELD11
                                   WA_DATA-FIELD12
                                   WA_DATA-FIELD13
                                   WA_DATA-FIELD14
                                   WA_DATA-FIELD15.

    CONDENSE : WA_DATA-FIELD1,WA_DATA-FIELD2,WA_DATA-FIELD3,WA_DATA-FIELD4,WA_DATA-FIELD5,WA_DATA-FIELD6.
    IF WA_DATA-FIELD1+0(1) NE '-'.
      APPEND WA_DATA TO IT_DATA.
    ENDIF.
  ENDLOOP.
  DELETE IT_DATA WHERE FIELD2 EQ ' '.
  DELETE IT_DATA WHERE FIELD1 EQ 'Asset'.
  DELETE IT_DATA WHERE FIELD11 EQ ' '.

* select data from ANLC
  SELECT BUKRS ANLN1 ANLN2 KANSW KNAFA NAFAG NAFAV NAFAL ANSWL
    FROM ANLC
    INTO CORRESPONDING FIELDS OF TABLE LT_ASSETVALUE_BASE
  WHERE ANLC~BUKRS IN S_BUKRS AND
    GJAHR IN S_GJAHR AND
    AFABE IN S_AFABE .      "'01'.

  SELECT BUKRS ANLN1 ANLN2 KANSW KAAFA
    FROM ANLC
    INTO CORRESPONDING FIELDS OF TABLE IT_ASST
    WHERE ANLC~BUKRS IN S_BUKRS AND
    GJAHR IN S_GJAHR AND
    AFABE IN S_AFABE .      "'01'.

  LOOP AT LT_ASSETVALUE_BASE INTO LS_ASSETVALUE_BASE.
    READ TABLE IT_ASST INTO WA_ASST WITH KEY ANLN1 = LS_ASSETVALUE_BASE-ANLN1 ANLN2 = LS_ASSETVALUE_BASE-ANLN2.
    IF WA_ASST-KAAFA IS NOT INITIAL.
      LS_ASSETVALUE_BASE-KNAFA = LS_ASSETVALUE_BASE-KNAFA + WA_ASST-KAAFA.
    ENDIF.
    MODIFY LT_ASSETVALUE_BASE FROM LS_ASSETVALUE_BASE TRANSPORTING KNAFA.
    CLEAR : LS_ASSETVALUE_BASE , WA_ASST.
  ENDLOOP.

  IF LT_ASSETVALUE_BASE IS NOT INITIAL.

    SELECT ANLN1 ANLN2 ANLKL INTO TABLE TM_ANLA FROM ANLA
      FOR ALL ENTRIES IN LT_ASSETVALUE_BASE
      WHERE ANLN1 = LT_ASSETVALUE_BASE-ANLN1 AND ANLN2 = LT_ASSETVALUE_BASE-ANLN2 AND ANLKL = 'Z100'.

    SELECT ANLN1 ANLN2 ABGDAT_B FROM ANLB INTO TABLE ANLB_IT
      FOR ALL ENTRIES IN LT_ASSETVALUE_BASE
      WHERE ANLN1 = LT_ASSETVALUE_BASE-ANLN1 AND ANLN2 = LT_ASSETVALUE_BASE-ANLN2.

  ENDIF.

  LOOP AT TM_ANLA INTO WM_ANLA.

    READ TABLE LT_ASSETVALUE_BASE INTO LS_ASSETVALUE_BASE WITH KEY ANLN1 = WM_ANLA-ANLN1 ANLN2 = WM_ANLA-ANLN2.
    IF SY-SUBRC = 0.
      DELETE LT_ASSETVALUE_BASE WHERE ANLN1 = WM_ANLA-ANLN1 AND ANLN2 = WM_ANLA-ANLN2.
    ENDIF.

    CLEAR : WM_ANLA , LS_ASSETVALUE_BASE.

  ENDLOOP.

  BREAK 1000868.

  LOOP AT LT_ASSETVALUE_BASE INTO LS_ASSETVALUE_BASE.

    SHIFT LS_ASSETVALUE_BASE-ANLN1 LEFT DELETING LEADING '0'.
    VSAID = LS_ASSETVALUE_BASE-ANLN2.
    VSAI1 = VSAID.

    READ TABLE IT_DATA INTO WA_DATA WITH KEY FIELD1 = LS_ASSETVALUE_BASE-ANLN1 FIELD2 = VSAI1.
    IF SY-SUBRC = 0.

      CALL FUNCTION 'HRCM_STRING_TO_AMOUNT_CONVERT'
        EXPORTING
          STRING              = WA_DATA-FIELD7
          DECIMAL_SEPARATOR   = '.'
          THOUSANDS_SEPARATOR = ','
        IMPORTING
          BETRG               = LS_ASSETVALUE_BASE-NAFAG.

    ENDIF.

    MOVE-CORRESPONDING LS_ASSETVALUE_BASE TO <LF_LINE>.
*    READ TABLE ANLB_IT INTO ANLB_WA WITH KEY ANLN1 = LS_ASSETVALUE_BASE-ANLN1 ANLN2 = LS_ASSETVALUE_BASE-ANLN2.
*     assign changed value of field.
    ASSIGN COMPONENT 'NAFAG' OF STRUCTURE <LF_LINE> TO <LF_FIELD>.
    IF <LF_FIELD> IS ASSIGNED.

*      <LF_FIELD> = LS_ASSETVALUE_BASE-NAFAG + LS_ASSETVALUE_BASE-NAFAV ."+ LS_ASSETVALUE_BASE-NAFAL.
*
*      IF ANLB_WA-ABGDAT_B IS NOT INITIAL AND <LF_FIELD> > 0 .
*
*        READ TABLE IT_ASST INTO WA_ASST WITH KEY ANLN1 = LS_ASSETVALUE_BASE-ANLN1 ANLN2 = LS_ASSETVALUE_BASE-ANLN2.
*
      <LF_FIELD> = LS_ASSETVALUE_BASE-NAFAG.
*
*      ENDIF.



    ENDIF.

    APPEND <LF_LINE> TO <LF_TAB>.
    CLEAR <LF_LINE>.

    CLEAR ANLB_WA.
    CLEAR : LS_ASSETVALUE_BASE,VSAID,VSAI1.

  ENDLOOP.



ENDFORM.                    " ASSET_VALUE_EXTRACT

*&---------------------------------------------------------------------*
*&      Form  SEND_REJECTED_ASSETS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_REJECTED_ASSETS .

  DATA LV_DATE TYPE SY-DATUM.
  DATA LT_DATA TYPE TRUXS_T_TEXT_DATA .
  DATA LT_TAB_DELIMITED TYPE TRUXS_T_TEXT_DATA .
* delete 1 year old data
  LV_DATE = SY-DATUM - 365.

* delete old asset import records which are processed
  DELETE FROM ZASSETRAK_IMPORT WHERE PRO_STATUS <> 'N' AND PROCESSED_ON < LV_DATE.
  COMMIT WORK AND WAIT.

* select rejected data to send

  GV_INDXKEY = 'ASSET_REJECT'.
*     get last extraction date
  IMPORT GV_EXTRACTDATE TO GV_EXTRACTDATE FROM DATABASE INDX(ZA)
                                ID GV_INDXKEY.
  IF GV_EXTRACTDATE IS INITIAL.
    GV_EXTRACTDATE = '20000101'.
  ENDIF.
* extract till yesterday
  LV_DATE = SY-DATUM .

  SELECT IMP_FILENAME BUKRS ANLN1 ANLN2 CHILD_ID PROCESSED_BY PROCESSED_ON PRO_STATUS MESSAGE
      INTO TABLE GT_ASSET_REJECTED
    FROM ZASSETRAK_IMPORT WHERE PRO_STATUS = 'R' AND
       PROCESSED_ON > GV_EXTRACTDATE AND PROCESSED_ON <= LV_DATE.
  IF SY-SUBRC = 0.
    CONCATENATE 'File Name' 'Company Code' 'Asset Number' 'Subasset Number'
      'Child ID' 'Rejected By' 'Rejected On' 'Status' 'Reason'
        INTO GV_LINE1 SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

    APPEND GV_LINE1 TO LT_DATA.


    CALL FUNCTION 'ZAT_CONVERT_TO_TEX_FORMAT'
      EXPORTING
        I_FIELD_SEPERATOR    = GC_FIELD_SEPERATOR
      TABLES
        I_TAB_SAP_DATA       = GT_ASSET_REJECTED
      CHANGING
        I_TAB_CONVERTED_DATA = LT_TAB_DELIMITED
      EXCEPTIONS
        CONVERSION_FAILED    = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
*             Implement suitable error handling here
      MESSAGE 'Error in converting Asset data to TAB Delimited format' TYPE 'E'.

    ELSE.

      APPEND LINES OF LT_TAB_DELIMITED TO LT_DATA.

*     conver data to bin format
      PERFORM CONVERT_TO_XSTRING USING LT_DATA CHANGING GX_ASSETDATA.


      PERFORM CREATE_FILE USING 'Rejected_Asset' 1 LT_DATA.

      GV_INDXKEY = 'ASSET_REJECT'.
*     get last extraction date
      GV_EXTRACTDATE = LV_DATE.
*     set last extraction date
      EXPORT GV_EXTRACTDATE TO DATABASE INDX(ZA) ID GV_INDXKEY.


    ENDIF.

  ELSE.
*   update extract date
    GV_INDXKEY = 'ASSET_REJECT'.
*   set last extraction date
    GV_EXTRACTDATE = LV_DATE.
    EXPORT GV_EXTRACTDATE TO DATABASE INDX(ZA) ID GV_INDXKEY.
  ENDIF.
ENDFORM.                    " SEND_REJECTED_ASSETS
*&---------------------------------------------------------------------*
*&      Form  SERNR_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LF_LINE>  text
*----------------------------------------------------------------------*
FORM SERNR_CONVERSION_EXIT  CHANGING PCS_LINE.


  FIELD-SYMBOLS : <LF_FIELD>, <LF_SERNR>.

  ASSIGN COMPONENT 'SERNR' OF STRUCTURE PCS_LINE TO <LF_SERNR>.

  IF <LF_SERNR> IS ASSIGNED AND SY-SUBRC = 0.

    CALL FUNCTION 'CONVERSION_EXIT_GERNR_OUTPUT'
      EXPORTING
        INPUT  = <LF_SERNR>
      IMPORTING
        OUTPUT = <LF_SERNR>.


  ENDIF.




ENDFORM.                    " SERNR_CONVERSION_EXIT

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FILENAME  text
*      -->P_LV_FILENAME  text
*      <--P_LT_LINES  text
*      <--P_LT_DATA  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FILENAME  text
*      -->P_LV_FILENAME  text
*      <--P_LT_LINES  text
*      <--P_LT_DATA  text
*----------------------------------------------------------------------*

FORM TRANSFER_FILE  USING  PIV_FILENAME_S
                        PIV_FILENAME_T
                        PIT_LINES TYPE TABLE"TYPE zsyst_tabix
                        PIT_DATA TYPE TRUXS_T_TEXT_DATA.

  CLEAR GV_FILE.
  DATA : L_LINES_FILE TYPE STRING,  "file name
        L_FILE TYPE AUTHB-FILENAME,  "filename
        L_DATA_FILE TYPE STRING,
        LWA_LINES TYPE CHAR10.
  FIELD-SYMBOLS   : <L1_DATA>.

*  DATA : result type.

  L_LINES_FILE = PIV_FILENAME_S.
  L_FILE = PIV_FILENAME_S.

  IF PIT_LINES IS NOT INITIAL.
*  Check authorization for file contaning no of lines.
    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
*    PROGRAM                =
        ACTIVITY               = SABC_ACT_WRITE
        FILENAME               = L_FILE
*  EXCEPTIONS
*    NO_AUTHORITY           = 1
*    ACTIVITY_UNKNOWN       = 2
*    OTHERS                 = 3
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here

    ELSE.
*   Write file lines
      OPEN DATASET L_LINES_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF SY-SUBRC = 0.
        LOOP AT PIT_LINES INTO LWA_LINES.
          TRANSFER LWA_LINES TO L_LINES_FILE.
        ENDLOOP.
        CLOSE DATASET L_LINES_FILE.
      ENDIF.
    ENDIF.
  ENDIF.


  IF PIT_DATA IS NOT INITIAL.

    L_DATA_FILE = PIV_FILENAME_T.
    L_FILE = PIV_FILENAME_T.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
*    PROGRAM                =
        ACTIVITY               = SABC_ACT_WRITE
        FILENAME               = L_FILE
*  EXCEPTIONS
*    NO_AUTHORITY           = 1
*    ACTIVITY_UNKNOWN       = 2
*    OTHERS                 = 3
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here

    ELSE.
*   Write file lines
      OPEN DATASET L_DATA_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF SY-SUBRC = 0.
        LOOP AT PIT_DATA ASSIGNING <L1_DATA>.
          TRANSFER <L1_DATA> TO L_DATA_FILE.
        ENDLOOP.
        CLOSE DATASET L_DATA_FILE.
      ENDIF.
    ENDIF.


  ENDIF.
ENDFORM.                   " TRANSFER_FILE

*&---------------------------------------------------------------------*
*&      Form  EMAIL_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MAILTXT  text
*----------------------------------------------------------------------*
FORM EMAIL_ERROR  USING    P_LT_MAILTXT.

* Data Declarations
  DATA: LT_MAILSUBJECT     TYPE SODOCCHGI1.
  DATA: LT_MAILRECIPIENTS  TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE.
  DATA: LT_MAILTXT         TYPE STANDARD TABLE OF SOLI      WITH HEADER LINE.
* Recipients
  LT_MAILRECIPIENTS-REC_TYPE  = 'U'.
  LT_MAILRECIPIENTS-RECEIVER = 'falgun@arhata.com'.
  APPEND LT_MAILRECIPIENTS .
  LT_MAILRECIPIENTS-REC_TYPE  = 'U'.
  LT_MAILRECIPIENTS-RECEIVER = 'vinayak.c@assetrak.in'.
  APPEND LT_MAILRECIPIENTS .
  CLEAR LT_MAILRECIPIENTS .
* Subject.
  LT_MAILSUBJECT-OBJ_NAME = 'TEST'.
  LT_MAILSUBJECT-OBJ_LANGU = SY-LANGU.
  LT_MAILSUBJECT-OBJ_DESCR = 'Data import eroor'.
* Mail Contents
  LT_MAILTXT = P_LT_MAILTXT.
  APPEND LT_MAILTXT. CLEAR LT_MAILTXT.
* Send Mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = LT_MAILSUBJECT
    TABLES
      OBJECT_CONTENT             = LT_MAILTXT
      RECEIVERS                  = LT_MAILRECIPIENTS
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.


ENDFORM.                    "email_error

*&---------------------------------------------------------------------*
*&      Form  send_email_for_import
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEND_EMAIL_FOR_IMPORT.

  DATA : T_HEXTAB TYPE TABLE OF STRING,

             LV_COUNT  TYPE I,
             GV_EMAIL         TYPE ADR6-SMTP_ADDR,
             T_BODY    TYPE TABLE OF SOLISTI1,
             GR_RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
             LT_CONTENTS TYPE SOLI_TAB,
             LV_ATTACH_NAME TYPE SOOD-OBJDES,
             LV_SUBJECT TYPE SO_OBJ_DES,
             LO_DOCUMENT TYPE REF TO CL_DOCUMENT_BCS,
             LO_MAIL TYPE REF TO CL_BCS,
             LS_CONTENTS LIKE LINE OF LT_CONTENTS,
             LV_FILENAME TYPE STRING,
             LV_RESULT TYPE OS_BOOLEAN,
            LV_STRING TYPE STRING.

  DATA LS_TVARVC TYPE TVARVC.


* subject
  LV_SUBJECT = 'Asset Audit : Data received from Assetrack'.

* mail body
  LS_CONTENTS-LINE = 'Hello,'.
  APPEND LS_CONTENTS TO LT_CONTENTS.
  LS_CONTENTS-LINE = 'Data received from Assetrak is empty'.
  APPEND LS_CONTENTS TO LT_CONTENTS.

* CREATE THE DOCUMENT WITH CONTENTS
  CREATE OBJECT LO_DOCUMENT.
  LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE       = 'HTM'
      I_SUBJECT    = LV_SUBJECT
      I_LENGTH     = '1000'
      I_LANGUAGE   = SY-LANGU
      I_IMPORTANCE = '1'
      I_TEXT       = LT_CONTENTS ).

* ATTACH THE FILE, THE ATTACHMENT TYPE SHOULD BE BIN TO ACCEPT ANY KIND OF ATTACHMENT,


*  PERFORM add_attachment USING 'ComapanyData.txt' gx_companydata CHANGING lo_document.
*  PERFORM add_attachment USING 'PlantData.txt' gx_plantdata CHANGING lo_document.
*  PERFORM add_attachment USING 'PlantStorageLocationData.txt' gx_plantstoragelocation CHANGING lo_document.
*  PERFORM add_attachment USING 'AssetClassData.txt' gx_assetclass CHANGING lo_document.
*  PERFORM add_attachment USING 'AssetData.txt' gx_assetdata CHANGING lo_document.

*gx_plantdata
*gx_plantstoragelocation
*gx_assetclass
*gx_assetdata
*


* CREATING PERSISTENT OBJECT WILL ALLOW YOU TO SET THE DOCUMENT IN THE MAIL
  LO_MAIL = CL_BCS=>CREATE_PERSISTENT( ).
  CALL METHOD LO_MAIL->SET_DOCUMENT( LO_DOCUMENT ).

  LOOP AT GT_TVARVC INTO LS_TVARVC WHERE NAME = 'ZASSETRAK_MAILID' .

*   EMAIL AS GIVEN IN THE TVARVC.
    GV_EMAIL = LS_TVARVC-LOW.

    GR_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( GV_EMAIL ).

*   Add recipient to send request
    CALL METHOD LO_MAIL->ADD_RECIPIENT
      EXPORTING
        I_RECIPIENT = GR_RECIPIENT
        I_EXPRESS   = 'X'.


  ENDLOOP.


* SEND THE MAIL
  CALL METHOD LO_MAIL->SEND(
    EXPORTING
      I_WITH_ERROR_SCREEN = 'X'
    RECEIVING
      RESULT              = LV_RESULT ).
* YOU CAN VERIFY THE STATUS IN THE LIST, YOU CAN ALSO SUBMIT THIS AS A BACKGROUND JOB.
  IF LV_RESULT = 'X'.
*    WRITE :/ , 1(9) lv_count, 10(55) file_table-pathname.
*    WRITE : 56(1) '-', 58(20) 'Mail sent'(003).
    COMMIT WORK.
  ELSE.
*    WRITE :/ , 1(9) lv_count, 10(55) file_table-pathname.
*    WRITE : 56(1) '-', 58(30) 'Error in sending Mail'(004).
    ROLLBACK WORK.
  ENDIF.
  REFRESH LT_CONTENTS[].




ENDFORM.                    "send_email_for_import
