*&---------------------------------------------------------------------*
*&  Include           ZASSETRAK_CONTROLLER_TOP
*&---------------------------------------------------------------------*
*TABLES:anlb,anla,anlc,anlz.
type-pools: truxs, abap ,slis.

constants: gc_n type char1  value  'N'.
constants: gc_p type char1  value  'P'.
constants: gc_r type char1  value  'R'.
constants: gc_e type char1  value  'E'.
constants: gc_x type char1  value  'X'.
constants: gc_s type char1  value  'S'.
constants: gc_y type char1  value  'Y'.
constants: gc_i type char1  value  'I'.

constants: gc_field_seperator value cl_abap_char_utilities=>horizontal_tab.

class cl_abap_char_utilities definition load.
*   TABLES : ZASSETRAK_CONFIG.

*FIELD-SYMBOLS <lf_tab> TYPE ANY TABLE.
*Internal table to upload data into
types: begin of ty_filerowdata ,
  row(5000) type c,
 end of ty_filerowdata.


types: begin of ty_xresponse,
      line(5000) type x,
      end of ty_xresponse.



types : begin of ty_fields..
        include structure sval.
types: end of ty_fields.
types tty_fields type standard table of ty_fields.

types: begin of ty_comp,
  bukrs	type bukrs,
  waers	type waers,
  last_extract  type timestampl,
  end of ty_comp.

*DATA gt_filerowdata TYPE STANDARD TABLE OF ty_filerowdata.
data gt_filerowdata type truxs_t_text_data.
data gt_return type standard table of bapiret2.

types : ty_stringtab type standard table of string.
types : ty_zassetrak_import_tab type standard table of zassetrak_import.
types:

 begin of ty_finalassets,
    bukrs type bukrs,
    anln1  type anln1,
    anln2	type anln2,
end of ty_finalassets.

types:
 begin of ty_assets,
    mandt type mandt,
    objectclas type cdobjectcl,
    bukrs type bukrs,
    anln1  type anln1,
    anln2	type anln2,
end of ty_assets.

types:
  begin of ty_companydata,
    bukrs type bukrs,
    name1 type name1,
    name2 type name2,
    street type ad_street,
    city1 type ad_city1,
    bezei type bezei20,
    post_code type ad_pstcd1,
    landx type landx,
    tel_number type ad_tlnmbr1,
    fax_number type ad_fxnmbr1,
    waers type waers,
  end of ty_companydata.

data gt_assetlist type standard table of zassetrak_import.
data gt_assetlist_alv type standard table of zassetrak_import_alv.

*types:
*  begin of ty_plantdata,
*    bukrs type bukrs,
*    werks type werks_d,
*    name1 type name1,
*    name2 type name2,
*    stras type stras,
*    pstlz type pstlz,
*    ort01 type ort01,
*    bezei type bezei20,
*    landx type landx,
*  end of ty_plantdata.

*new *changes
TYPES:
BEGIN OF ty_plantdata,
bukrs TYPE bukrs,
WERKS(14) TYPE C,
name1 TYPE name1,
name2 TYPE name2,
stras TYPE stras,
pstlz TYPE pstlz,
ort01 TYPE ort01,
bezei TYPE bezei20,
landx TYPE landx,
  werks1(4) TYPE C,
*plant_storage_code(12) type c,
*plant_storage_desc(50) type c,
END OF ty_plantdata.

types:
BEGIN OF ty_plantdata3,
bukrs TYPE bukrs,
werks1(4) TYPE C,
name1 TYPE name1,
name2 TYPE name2,
stras TYPE stras,
pstlz TYPE pstlz,
ort01 TYPE ort01,
bezei TYPE bezei20,
landx TYPE landx,

*plant_storage_code(12) type c,
*plant_storage_desc(50) type c,
END OF ty_plantdata3.

***

types:
  begin of ty_plantlocation,
    werks	type werks_d,
    stand	type stort_t499s,
    ktext	type text40,
  end of ty_plantlocation.

types:
  begin of ty_asset_classes,
    anlkl type anlkl,
    txk20	type txt20_ankt,
    txk50	type txt50_ankt,
  end of ty_asset_classes.

* for asset value extract
types:
 begin of ty_assetvalues_base,
   bukrs type bukrs,
   anln1  type anln1,
   anln2  type anln2,
   anlkl TYPE anlkl,
   kansw type anlc-kansw,
   knafa type anlc-knafa,
*{   INSERT         AESK906284                                        4
   kaafa type anlc-kaafa,                                       " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
   aafap type anlc-aafap,                                       " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
   nafag type anlc-nafag,
   nafav type anlc-nafav,
   nafal type anlc-nafal,
   wdv_date type zassetrak-wdv_date,
   answl type anlc-answl,
end of ty_assetvalues_base,

begin of ty_assetvalues,
    bukrs type bukrs,
    anln1  type anln1,
    anln2  type anln2,
    kansw type anlc-kansw,
    knafa type anlc-knafa,
    cal_value type anlc-nafag, " calculated value (ANLC-NAFAG+ ANLC-NAFAV+ ANLC-NAFAL)
    wdv_date type zassetrak-wdv_date,
    answl type anlc-answl,
end of ty_assetvalues.

types:
begin of ty_asset_rejected,
    imp_filename type zat_filename,
    bukrs type bukrs,
    anln1 type  anln1,
    anln2 type anln2,
    child_id type zat_counter ,
    processed_by type zat_processedby,
    processed_on type zat_processedon,
    pro_status type zat_prostatus,
    message type bapi_msg,
  end of ty_asset_rejected.

TYPES : BEGIN OF TY_T499S,
  WERKS(14) TYPE C,
  STAND TYPE AP_STAND,
  ktext type t499s-ktext,
  END OF TY_T499S.

  DATA : IT_T499S TYPE TABLE OF TY_T499S,
         WA_T499S TYPE TY_T499S.


data gv_date TYPE sy-datum.
data gv_bukrs type bukrs.
data gv_werks type werks_d.
data gv_afabe type afabe_d.
data gv_gjahr type gjahr.
data gv_file type char200.
data gt_asset_company type standard table of ty_comp.
data gr_company type range of bukrs.
data gt_plantdata type standard table of ty_plantdata.
data wa_plantdata TYPE ty_plantdata.
data gt_companydata type standard table of ty_companydata.
data gt_plantlocationdata type standard table of ty_plantlocation.
data gt_asset_classes type standard table of ty_asset_classes.
data gt_asset_classes1 type standard table of ty_asset_classes .
data gt_asset_classes2 type standard table of ty_asset_classes .
data gs_asset_class TYPE ty_asset_classes.
data gs_asset_class1 TYPE ty_asset_classes.

DATA gt_plantdata1 TYPE STANDARD TABLE OF ty_plantdata.
DATA gt_plantdata3 TYPE STANDARD TABLE OF ty_plantdata.
DATA WA_plantdata2 TYPE  ty_plantdata.
DATA WA_plantdata3 TYPE ty_plantdata.


*{   DELETE         AESK906120                                        2
*\DATA : IT_ZANKT TYPE STANDARD TABLE OF ZANKT,
*}   DELETE
*{   INSERT         AESK906120                                        1
    data : it_zankt type STANDARD TABLE OF ankt,
           wa_zankt type ankt.
*}   INSERT
*{   DELETE         AESK906120                                        3
*\      WA_ZANKT TYPE ZANKT.
*}   DELETE

data go_table type ref to data.
data gv_fieldlist type string.
data gv_importlist type string.
data gv_line1(4096) type c.
data gv_line2(4096) type c.

data gt_cdhdr    type standard table of cdhdr.
data gt_cdpos    type standard table of cdpos.

data gv_path type string.
data gv_datetime type string.

data: gv_indxkey type indx-srtfd.
data gv_extractdatetime type char14.
data gv_extractdate type sy-datum.

data gt_newasset type standard table of ty_assets.
data gt_changedasset type standard table of ty_assets.
data gt_config type standard table of zassetrak_config.
data gt_export type standard table of zassetrak_config.

data gt_finalasset type standard table of ty_finalassets.

data gt_assetvalies type standard table of ty_assetvalues.

data gt_asset_rejected type standard table of ty_asset_rejected.

data gt_tvarvc type standard table of tvarvc.
data gt_tcodenew type range of sy-tcode.
data gt_tcodechange type range of sy-tcode.
data gt_tcodeall type range of sy-tcode.

* real depriciation area.
data gt_t093d type standard table of t093d.

field-symbols: <gf_finaltab>  type standard table.
field-symbols: <lf_tab>  type standard table.


data gx_companydata type xstring.
data gx_plantdata type xstring.
data gx_plantstoragelocation type xstring.
data gx_assetclass type xstring.
data gx_assetdata type xstring.

* declarations for alv and internal tables
*----------------------------------------------------------------------*
data: gt_fieldcat type lvc_t_fcat,
      gs_variant type disvariant,
      gs_layout type lvc_s_layo,
      gt_eventexits     type  slis_t_event_exit,
      gs_eventexit      type  slis_event_exit.

*declarations for testing internal table-subrat
*
***----------------------------------------------------
**
types: begin of ty_anla1,
              bukrs type anla-bukrs,
              anln1 type anla-anln1,
              anln2 type anla-anln2,
              anlkl type anla-anlkl,
              anlar type anla-anlar,
              ernam type anla-ernam,
              erdat type anla-erdat,
              aktiv type anla-aktiv,
              deakt type anla-deakt,
              liefe type anla-liefe,
              aibn1 type anla-aibn1,
              meins type anla-meins,
              menge type anla-menge,
              inken type anla-inken,
              ivdat type anla-ivdat,
              invzu type anla-invzu,
              invnr type anla-invnr,
              txt50 type anla-txt50,
              txa50 type anla-txa50,
              sernr type anla-sernr,
      end of ty_anla1.
data: it_anla1 type table of ty_anla1,
      it_anla3 type TABLE OF ty_anla1,
      wa_anla1 type ty_anla1.
*
TYPES: BEGIN OF ty_anlz1,
         bukrs TYPE anlz-bukrs,
         anln1 TYPE anlz-anln1,
         anln2 TYPE anlz-anln2,
         adatu TYPE anlz-adatu,   " Fetch newly trasnferred assets based on date, Changes made on 9th Jan 2019.
         kostl TYPE anlz-kostl,
         werks TYPE anlz-werks,
         stort TYPE anlz-stort,
         raumn TYPE anlz-raumn,
       END OF ty_anlz1.

data : it_anlz1 type table of ty_anlz1,
       wa_anlz1 type ty_anlz1.
*
types: begin of ty_anlu1,
        bukrs type anlu-bukrs,
        anln1 type anlu-anln1,
        anln2 type anlu-anln2,
        zzbuilding type anlu-zzbuilding,
        zzuser_name type anlu-zzuser_name,
        zzuser_email type anlu-zzuser_email,
        zzinventory_by type anlu-zzinventory_by,
        zznote type anlu-zznote,
        zzresi_value type anlu-zzresi_value,
        zzresi_date type anlu-zzresi_date,
        zzprevs_remark type anlu-zzprevs_remark,
        zzverified_at type anlu-zzverified_at,
        zzinvmode type anlu-zzinvmode,
        zzinv_comment type anlu-zzinv_comment,
        zzretire_ind type anlu-zzretire_ind,
      end of ty_anlu1.
*
data : it_anlu1 type table of ty_anlu1,
       wa_anlu1 type ty_anlu1.
*
types: begin of ty_anlc1,
        bukrs type anlc-bukrs,
        anln1 type anlc-anln1,
        anln2 type anlc-anln2,
        gjahr type anlc-gjahr,
        afabe type anlc-afabe,
        kansw type anlc-kansw,
        knafa type anlc-knafa,
*{   INSERT         AESK906284                                        5
        kaafa type anlc-kaafa,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
        aafap type anlc-aafap,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
        nafag type anlc-nafag,
        answl type anlc-answl,
      end of ty_anlc1.
*
data : it_anlc1 type table of ty_anlc1,
      wa_anlc1 type ty_anlc1.
*
types: begin of ty_anlb1,
        bukrs type anlb-bukrs,
        anln1 type anlb-anln1,
        anln2 type anlb-anln2,
        ndjar type anlb-ndjar,
      end of ty_anlb1.

data : it_anlb1 type table of ty_anlb1,
       wa_anlb1 type ty_anlb1.

types: BEGIN OF ty_csks,
  kostl type kostl,
  bukrs type bukrs,
  prctr type prctr,
  END OF ty_csks.

  types: BEGIN OF ty_cskt,
  kostl type kostl,
  ltext type kltxt,
  END OF ty_cskT.

TYPES : BEGIN OF TY_CEPC,
  PRCTR TYPE PRCTR,
  SEGMENT TYPE CEPC-SEGMENT,
  END OF TY_CEPC.
  data : it_csks TYPE TABLE OF ty_csks,
         wa_csks type ty_csks,
         it_cskt type TABLE OF TY_CSKT,
         WA_CSKT TYPE TY_CSKT,
         IT_CEPC TYPE TABLE OF TY_CEPC,
         WA_CEPC TYPE TY_CEPC.
*
types : begin of ty_final,

              bukrs type anla-bukrs,
              anln1 type anla-anln1,
              anln2 type anla-anln2,
              invnr type anla-invnr,
              aktiv type anla-aktiv,
              txa50 type anla-txa50,
              menge type anla-menge,
              stort type anlz-stort,
              txt50 type anla-txt50,
              zzbuilding type anlu-zzbuilding,
              raumn type anlz-raumn,
              inken type anla-inken,
              invzu type anla-invzu,
              anlkl type anla-anlkl,
              liefe type anla-liefe,
              zzuser_name type anlu-zzuser_name,
              zzuser_email type anlu-zzuser_email,
              kansw type anlc-kansw,
              knafa type anlc-knafa,
*{   INSERT         AESK906284                                        6
*              aafap type anlc-aafap,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
              nafag type anlc-nafag,
              ivdat type anla-ivdat,
              zzinventory_by type anlu-zzinventory_by,
              zznote type anlu-zznote,
              deakt type anla-deakt,
              werks(14) type c , "anlz-werks,
              sernr type anla-sernr,
              zzresi_value type anlu-zzresi_value,
              zzresi_date type anlu-zzresi_date,
              zzprevs_remark type anlu-zzprevs_remark,
              zzverified_at type anlu-zzverified_at,
              zzinvmode type anlu-zzinvmode,
              zzinv_comment type anlu-zzinv_comment,

              WDV_DATE TYPE ZASSETRAK-WDV_DATE,               "Added
              aibn1 type anla-aibn1,
              zzretire_ind type anlu-zzretire_ind,
              gjahr type anlc-gjahr,
              afabe type anlc-afabe,
              meins type anla-meins,
              answl type anlc-answl,
              RETIRE_FLAG TYPE ZASSETRAK-RETIRE_FLAG,                        "added
              REVIEW_FLAG TYPE ZASSETRAK-REVIEW_FLAG,
              PARENT_FLAG TYPE ZASSETRAK-PARENT_FLAG,
              CHILD_ID TYPE ZASSETRAK-CHILD_ID,
              WDV_AMOUNT TYPE ZASSETRAK-WDV_AMOUNT,
              AT_AQC TYPE ZASSETRAK-AT_AQC,
              AT_QUANTITY TYPE ZASSETRAK-AT_QUANTITY,
              AT_UNIT TYPE ZASSETRAK-AT_UNIT,
              AT_ADDL2 TYPE ZASSETRAK-AT_ADDL2,
              SPLIT_FLAG TYPE ZASSETRAK-SPLIT_FLAG,
              CHANGE_FLAG TYPE ZASSETRAK-CHANGE_FLAG,
              ndjar type anlb-ndjar,
*{   INSERT         AESK906284                                        8
              kaafa type anlc-kaafa,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
              aafap type anlc-aafap,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
              txk20 TYPE ankt-txk20,
  end of ty_final.
data : it_final type table of ty_final,
       it_final1 type table of ty_final,
       wa_final type ty_final,
       wa_final1 type ty_final.

types : begin of ty_final1,

              bukrs type anla-bukrs,
              anln1 type anla-anln1,
              anln2 type anla-anln2,
              invnr type anla-invnr,
              aktiv type anla-aktiv,
              txa50 type anla-txa50,
              menge type anla-menge,
              stort type anlz-stort,
              txt50 type anla-txt50,
              zzbuilding type anlu-zzbuilding,
              raumn type anlz-raumn,
              inken type anla-inken,
              invzu type anla-invzu,
              anlkl type anla-anlkl,
              liefe type anla-liefe,
              zzuser_name type anlu-zzuser_name,
              zzuser_email type anlu-zzuser_email,
              kansw type anlc-kansw,
              knafa type anlc-knafa,
*{   INSERT         AESK906284                                        7
*              aafap type anlc-aafap,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
              nafag type anlc-nafag,
              ivdat type anla-ivdat,
              zzinventory_by type anlu-zzinventory_by,
              zznote type anlu-zznote,
              deakt type anla-deakt,
              werks(14) type c ," anlz-werks,
              sernr type anla-sernr,
              zzresi_value type anlu-zzresi_value,
              zzresi_date type anlu-zzresi_date,
              zzprevs_remark type anlu-zzprevs_remark,
              zzverified_at type anlu-zzverified_at,
              zzinvmode type anlu-zzinvmode,
              zzinv_comment type anlu-zzinv_comment,

              WDV_DATE TYPE ZASSETRAK-WDV_DATE,               "Added
              aibn1 type anla-aibn1,
              zzretire_ind type anlu-zzretire_ind,
              gjahr type anlc-gjahr,
              afabe type anlc-afabe,
              meins type anla-meins,
              answl type anlc-answl,
              RETIRE_FLAG TYPE ZASSETRAK-RETIRE_FLAG,                        "added
              REVIEW_FLAG TYPE ZASSETRAK-REVIEW_FLAG,
              PARENT_FLAG TYPE ZASSETRAK-PARENT_FLAG,
              CHILD_ID TYPE ZASSETRAK-CHILD_ID,
              WDV_AMOUNT TYPE ZASSETRAK-WDV_AMOUNT,
              AT_AQC TYPE ZASSETRAK-AT_AQC,
              AT_QUANTITY TYPE ZASSETRAK-AT_QUANTITY,
              AT_UNIT TYPE ZASSETRAK-AT_UNIT,
              AT_ADDL2 TYPE ZASSETRAK-AT_ADDL2,
              SPLIT_FLAG TYPE ZASSETRAK-SPLIT_FLAG,
              CHANGE_FLAG TYPE ZASSETRAK-CHANGE_FLAG,
              ndjar type anlb-ndjar,
*{   INSERT         AESK906284                                        9
              kaafa type anlc-kaafa,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
              aafap type anlc-aafap,                                        " Cumulative Unplanned depreciation " Added by Ashish.P on 13.08.18
*}   INSERT
*              txk20 TYPE ankt-txk20,
  end of ty_final1.

  data : lt_final TYPE TABLE OF ty_final1,
         ls_final TYPE ty_final1.

 data: w_curry TYPE BKPF-GJAHR,
       w_currm TYPE BKPF-MONAT,
       w_prevm TYPE BKPF-MONAT,
       w_prevy TYPE BKPF-GJAHR.

 call function 'GET_CURRENT_YEAR'
  EXPORTING
    BUKRS         = '1000'
    DATE          = SY-DATUM
  IMPORTING
    CURRM         = w_currm
    CURRY         = w_curry
    PREVM         = w_prevm
    PREVY         = w_prevy
           .

 data : it_anla2 TYPE TABLE OF anla,
        wa_anla2 TYPE anla.
