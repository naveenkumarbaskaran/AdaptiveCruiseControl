*&---------------------------------------------------------------------*
*&  Include           ZASSETRAK_CONTROLLER_EVE
*&---------------------------------------------------------------------*
INITIALIZATION.
* get company msater maintained for Assetrak.
  DATA lr_company LIKE LINE OF gr_company.
  DATA ls_company TYPE ty_comp.

  IF gt_asset_company IS INITIAL.
    SELECT bukrs waers last_extract FROM zassetrak_compny INTO TABLE gt_asset_company.
    IF sy-subrc = 0.
      lr_company-sign = 'I'.
      lr_company-option = 'EQ'.
      LOOP AT gt_asset_company INTO ls_company.
        lr_company-low = ls_company-bukrs.
        APPEND lr_company TO gr_company.
      ENDLOOP.
    ELSE.
      MESSAGE 'Maintain company code in ZASSETRACK_COMPNY table' TYPE 'E'.
    ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_bukrs-low.
  PERFORM get_company_f4 CHANGING s_bukrs-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_bukrs-high.
  PERFORM get_company_f4 CHANGING s_bukrs-high.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'EXP' AND pr_imp EQ 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF screen-group1 = 'IMP' AND pr_exp EQ 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF pr_rev EQ 'X' AND ( screen-group1 = 'IMP' OR screen-group1 = 'EXP').
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF pr_arej EQ 'X' AND ( screen-group1 = 'IMP' OR screen-group1 = 'EXP').
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.
