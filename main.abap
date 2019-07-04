*&---------------------------------------------------------------------*
*& Report  ZASSETRAK_CONTROLLER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zassetrak_controller.

*TABLES : zassetrak_config.
* top include
INCLUDE zassetrak_controller_top.

* selection screen include
INCLUDE zassetrak_controller_sel.

* Event include
INCLUDE zassetrak_controller_eve.


*subroutines for testing
INCLUDE zassetrak_controller_sub.

* start of selection
START-OF-SELECTION.

* validations
  PERFORM validation.

* read TVARVC
  PERFORM read_tvarvc.

* build output table based on configuration
  PERFORM build_output_table CHANGING go_table gv_fieldlist gv_importlist  gv_line1 gv_line2.
*  * download master / config / asset data
  IF pr_exp = gc_x.

*   extract master data
    IF p_mdata = gc_x.

*     extract Asset related master data
      PERFORM extract_master_data.

    ENDIF.

*   extract Asset Data
    IF p_adata = gc_x.

*     extract Asset data
      PERFORM select_asset_data.

    ENDIF.

*   send email
*    PERFORM send_email.

* import file
  ELSEIF pr_imp = gc_x.

*   import files
    PERFORM import_data.

* review records
  ELSEIF pr_rev = gc_x.

*   select review records ( status = N or E)
    SELECT * INTO TABLE gt_assetlist FROM zassetrak_import WHERE ( pro_status = 'N' OR pro_status = 'E').
    SORT gt_assetlist BY imp_filename DESCENDING .
    PERFORM call_alv.

* download rejected asset changes
  ELSEIF pr_arej = gc_x.

*   send rejected asset data details
    PERFORM send_rejected_assets.

  ENDIF.
