REPORT zitetr_fat2eirsal_olustur.

INCLUDE zitetr_fat2eirsal_olustur_top.
INCLUDE zitetr_fat2eirsal_olustur_sel.
INCLUDE zitetr_fat2eirsal_olustur_cls.
INCLUDE zitetr_fat2eirsal_olustur_clsi.
INCLUDE zitetr_fat2eirsal_olustur_alv.
INCLUDE zitetr_fat2eirsal_olustur_for.
*-----------------------------------------------------------------------
* TOP Include - Global Tipler & Veriler (ousqt / ousrs kaldırıldı)
*-----------------------------------------------------------------------

TYPE-POOLS: abap.
TABLES: vbrk.
TYPES ty_rule_in  TYPE /itetr/dlv_s_edrules_in.
TYPES ty_rule_out TYPE /itetr/dlv_s_edrules_out.

TYPES: BEGIN OF ty_result,
         vbeln   TYPE vbrk-vbeln,
         status  TYPE char1,
         message TYPE string,
         belnr   TYPE belnr_d,
         docui   TYPE /itetr/com_e_docui,
       END OF ty_result.
TYPES ty_t_result TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

TYPES ty_t_vbrp TYPE STANDARD TABLE OF vbrp WITH DEFAULT KEY.

TYPES: BEGIN OF ty_item_wrk,
         selii TYPE matnr,
         mdesc TYPE char40,
         descr TYPE char70,
         netpr TYPE netpr,
         waers TYPE waers,
         menge TYPE vbrp-fkimg,
         meins TYPE meins,
       END OF ty_item_wrk.
TYPES ty_t_item_wrk TYPE STANDARD TABLE OF ty_item_wrk WITH DEFAULT KEY.

CONSTANTS:
  gc_status_success TYPE char1 VALUE 'S',
  gc_status_error   TYPE char1 VALUE 'E',
  gc_status_skip    TYPE char1 VALUE 'I'.

DATA: gt_results TYPE ty_t_result,
      gs_result  TYPE ty_result,
      gs_head    TYPE /itetr/dlv_ogdlv.
*-----------------------------------------------------------------------
* Selection Screen (Çerçeveli) - p_mode listbox
*-----------------------------------------------------------------------
TYPE-POOLS: vrm.              " VRM_SET_VALUES tipleri


* Blok 1: Fatura + Şirket
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS    p_bukrs TYPE bukrs OBLIGATORY.
SELECT-OPTIONS s_vbeln FOR vbrk-vbeln OBLIGATORY.
SELECT-OPTIONS s_fkdat FOR vbrk-fkdat.
SELECTION-SCREEN END OF BLOCK b1.

* Blok 2: Ayarlar
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
PARAMETERS:
  p_bal   TYPE boole_d AS CHECKBOX DEFAULT abap_false,
  p_emal  TYPE boole_d AS CHECKBOX DEFAULT abap_false,
  p_mode  TYPE c LENGTH 1 AS LISTBOX VISIBLE LENGTH 18.   " L=Canlı, T=Test
SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
  IF p_mode IS INITIAL.
    p_mode = 'L'.
  ENDIF.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN OUTPUT
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_mode_list.
  LOOP AT SCREEN.
    IF screen-name = 'P_EMAL'.
      IF p_mode = 'T'.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN ON p_mode
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON p_mode.
  IF p_mode = 'T'.
    p_emal = abap_false.
  ENDIF.

*-----------------------------------------------------------------------
* FORM fill_mode_list
*-----------------------------------------------------------------------
FORM fill_mode_list.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  CLEAR lt_values.

  ls_value-key  = 'L'.
  ls_value-text = 'Canlı (Kayıt)'.
  APPEND ls_value TO lt_values.

  ls_value-key  = 'T'.
  ls_value-text = 'Test (Simülasyon)'.
  APPEND ls_value TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_MODE'
      values = lt_values.
ENDFORM.
*-----------------------------------------------------------------------
* Local Class Definition
*-----------------------------------------------------------------------
CLASS lcl_bill2dlv DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_test TYPE abap_bool,
      run
        IMPORTING iv_vbeln         TYPE vbrk-vbeln
        RETURNING VALUE(rs_result) TYPE ty_result,
      get_billing_header
        IMPORTING iv_vbeln     TYPE vbrk-vbeln
        EXPORTING es_vbrk      TYPE vbrk
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      get_billing_items
        IMPORTING iv_vbeln     TYPE vbrk-vbeln
        EXPORTING et_vbrp      TYPE ty_t_vbrp
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      build_rule_input
        IMPORTING is_vbrk      TYPE vbrk
                  it_vbrp      TYPE ty_t_vbrp
        RETURNING VALUE(rs_in) TYPE ty_rule_in,
      get_edelivery_rule
        IMPORTING iv_rule_type          TYPE char1
                  is_rule_input         TYPE ty_rule_in
        RETURNING VALUE(rs_rule_output) TYPE ty_rule_out,
      apply_primary_defaults
        CHANGING cs_out TYPE ty_rule_out,
      duplicate_exists
        IMPORTING iv_vbeln         TYPE vbrk-vbeln
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      fill_header
        IMPORTING is_vbrk TYPE vbrk
                  is_rule TYPE ty_rule_out,
      fill_items
        IMPORTING it_vbrp TYPE ty_t_vbrp
                  is_vbrk TYPE vbrk,
      assign_number
        CHANGING  cs_head      TYPE /itetr/dlv_ogdlv
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      persist
        RETURNING VALUE(rv_ok) TYPE abap_bool.

  PRIVATE SECTION.
    DATA mv_test         TYPE abap_bool.
    DATA mv_company_code TYPE bukrs.
    DATA gs_head         TYPE /itetr/dlv_ogdlv.
    DATA gt_items        TYPE ty_t_item_wrk.

    DATA gs_rule_in TYPE ty_rule_in.
    DATA gs_rule_p  TYPE ty_rule_out.
    DATA gs_rule_s  TYPE ty_rule_out.
    DATA gs_rule_x  TYPE ty_rule_out.
ENDCLASS.
*-----------------------------------------------------------------------
* Class Implementation (ousqt / ousrs alanlarıyla)
*-----------------------------------------------------------------------
CLASS lcl_bill2dlv IMPLEMENTATION.

  METHOD constructor.
    mv_test = iv_test.
  ENDMETHOD.

  METHOD run.
    DATA ls_vbrk TYPE vbrk.
    DATA lt_vbrp TYPE ty_t_vbrp.
    DATA lv_docui_text TYPE c LENGTH 64.

    rs_result-vbeln = iv_vbeln.

    IF me->get_billing_header( EXPORTING iv_vbeln = iv_vbeln IMPORTING es_vbrk = ls_vbrk ) = abap_false.
      rs_result-status  = gc_status_error.
      rs_result-message = 'Fatura bulunamadı'.
      RETURN.
    ENDIF.

    IF ls_vbrk-bukrs <> p_bukrs.
      rs_result-status  = gc_status_skip.
      rs_result-message = 'Şirket kodu uyuşmuyor'.
      RETURN.
    ENDIF.

    mv_company_code = ls_vbrk-bukrs.

    IF me->get_billing_items( EXPORTING iv_vbeln = iv_vbeln IMPORTING et_vbrp = lt_vbrp ) = abap_false
       OR lt_vbrp IS INITIAL.
      rs_result-status  = gc_status_error.
      rs_result-message = 'Kalem bulunamadı'.
      RETURN.
    ENDIF.

    IF me->duplicate_exists( iv_vbeln = iv_vbeln ) = abap_true.
      rs_result-status  = gc_status_skip.
      rs_result-message = 'Duplicate - önceden mevcut'.
      RETURN.
    ENDIF.

    CLEAR: gs_rule_in, gs_rule_p, gs_rule_s, gs_rule_x.

    gs_rule_in = me->build_rule_input( is_vbrk = ls_vbrk it_vbrp = lt_vbrp ).

    gs_rule_p = me->get_edelivery_rule( iv_rule_type = 'P' is_rule_input = gs_rule_in ).
    IF gs_rule_p IS INITIAL OR gs_rule_p-excld = 'X'.
      rs_result-status  = gc_status_skip.
      rs_result-message = 'Primary rule yok veya exclude'.
      RETURN.
    ENDIF.
    me->apply_primary_defaults( CHANGING cs_out = gs_rule_p ).

    gs_rule_s = me->get_edelivery_rule( iv_rule_type = 'S' is_rule_input = gs_rule_in ).
    IF gs_rule_s-serpr IS NOT INITIAL.
      gs_rule_p-serpr = gs_rule_s-serpr.
    ENDIF.

    gs_rule_x = me->get_edelivery_rule( iv_rule_type = 'X' is_rule_input = gs_rule_in ).
    IF gs_rule_x-xsltt IS NOT INITIAL.
      gs_rule_p-xsltt = gs_rule_x-xsltt.
    ENDIF.

    me->fill_header(
      is_vbrk = ls_vbrk
      is_rule = gs_rule_p ).

    me->fill_items(
      it_vbrp = lt_vbrp
      is_vbrk = ls_vbrk ).
    IF gt_items IS INITIAL.
      rs_result-status  = gc_status_error.
      rs_result-message = 'Kalem üretilemedi'.
      RETURN.
    ENDIF.

    IF mv_test = abap_true.
      rs_result-status  = gc_status_skip.
      rs_result-message = 'TEST modu - yazılmadı'.
      RETURN.
    ENDIF.

    IF me->assign_number( CHANGING cs_head = gs_head ) = abap_false.
      rs_result-status  = gc_status_error.
      rs_result-message = 'Numara alınamadı'.
      RETURN.
    ENDIF.

    IF me->persist( ) = abap_false.
      rs_result-status  = gc_status_error.
      rs_result-message = 'Kayıt hatası'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK AND WAIT.
    rs_result-status = gc_status_success.

    CLEAR lv_docui_text.
    WRITE gs_head-docui TO lv_docui_text.
    CONCATENATE 'Oluşturuldu BELNR:' gs_head-belnr 'DOCUI:' lv_docui_text
      INTO rs_result-message SEPARATED BY space.

    rs_result-docui = gs_head-docui.
    rs_result-belnr = gs_head-belnr.
  ENDMETHOD.

  METHOD get_billing_header.
    SELECT SINGLE * FROM vbrk INTO es_vbrk WHERE vbeln = iv_vbeln.
    rv_ok = abap_true.
    IF sy-subrc <> 0.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_billing_items.
    SELECT * FROM vbrp INTO TABLE et_vbrp WHERE vbeln = iv_vbeln.
    rv_ok = abap_true.
    IF sy-subrc <> 0.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD build_rule_input.
    DATA ls_vbrp TYPE vbrp.
    CLEAR rs_in.
    rs_in-agent = ''.          " Varsayılan boş
    rs_in-awtyp = 'MANU'.
    rs_in-vkorg = is_vbrk-vkorg.
    rs_in-vtweg = is_vbrk-vtweg.
    rs_in-sddty = is_vbrk-fkart.
    rs_in-kunnr = is_vbrk-kunrg.
    IF it_vbrp IS NOT INITIAL.
      READ TABLE it_vbrp INTO ls_vbrp INDEX 1.
      IF sy-subrc = 0.
        rs_in-werks = ls_vbrp-werks.
        rs_in-lgort = ls_vbrp-lgort.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_edelivery_rule.
    DATA: lt_agent TYPE RANGE OF /itetr/com_e_agent,
          lt_awtyp TYPE RANGE OF /itetr/dlv_e_awtyp,
          lt_vkorg TYPE RANGE OF vkorg,
          lt_vtweg TYPE RANGE OF vtweg,
          lt_werks TYPE RANGE OF werks_d,
          lt_lgort TYPE RANGE OF lgort_d,
          lt_umwrk TYPE RANGE OF werks_d,
          lt_umlgo TYPE RANGE OF lgort_d,
          lt_sobkz TYPE RANGE OF sobkz,
          lt_bwart TYPE RANGE OF bwart,
          lt_pidin TYPE RANGE OF /itetr/dlv_e_prfid,
          lt_dtyin TYPE RANGE OF /itetr/dlv_e_dlvty,
          lt_sddty TYPE RANGE OF fkart,
          lt_mmdty TYPE RANGE OF blart,
          lt_fidty TYPE RANGE OF blartfi,
          lt_kunnr TYPE RANGE OF kunnr,
          lt_lifnr TYPE RANGE OF lifnr,
          ls_range TYPE rsis_s_range,
          ls_rule  TYPE /itetr/dlv_edru.

    ls_range-sign   = 'I'.
    ls_range-option = 'EQ'.

    " Her alan için boş + değer
    DEFINE add_range.
      ls_range-low = ''.        APPEND ls_range TO &1.
      ls_range-low = &2.        APPEND ls_range TO &1.
    END-OF-DEFINITION.

    add_range lt_agent  gs_rule_in-agent.
    add_range lt_awtyp  gs_rule_in-awtyp.
    add_range lt_vkorg  gs_rule_in-vkorg.
    add_range lt_vtweg  gs_rule_in-vtweg.
    add_range lt_werks  gs_rule_in-werks.
    add_range lt_lgort  gs_rule_in-lgort.
    add_range lt_umwrk  gs_rule_in-umwrk.
    add_range lt_umlgo  gs_rule_in-umlgo.
    add_range lt_sobkz  gs_rule_in-sobkz.
    add_range lt_bwart  gs_rule_in-bwart.
    add_range lt_dtyin  gs_rule_in-dtyin.
    add_range lt_sddty  gs_rule_in-sddty.
    add_range lt_mmdty  gs_rule_in-mmdty.
    add_range lt_fidty  gs_rule_in-fidty.
    add_range lt_pidin  gs_rule_in-pidin.
    add_range lt_kunnr  gs_rule_in-kunnr.
    add_range lt_lifnr  gs_rule_in-lifnr.

    SELECT *
      UP TO 1 ROWS
      FROM /itetr/dlv_edru
      INTO ls_rule
      WHERE bukrs = mv_company_code
        AND rulet = iv_rule_type
        AND agent IN lt_agent
        AND awtyp IN lt_awtyp
        AND vkorg IN lt_vkorg
        AND vtweg IN lt_vtweg
        AND werks IN lt_werks
        AND lgort IN lt_lgort
        AND umwrk IN lt_umwrk
        AND umlgo IN lt_umlgo
        AND sobkz IN lt_sobkz
        AND bwart IN lt_bwart
        AND pidin IN lt_pidin
        AND dtyin IN lt_dtyin
        AND sddty IN lt_sddty
        AND mmdty IN lt_mmdty
        AND fidty IN lt_fidty
        AND kunnr IN lt_kunnr
        AND lifnr IN lt_lifnr
      ORDER BY agent DESCENDING awtyp DESCENDING vkorg DESCENDING vtweg DESCENDING
               werks DESCENDING lgort DESCENDING umwrk DESCENDING umlgo DESCENDING
               sobkz DESCENDING bwart DESCENDING pidin DESCENDING dtyin DESCENDING
               sddty DESCENDING mmdty DESCENDING fidty DESCENDING
               kunnr DESCENDING lifnr DESCENDING.
    ENDSELECT.

    IF sy-subrc = 0 AND ls_rule IS NOT INITIAL.
      MOVE-CORRESPONDING ls_rule TO rs_rule_output.
    ENDIF.

    IF iv_rule_type = 'P'.
      me->apply_primary_defaults( CHANGING cs_out = rs_rule_output ).
    ENDIF.
  ENDMETHOD.

  METHOD apply_primary_defaults.
    IF cs_out-pidou IS INITIAL.
      cs_out-pidou = 'TEMEL'.
    ENDIF.
    IF cs_out-dtyou IS INITIAL.
      cs_out-dtyou = 'SEVK'.
    ENDIF.
  ENDMETHOD.

  METHOD duplicate_exists.
    DATA lv_dummy TYPE /itetr/com_e_docui.
    rv_exists = abap_false.
    SELECT SINGLE docui
      FROM /itetr/dlv_ogdlv
      INTO lv_dummy
      WHERE awtyp    = 'MANU'
        AND le_vbeln = iv_vbeln.
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD fill_header.
    CLEAR gs_head.
    gs_head-bukrs    = is_vbrk-bukrs.
    gs_head-bldat    = is_vbrk-fkdat.
    gs_head-gjahr    = is_vbrk-fkdat(4).
    gs_head-kunnr    = is_vbrk-kunrg.
    gs_head-awtyp    = 'MANU'.
    gs_head-objtype  = 'BIL'.
    gs_head-le_vbeln = is_vbrk-vbeln.
    gs_head-prfid    = is_rule-pidou.
    gs_head-dlvty    = is_rule-dtyou.
    gs_head-serpr    = is_rule-serpr.
    gs_head-xsltt    = is_rule-xsltt.
    gs_head-summary  = is_rule-summary.
    gs_head-stock_transfer = is_rule-stock_transfer.
    gs_head-aprvd    = 'X'.
    gs_head-ernam    = sy-uname.
    gs_head-erdat    = sy-datum.
    gs_head-erzet    = sy-uzeit.

    SELECT SINGLE stcd1 FROM kna1 INTO gs_head-taxid WHERE kunnr = gs_head-kunnr.

    TRY.
        IF gs_head-docui IS INITIAL.
          gs_head-docui = /itetr/cl_regulative_common=>generate_document_uuid_x16( ).
        ENDIF.
        IF gs_head-dlvui IS INITIAL.
          gs_head-dlvui = /itetr/cl_regulative_common=>generate_document_uuid_c36( ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD fill_items.
    DATA ls_vbrp TYPE vbrp.
    DATA lv_price TYPE netpr.
    FIELD-SYMBOLS <ls_i> TYPE ty_item_wrk.
    REFRESH gt_items.

    LOOP AT it_vbrp INTO ls_vbrp.
      CLEAR lv_price.
      IF ls_vbrp-fkimg IS NOT INITIAL AND ls_vbrp-netwr IS NOT INITIAL.
        lv_price = ls_vbrp-netwr / ls_vbrp-fkimg.
      ENDIF.
      APPEND INITIAL LINE TO gt_items ASSIGNING <ls_i>.
      <ls_i>-selii = ls_vbrp-matnr.
      <ls_i>-mdesc = ls_vbrp-arktx.
      <ls_i>-descr = ls_vbrp-arktx.
      <ls_i>-netpr = lv_price.
      <ls_i>-waers = is_vbrk-waerk.
      <ls_i>-menge = ls_vbrp-fkimg.
      <ls_i>-meins = ls_vbrp-vrkme.
    ENDLOOP.
  ENDMETHOD.

  METHOD assign_number.
    rv_ok = abap_true.
    IF cs_head-belnr IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = '/ITETR/MDL'
        IMPORTING
          number      = cs_head-belnr
        EXCEPTIONS
          OTHERS      = 1.
      IF sy-subrc <> 0 OR cs_head-belnr IS INITIAL.
        rv_ok = abap_false.
        RETURN.
      ENDIF.
      cs_head-gjahr = cs_head-bldat(4).
    ENDIF.
  ENDMETHOD.

  METHOD persist.
    DATA lv_linno    TYPE /itetr/com_e_linno.
    DATA ls_wrk      TYPE ty_item_wrk.
    DATA ls_db_item  TYPE /itetr/dlv_ogdli.

    DATA lt_db_item  TYPE STANDARD TABLE OF /itetr/dlv_ogdli WITH EMPTY KEY.

    rv_ok = abap_true.

    " Header tek seferde
    INSERT /itetr/dlv_ogdlv FROM gs_head.
    IF sy-subrc <> 0.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Item'ları topla
    lv_linno = 0.
    LOOP AT gt_items INTO ls_wrk.
      lv_linno = lv_linno + 1.
      CLEAR ls_db_item.

      ls_db_item-mandt = sy-mandt.
      ls_db_item-docui = gs_head-docui.
      ls_db_item-linno = lv_linno.
      ls_db_item-selii = ls_wrk-selii.
      ls_db_item-mdesc = ls_wrk-mdesc.
      ls_db_item-descr = ls_wrk-descr.
      ls_db_item-netpr = ls_wrk-netpr.
      ls_db_item-waers = ls_wrk-waers.
      ls_db_item-menge = ls_wrk-menge.
      ls_db_item-meins = ls_wrk-meins.

      APPEND ls_db_item TO lt_db_item.
    ENDLOOP.

    INSERT /itetr/dlv_ogdli FROM TABLE lt_db_item.
    IF sy-subrc <> 0.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
*-----------------------------------------------------------------------
* ALV & Application Log Helpers  (Generic column handling - retro fit)
*-----------------------------------------------------------------------

FORM display_alv.
  DATA lo_alv  TYPE REF TO cl_salv_table.
  DATA lo_cols TYPE REF TO object. " generic container

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_results ).

      lo_cols = lo_alv->get_columns( ).

      PERFORM optimize_columns USING lo_cols.

      PERFORM set_col_text USING lo_cols 'VBELN'   'Fatura'.
      PERFORM set_col_text USING lo_cols 'STATUS'  'Durum'.
      PERFORM set_col_text USING lo_cols 'BELNR'   'Belge No'.
      PERFORM set_col_text USING lo_cols 'DOCUI'   'DocUI'.
      PERFORM set_col_text USING lo_cols 'MESSAGE' 'Mesaj'.

      lo_alv->get_display_settings( )->set_list_header(
        'e-İrsaliye Oluşturma Sonuçları' ).

      lo_alv->display( ).
    CATCH cx_salv_msg.
      MESSAGE 'ALV gösterim hatası, basit liste gösteriliyor.' TYPE 'I'.
      PERFORM fallback_list.
  ENDTRY.
ENDFORM.

*-----------------------------------------------------------------------
* Kolon Optimize (generic, eski sürümlerle uyumlu)
*-----------------------------------------------------------------------
FORM optimize_columns USING pr_cols TYPE REF TO object.
  DATA lo_cols_tab TYPE REF TO cl_salv_columns_table.
  DATA lo_cols_gen TYPE REF TO cl_salv_columns.

  " Önce tablo kolon tipi
  TRY.
      lo_cols_tab ?= pr_cols.
      IF lo_cols_tab IS BOUND.
        lo_cols_tab->set_optimize( abap_true ).
        RETURN.
      ENDIF.
    CATCH cx_sy_move_cast_error.
  ENDTRY.

  " Sonra genel kolon tipi
  TRY.
      lo_cols_gen ?= pr_cols.
      IF lo_cols_gen IS BOUND.
        " Bazı versiyonlarda set_optimize olmayabilir; bu yüzden CALL METHOD ... turunda
        CALL METHOD lo_cols_gen->('SET_OPTIMIZE')  " dinamizm, yoksa dump olmaz
          EXPORTING
            value = abap_true.
      ENDIF.
    CATCH cx_sy_move_cast_error cx_root.
  ENDTRY.
ENDFORM.

*-----------------------------------------------------------------------
* Kolon Başlık Metni Set (generic)
*-----------------------------------------------------------------------
FORM set_col_text USING pr_cols TYPE REF TO object
                         pv_col  TYPE lvc_fname
                         pv_text TYPE string.
  DATA: lv_short  TYPE scrtext_s,
        lv_medium TYPE scrtext_m,
        lv_long   TYPE scrtext_l.

  DATA lo_cols_tab TYPE REF TO cl_salv_columns_table.
  DATA lo_col_tab  TYPE REF TO cl_salv_column_table.
  DATA lo_cols_gen TYPE REF TO cl_salv_columns.
  DATA lo_col_gen  TYPE REF TO cl_salv_column.

  lv_long   = pv_text.
  lv_medium = pv_text.
  lv_short  = pv_text.

  " 1) Tablo kolon tipi
  TRY.
      lo_cols_tab ?= pr_cols.
      IF lo_cols_tab IS BOUND.
        lo_col_tab ?= lo_cols_tab->get_column( pv_col ).
        IF lo_col_tab IS BOUND.
          lo_col_tab->set_short_text(  lv_short ).
          lo_col_tab->set_medium_text( lv_medium ).
          lo_col_tab->set_long_text(   lv_long ).
          RETURN.
        ENDIF.
      ENDIF.
    CATCH cx_salv_not_found cx_sy_move_cast_error.
      " Devam
    CATCH cx_root.
      RETURN.
  ENDTRY.

  " 2) Genel kolon tipi
  TRY.
      lo_cols_gen ?= pr_cols.
      IF lo_cols_gen IS BOUND.
        lo_col_gen ?= lo_cols_gen->get_column( pv_col ).
        IF lo_col_gen IS BOUND.
          lo_col_gen->set_short_text(  lv_short ).
          lo_col_gen->set_medium_text( lv_medium ).
          lo_col_gen->set_long_text(   lv_long ).
        ENDIF.
      ENDIF.
    CATCH cx_salv_not_found cx_sy_move_cast_error.
    CATCH cx_root.
  ENDTRY.
ENDFORM.

*-----------------------------------------------------------------------
* Fallback Basit Listeleme (ALV olmazsa)
*-----------------------------------------------------------------------
FORM fallback_list.
  DATA lv_docui_text TYPE c LENGTH 64.
  WRITE: / '--- SONUÇ LİSTESİ (Fallback) ---'.
  ULINE.
  WRITE: / 'VBELN', 15 'STATUS', 28 'BELNR', 42 'DOCUI', 70 'MESAJ'.
  ULINE.
  LOOP AT gt_results INTO gs_result.
    CLEAR lv_docui_text.
    IF gs_result-docui IS NOT INITIAL.
      WRITE gs_result-docui TO lv_docui_text.
    ENDIF.
    WRITE: / gs_result-vbeln,
             15 gs_result-status,
             28 gs_result-belnr,
             42 lv_docui_text,
             70 gs_result-message.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
* Application Log (BAL) Helpers
*-----------------------------------------------------------------------
DATA gv_bal_log_handle TYPE balloghndl.
DATA gv_bal_created    TYPE abap_bool.

FORM bal_create.
  DATA ls_log TYPE bal_s_log.
  IF gv_bal_created = abap_true.
    RETURN.
  ENDIF.
  ls_log-object    = 'ZDLV'.
  ls_log-subobject = 'PROC'.
  ls_log-extnumber = sy-repid.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = gv_bal_log_handle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc = 0.
    gv_bal_created = abap_true.
  ENDIF.
ENDFORM.

FORM bal_msg USING    pv_type    TYPE symsgty
                        pv_obj     TYPE balobj_d
                        pv_msgno   TYPE symsgno
                        pv_text    TYPE string
                        pv_vbeln   TYPE vbrk-vbeln
                        pv_belnr   TYPE belnr_d
                        pv_docui   TYPE /itetr/com_e_docui.
  DATA ls_msg TYPE bal_s_msg.
  CHECK gv_bal_created = abap_true.

  ls_msg-msgty    = pv_type.
  ls_msg-msgid    = pv_obj.
  ls_msg-msgno    = pv_msgno.
  ls_msg-msgv1    = pv_vbeln.
  ls_msg-msgv2    = pv_belnr.
  ls_msg-msgv3    = pv_docui.
  ls_msg-detlevel = 1.
  IF pv_text IS NOT INITIAL.
    ls_msg-msgv4 = pv_text(50).
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = gv_bal_log_handle
      i_s_msg      = ls_msg
    EXCEPTIONS
      OTHERS       = 1.
ENDFORM.

FORM bal_save.
  CHECK gv_bal_created = abap_true.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle = VALUE bal_t_logh( ( gv_bal_log_handle ) )
    EXCEPTIONS
      OTHERS         = 1.
  CALL FUNCTION 'BAL_DB_COMMIT'.
ENDFORM.

*-----------------------------------------------------------------------
* Summary E-mail (Stub)
*-----------------------------------------------------------------------
FORM send_summary_email USING pv_succ TYPE i
                               pv_err  TYPE i
                               pv_skip TYPE i.
  WRITE: / 'E-posta (stub): S=', pv_succ, ' E=', pv_err, ' K=', pv_skip.
ENDFORM.
*-----------------------------------------------------------------------
* FOR Include - İş Mantığı (Selection-screen event yok)
*-----------------------------------------------------------------------

CONSTANTS: gc_mode_live TYPE c VALUE 'L',
           gc_mode_test TYPE c VALUE 'T'.

START-OF-SELECTION.
  " Test modu => kayıt yapılmasın
  DATA(lo_job) = NEW lcl_bill2dlv( iv_test = xsdbool( p_mode = gc_mode_test ) ).

  " Örnek iskelet (kendi mevcut seçme & işleme kodunla değiştir):
  DATA: ls_vbrk TYPE vbrk, ls_res TYPE ty_result.
  SELECT vbeln fkdat bukrs waerk kunrg vkorg vtweg fkart
         FROM vbrk
         INTO CORRESPONDING FIELDS OF ls_vbrk
         WHERE vbeln IN s_vbeln
           AND fkdat IN s_fkdat
           AND bukrs = p_bukrs.
    ls_res = lo_job->run( iv_vbeln = ls_vbrk-vbeln ).
    APPEND ls_res TO gt_results.
  ENDSELECT.
  "
  " PERFORM display_results.
