class ZCLCA_FIXEDVALS definition
  public
  final
  create public .

public section.

  constants GC_PROC_ART_ADAIA type ZCA_PROCE_E value 'ADAIA_ART_OUTTB' ##NO_TEXT.
  constants GC_PROC_RE_CONTR type ZCA_PROCE_E value 'RE_CONTR' ##NO_TEXT.
  constants GC_PROC_ADAIA_SUP_OUTTB type ZCA_PROCE_E value 'ADAIA_SUP_OUTTB' ##NO_TEXT.
  constants GC_PROC_ASSIGN_FIELD type ZCA_PROCE_E value 'ASSIGN_FILED' ##NO_TEXT.
  constants GC_PROC_AGGR_SUB type ZCA_PROCE_E value 'AGGREMENT_SUBS' ##NO_TEXT.
  constants GC_FIELD_NAME_TCODE type NAME_FELD value 'TCODE' ##NO_TEXT.
  constants GC_BADI_ACC_DOCU type ZCA_EXITS_T-ENAME value 'BADI_ACC_DOCUMENT' ##NO_TEXT.
  constants GC_PROC_DMEE_TREE type ZCA_PROCE_E value 'DMEE_TREE' ##NO_TEXT.
  constants GC_VENDOR_GROUP type NAME_FELD value 'VENDOR_GROUP' ##NO_TEXT.
  constants GC_PAYMENT_TYPE_HIGH type NAME_FELD value 'PAYMENT_HIGH' ##NO_TEXT.
  constants GC_PAYMENT_TYPE_URGP type NAME_FELD value 'PAYMENT_URGP' ##NO_TEXT.
  constants GC_BADI_ARTICLE_REF_RT type ZCA_EXITS_T-ENAME value 'BADI_ARTICLE_REF_RT' ##NO_TEXT.
  constants GC_USEREXIT_PRICING_TKOMP type ZCA_EXITS_T-ENAME value 'USEREXIT_PRICING_PREPARE_TKOMP' ##NO_TEXT.
  constants GC_USEREXIT_PRICING_TKOMK type ZCA_EXITS_T-ENAME value 'USEREXIT_PRICING_PREPARE_TKOMK' ##NO_TEXT.
  constants GC_BADI_MATERIAL_CHECK type ZCA_EXITS_T-ENAME value 'BADI_MATERIAL_CHECK' ##NO_TEXT.
  constants GC_BADI_WPOS_WPUBON type ZCA_EXITS_T-ENAME value 'BADI_WPOS_WPUBON' ##NO_TEXT.
  constants GC_ME_PROCESS_PO_CUST type ZCA_EXITS_T-ENAME value 'ME_PROCESS_PO_CUST' ##NO_TEXT.
  constants GC_ME_GUI_PO_CUST type ZCA_EXITS_T-ENAME value 'ME_GUI_PO_CUST' ##NO_TEXT.
  constants GC_INTRASTAT_REPORT type ZCA_EXITS_T-ENAME value 'INTRASTAT_REPORT' ##NO_TEXT.
  constants GC_CVM_ATTACHEMENT type ZCA_PROCE_E value 'CVM_ATTACHMENT' ##NO_TEXT.
  constants GC_PROC_PIR_MASS_UPLOAD type ZCA_PROCE_E value 'PIR_MASS_UPLOAD' ##NO_TEXT.
  constants GC_CVM_DOC_TYPE type NAME_FELD value 'BSART' ##NO_TEXT.
  constants GC_CVM_EKORG type NAME_FELD value 'EKORG' ##NO_TEXT.
  constants GC_INSTORE_SALES type ZCA_PROCE_E value 'INSTORE_SALES' ##NO_TEXT.
  constants GC_INSTORE_MOVETYPE type NAME_FELD value 'MOVEMENT_TYPE' ##NO_TEXT.
  constants GC_DELIVERY_PUSH type ZCA_EXITS_T-ENAME value 'DELIVERY_PUBLISH' ##NO_TEXT.
  constants GC_INSTORE_DOCTYPE type NAME_FELD value 'LFART' ##NO_TEXT.
  constants GC_PROC_CUST_STORE_RETURNS type ZCA_PROCE_E value 'CUST_STORE_RETURNS' ##NO_TEXT.
  constants GC_MODULE_MM type ZCA_MODUL_E value 'MM' ##NO_TEXT.
  constants GC_EXIT_MV50AFZ1 type ZCA_EXITS_T-ENAME value 'MV50AFZ1' ##NO_TEXT.
  constants GC_MARA type ZCA_CONSTANTS_T-LOW value 'MARA' ##NO_TEXT.
  constants GC_MARM type ZCA_CONSTANTS_T-LOW value 'MARM' ##NO_TEXT.
  constants GC_PROCE_EXEARTICLES type ZCA_PROCE_E value 'EXEARTICLES' ##NO_TEXT.
  constants GC_SEPARATOR_SLASH type C value '/' ##NO_TEXT.
  constants GC_PROC_PLM_ARTICLES type ZCA_PROCE_E value 'PLM_ARTICLES' ##NO_TEXT.
  constants GC_RECE_ASSET_ASSIGNMENT type ZCA_EXITS_T-ENAME value 'BADI_RECE_ASSEST_ASSIGNMENT' ##NO_TEXT.
  constants GC_PROC_PO_SEND type ZCA_PROCE_E value 'PO_SEND' ##NO_TEXT.
  constants GC_PROC_PRC_MASS_UPLD type ZCA_PROCE_E value 'PRC_MASS_UPLD' ##NO_TEXT.
  constants GC_PROC_SMFO type ZCA_PROCE_E value 'SMFO' ##NO_TEXT.
  constants GC_PROC_BCM_CORRESP type ZCA_PROCE_E value 'BCM_CORRESP' ##NO_TEXT.
  constants GC_PROC_CUST_B2B type ZCA_PROCE_E value 'CUSTOMERSB2B' ##NO_TEXT.
  constants GC_PROC_ONLI_STORE type ZCA_PROCE_E value 'ONLINESTORES' ##NO_TEXT.
  constants GC_PROC_PART_CODE type ZCA_PROCE_E value 'PARTNER_CODE' ##NO_TEXT.
  constants GC_PROC_CLL_CNTRY type ZCA_PROCE_E value 'CLL_COUNTRY' ##NO_TEXT.
  constants GC_PROC_CLL_DISC type ZCA_PROCE_E value 'CLL_DISCOUNTS' ##NO_TEXT.
  constants GC_PROC_PO_INTERCO type ZCA_PROCE_E value 'PO_INTERCO' ##NO_TEXT.
  constants GC_BNK_BADI_UI_ENHANCE type ZCA_EXITS_T-ENAME value 'BNK_BADI_UI_ENHANCE' ##NO_TEXT.
  constants GC_PROC_EXE_SART type ZCA_PROCE_E value 'EXE_STRUCT_ARTICLES' ##NO_TEXT.
  constants GC_PROC_EXE_GEN type ZCA_PROCE_E value 'EXE_GENERAL' ##NO_TEXT.
  constants GC_PROC_OMS_PGTO_IM type ZCA_PROCE_E value 'OMS_PGTO_IM' ##NO_TEXT.
  constants GC_PROC_OMS_PGTO_MB type ZCA_PROCE_E value 'OMS_PGTO_MB' ##NO_TEXT.
  constants GC_PROC_ARTICLE_MASS_CREATE type ZCA_PROCE_E value 'ARTICLE_MASS_CREATE' ##NO_TEXT.
  constants GC_EXIT_PR_POTYPE type ZCA_EXITS_T-ENAME value 'ME_BSART_DET' ##NO_TEXT.
  constants GC_PROC_CONT_PERS type ZCA_PROCE_E value 'CONTACT_PERS' ##NO_TEXT.
  constants GC_PROC_AGREEM_SUBS type ZCA_PROCE_E value 'AGREEMENTS SUBSCRIPT' ##NO_TEXT.
  constants GC_PROC_PO_COND_INACT type ZCA_PROCE_E value 'PO_COND_INACT' ##NO_TEXT.
  constants GC_PROC_PO_FORM type ZCA_PROCE_E value 'PO_FORM' ##NO_TEXT.
  constants GC_PROC_ARTIC_OUTBD type ZCA_PROCE_E value 'ARTIC_OUTBD' ##NO_TEXT.
  constants GC_PROC_STORE_OUTBD type ZCA_PROCE_E value 'STORE_OUTBD' ##NO_TEXT.
  constants GC_PROC_CVM_REFDESIGN type ZCA_PROCE_E value 'Z_REF_DESIGN' ##NO_TEXT.
  constants GC_PROC_CVM_LIFSTYE type ZCA_PROCE_E value 'Z_LIFESTYLE' ##NO_TEXT.
  constants GC_PROC_CVM_PROGRAM type ZCA_PROCE_E value 'Z_PROGRAM' ##NO_TEXT.
  constants GC_CVM_DESIGN type NAME_FELD value 'REF_DESIGN' ##NO_TEXT.
  constants GC_CVM_PROGRAM type NAME_FELD value 'PROGRAM' ##NO_TEXT.
  constants GC_CVM_LIFESTYLE type NAME_FELD value 'LIFESTYLE' ##NO_TEXT.
  constants GC_PROC_PO_EXCHANGE_RATE type ZCA_PROCE_E value 'DEFINE_EXCHANGE_RATE' ##NO_TEXT.
  constants GC_PROC_PO_ITM_LIMIT type ZCA_PROCE_E value 'PO_ITM_LIMIT' ##NO_TEXT.
  constants GC_PROC_VAL_RULE type ZCA_PROCE_E value 'VAL_RULE' ##NO_TEXT.
  constants GC_PROC_SEND_ART type ZCA_PROCE_E value 'SEND_ARTICLES' ##NO_TEXT.
  constants GC_PROC_EXE_SO type ZCA_PROCE_E value 'EXE_SALES_ORDERS' ##NO_TEXT.
  constants GC_PROC_EXE_STOR type ZCA_PROCE_E value 'EXE_STORES' ##NO_TEXT.
  constants GC_PROC_EXE_REC_CONF type ZCA_PROCE_E value 'EXE_REC_CONF' ##NO_TEXT.
  constants GC_PROC_EXE_SHIP type ZCA_PROCE_E value 'EXE_SHIPMENTS' ##NO_TEXT.
  constants GC_PROC_EXE_SHIPEXCP type ZCA_PROCE_E value 'EXE_SHIPEXCP' ##NO_TEXT.
  constants GC_PROC_EXE_DLVCANC type ZCA_PROCE_E value 'EXE_DLVCANC' ##NO_TEXT.
  constants GC_MODULE_SD type ZCA_MODUL_E value 'SD' ##NO_TEXT.
  constants GC_MODULE_CA type ZCA_MODUL_E value 'CA' ##NO_TEXT.
  constants GC_MODULE_FI type ZCA_MODUL_E value 'FI' ##NO_TEXT.
  constants GC_PROC_SALESFORCE type ZCA_PROCE_E value 'SALESFORCE' ##NO_TEXT.
  constants GC_PROC_SAPHETY_PO type ZCA_PROCE_E value 'SAPHETY_PURCH_ORDERS' ##NO_TEXT.
  constants GC_PROC_SAPHETY_RECADV type ZCA_PROCE_E value 'SAPHETY_RECADV' ##NO_TEXT.
  constants GC_PROC_LOGO type ZCA_PROCE_E value 'LOGO' ##NO_TEXT.
  constants GC_PROC_FOOTER type ZCA_PROCE_E value 'FOOTER' ##NO_TEXT.
  constants GC_PROC_MIRO_SUBST type ZCA_PROCE_E value 'MIRO_SUBST' ##NO_TEXT.
  constants GC_PROC_DLV_DATE type ZCA_PROCE_E value 'DLV_DATE' ##NO_TEXT.
  constants GC_PROC_F110_AVIS_INT type ZCA_PROCE_E value 'ZFI_F110_AVIS_INT_AF' ##NO_TEXT.
  constants GC_PROC_F150_DUNN type ZCA_PROCE_E value 'ZFI_F150_DUNN_SF' ##NO_TEXT.
  constants GC_PROC_SD_INVOICE type ZCA_PROCE_E value 'ZFI_SD_INVOICE_FORM01' ##NO_TEXT.
  constants GC_PROC_INVENT_ANNUAL type ZCA_PROCE_E value 'INVENT_ANNUAL' ##NO_TEXT.
  constants GC_PROC_SLSB_PRICE type ZCA_PROCE_E value 'SLSBROKER_PRICES' ##NO_TEXT.
  constants GC_PROC_MASS_ART_GROUP type ZCA_PROCE_E value 'MASS_ARTICLE_GRP' ##NO_TEXT.
  constants GC_PROC_MM_AUTOMAT_CONFIRM type ZCA_PROCE_E value 'MM_AUTOMAT_CONFIRM' ##NO_TEXT.
  constants GC_PROC_FI_FOOTER type ZCA_PROCE_E value 'FOOTER' ##NO_TEXT.
  constants GC_PROC_FI_DEBIT_CREDIT type ZCA_PROCE_E value 'ZFI_INVOICE_DEBIT_CR' ##NO_TEXT.
  constants GC_PROC_FI_RFKORD50_PDF type ZCA_PROCE_E value 'RFKORD50' ##NO_TEXT.
  constants GC_PROC_HTRAN_OUTBD type ZCA_PROCE_E value 'HIRCY_TRANSL' ##NO_TEXT.
  constants GC_PROC_REPL_PARAMS type ZCA_PROCE_E value 'REPL_PARAMS' ##NO_TEXT.
  constants GC_PROC_EXPEDITION_CUST type ZCA_PROCE_E value 'EXPEDITION_CUST' ##NO_TEXT.
  constants GC_PROC_DNA_ARTICLES type ZCA_PROCE_E value 'DNA_ARTICLES' ##NO_TEXT.
  constants GC_PROC_DNA_SUPPLIERS type ZCA_PROCE_E value 'DNA_SUPPLIERS' ##NO_TEXT.
  constants GC_PROC_NEOGRID_PRD_ORD type ZCA_PROCE_E value 'NEOGRID_PROD_ORD' ##NO_TEXT.
  constants GC_PROC_NEOGRID_PUR_ORD type ZCA_PROCE_E value 'NEOGRID_PURC_ORD' ##NO_TEXT.

  class-methods GET_CONS_VAL
    importing
      !IV_BUKRS type ZCA_CONSTANTS_T-BUKRS
      !IV_MODUL type ZCA_CONSTANTS_T-MODUL
      !IV_PROCE type ZCA_CONSTANTS_T-PROCE
      !IV_FNAME type ZCA_CONSTANTS_T-FNAME optional
      !IV_SEQUE type ZCA_CONSTANTS_T-SEQUE default 1
    exporting
      !EV_CONST type ANY
    exceptions
      NO_DATA .
  class-methods GET_CONS_RAN
    importing
      !IV_BUKRS type ZCA_CONSTANTS_T-BUKRS
      !IV_MODUL type ZCA_CONSTANTS_T-MODUL
      !IV_PROCE type ZCA_CONSTANTS_T-PROCE
      !IV_FNAME type ZCA_CONSTANTS_T-FNAME
      !IV_SEQUE type ZCA_CONSTANTS_T-SEQUE optional
    exporting
      !ET_RANGE type TABLE
    exceptions
      NO_DATA .
  class-methods GET_CONS_PRO .
  class-methods GET_EXIT
    importing
      !IV_BUKRS type ZCA_EXITS_T-BUKRS
      !IV_ENAME type ZCA_EXITS_T-ENAME
    exporting
      !EV_CLASS type ZCA_EXITS_T-CLASS
      !EV_MTHOD type ZCA_EXITS_T-MTHOD
    exceptions
      NO_DATA .
  class-methods GET_EXITS
    importing
      !IV_BUKRS type ZCA_EXITS_T-BUKRS
      !IV_ENAME type ZCA_EXITS_T-ENAME
    exporting
      !ET_EXITS type ZCA_EXIT_CLMT_TT
    exceptions
      NO_DATA .
  class-methods FILL_BAPIUPDATE_STRUCT
    importing
      !I_BAPISTRUCT type ANY
      !I_NAMESTRUCT type STRING
      !I_NAMESTRUCTX type STRING
    changing
      !C_BAPISTRUCTX type ANY .
  PROTECTED SECTION.
private section.

  types:
    ty_r_seque TYPE RANGE OF zca_constants_t-seque .

  class-data GO_IDATA type ref to DATA .
  class-data GO_EDATA type ref to DATA .
  constants GC_SIGN type NAME_FELD value 'SIGN' ##NO_TEXT.
  constants GC_OPTI type NAME_FELD value 'OPTI' ##NO_TEXT.
  constants GC_OPTION type NAME_FELD value 'OPTION' ##NO_TEXT.
  constants GC_LOW type NAME_FELD value 'LOW' ##NO_TEXT.
  constants GC_HIGH type NAME_FELD value 'HIGH' ##NO_TEXT.

  class-methods GET_EXP_RANGE
    changing
      !CS_EDATA type ANY .
  class-methods MOVE_DATA
    importing
      !IV_FNAME type NAME_FELD .
  class-methods SET_RANGES
    importing
      !IS_EDATA type ANY
      !IS_IDATA type ANY .
ENDCLASS.



CLASS ZCLCA_FIXEDVALS IMPLEMENTATION.


  METHOD fill_bapiupdate_struct.

    DATA: lo_bapist  TYPE REF TO data,
          lo_bapistx TYPE REF TO data,
          ls_comp    TYPE abap_componentdescr,
          lo_struct  TYPE REF TO cl_abap_structdescr,
          lo_structx TYPE REF TO cl_abap_structdescr,
          lo_type    TYPE REF TO cl_abap_typedescr,
          ls_dd04v   TYPE dd04v,
          lv_ddname  TYPE ddobjname,
          lv_fldname TYPE string.

    FIELD-SYMBOLS: <fs_component> TYPE abap_compdescr,
                   <fs_value>     TYPE any,
                   <fs_update>    TYPE bapiupdate.


    TRY.
        CREATE DATA lo_bapist TYPE (i_namestruct).
      CATCH cx_sy_create_data_error.
    ENDTRY.

    TRY.
        CREATE DATA lo_bapistx TYPE (i_namestructx).
      CATCH cx_sy_create_data_error.
    ENDTRY.

    lo_struct ?= cl_abap_typedescr=>describe_by_data_ref( lo_bapist ).
    lo_structx ?= cl_abap_typedescr=>describe_by_data_ref( lo_bapistx ).

    LOOP AT lo_struct->components ASSIGNING <fs_component>.

      CLEAR: lv_fldname, lv_ddname, ls_dd04v.

* Verifica se campo está prenchido
      ASSIGN COMPONENT <fs_component>-name OF STRUCTURE i_bapistruct TO <fs_value>.
      CHECK <fs_value> IS ASSIGNED.
      CHECK <fs_value> IS NOT INITIAL.

* Campo da estrutura BAPIUPDATE
      CONCATENATE lo_structx->absolute_name+6 '-' <fs_component>-name INTO lv_fldname.
*        lo_type ?= cl_abap_typedescr=>describe_by_name( lv_fldname ).
      CALL METHOD cl_abap_typedescr=>describe_by_name
        EXPORTING
          p_name         = lv_fldname
        RECEIVING
          p_descr_ref    = lo_type
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.
      CHECK sy-subrc = 0.
      lv_ddname = lo_type->absolute_name+6.
      IF lv_ddname = 'BAPIUPDATE'.
        ASSIGN COMPONENT <fs_component>-name OF STRUCTURE c_bapistructx TO <fs_update>.
        CHECK <fs_update> IS ASSIGNED.
        <fs_update> = abap_true.
      ENDIF.
      UNASSIGN: <fs_value>, <fs_update>.
      FREE: lo_type.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cons_pro.
  ENDMETHOD.


  METHOD get_cons_ran.

*   Range for sequential number
    DATA(lr_seque) = COND ty_r_seque( WHEN NOT iv_seque IS INITIAL
                                        THEN VALUE #( ( sign = ztgca_c_sign_i
                                                               option = ztgca_c_option_eq
                                                               low    = iv_seque
                                                    ) )
                                     ).

*   Get constants data
    SELECT sign,
           opti,
           low,
           high
      FROM zca_constants_t
      INTO TABLE @DATA(lt_range)
      WHERE bukrs EQ @iv_bukrs
        AND modul EQ @iv_modul
        AND proce EQ @iv_proce
        AND fname EQ @iv_fname
        AND seque IN @lr_seque
        AND activ EQ @abap_true.
    IF NOT sy-subrc IS INITIAL.
      RAISE no_data.
    ENDIF.

*   Export data
    LOOP AT lt_range ASSIGNING FIELD-SYMBOL(<fs_range>).
      APPEND INITIAL LINE TO et_range ASSIGNING FIELD-SYMBOL(<fs_erange>).
      CHECK <fs_erange> IS ASSIGNED.

      set_ranges( is_idata = <fs_range>
                  is_edata = <fs_erange> ).

      move_data(: gc_sign ),
                  gc_opti ),
                  gc_low  ),
                  gc_high ).

      get_exp_range( CHANGING cs_edata = <fs_erange> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cons_val.

    SELECT SINGLE low
      FROM zca_constants_t
      INTO ev_const
      WHERE bukrs EQ iv_bukrs
        AND modul EQ iv_modul
        AND proce EQ iv_proce
        AND fname EQ iv_fname
        AND seque EQ iv_seque
        AND activ EQ abap_true.
    IF NOT sy-subrc IS INITIAL.
      RAISE no_data.
    ENDIF.

  ENDMETHOD.


  METHOD get_exit.

    SELECT SINGLE class
                  mthod
      FROM zca_exits_t
      INTO (ev_class,ev_mthod)
      WHERE bukrs EQ iv_bukrs
        AND ename EQ iv_ename
        AND seque EQ 0
        AND activ EQ abap_true.
    IF NOT sy-subrc IS INITIAL.
      RAISE no_data.
    ENDIF.

  ENDMETHOD.


  METHOD get_exits.

    SELECT class
           mthod
      FROM zca_exits_t
      INTO TABLE et_exits
      WHERE bukrs EQ iv_bukrs
        AND ename EQ iv_ename
        AND activ EQ abap_true.
    IF NOT sy-subrc IS INITIAL.
      RAISE no_data.
    ENDIF.

  ENDMETHOD.


  METHOD get_exp_range.

    ASSIGN go_edata->* TO FIELD-SYMBOL(<fs_edata>).
    CHECK <fs_edata> IS ASSIGNED.
    cs_edata = <fs_edata>.

  ENDMETHOD.


  METHOD move_data.

    ASSIGN: go_idata->* TO FIELD-SYMBOL(<fs_idata>),
            go_edata->* TO FIELD-SYMBOL(<fs_edata>).
    CHECK: <fs_idata> IS ASSIGNED,
           <fs_edata> IS ASSIGNED.

    ASSIGN COMPONENT: iv_fname OF STRUCTURE <fs_idata> TO FIELD-SYMBOL(<fs_ifield>),
                      COND #( WHEN iv_fname EQ gc_opti
                                THEN gc_option
                              ELSE iv_fname ) OF STRUCTURE <fs_edata> TO FIELD-SYMBOL(<fs_efield>).

    CHECK: <fs_ifield> IS ASSIGNED,
           <fs_efield> IS ASSIGNED.

    <fs_efield> = <fs_ifield>.

  ENDMETHOD.


  METHOD set_ranges.

    CREATE DATA: go_idata LIKE is_idata,
                 go_edata LIKE is_edata.

    ASSIGN: go_idata->* TO FIELD-SYMBOL(<fs_idata>),
            go_edata->* TO FIELD-SYMBOL(<fs_edata>).

    CHECK: <fs_idata> IS ASSIGNED,
           <fs_edata> IS ASSIGNED.

    <fs_idata> = is_idata.
    <fs_edata> = is_edata.

  ENDMETHOD.
ENDCLASS.
