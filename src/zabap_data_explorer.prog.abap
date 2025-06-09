*&---------------------------------------------------------------------*
*& Report zabap_data_explorer
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_data_explorer.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE b1_title.
  PARAMETERS p_table TYPE zcl_data_explorer=>ty_tabname OBLIGATORY.
  PARAMETERS p_limit TYPE i DEFAULT 500 OBLIGATORY.
  PARAMETERS p_where TYPE char255.
  PARAMETERS p_dtech AS CHECKBOX DEFAULT abap_false.
  PARAMETERS p_optim AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.


CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS modify_screen.
    CLASS-METHODS build_where.

    CLASS-METHODS run
      IMPORTING iv_table TYPE zcl_data_explorer=>ty_tabname
                iv_limit TYPE i
                iv_optim TYPE abap_bool
                iv_dtech TYPE abap_bool
                iv_where TYPE string.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD init.
    b1_title = 'Selection Criteria'(t01).
    %_p_table_%_app_%-text = 'Table Name'(t02).
    %_p_limit_%_app_%-text = 'Row Limit'(t03).
    %_p_where_%_app_%-text = 'Filter Condition'(t04).
    %_p_dtech_%_app_%-text = 'Display Technical Names'(t05).
    %_p_optim_%_app_%-text = 'Optimize Empty Columns'(t06).
  ENDMETHOD.

  METHOD modify_screen.
    LOOP AT SCREEN.
      IF screen-name = 'P_WHERE'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_where.
    DATA lt_fields    TYPE STANDARD TABLE OF sval.
    DATA lv_table     TYPE zcl_data_explorer=>ty_tabname.
    DATA lv_where     TYPE string.
    DATA lt_dynfields TYPE STANDARD TABLE OF dynpread.

    lt_dynfields = VALUE #( ( fieldname = 'P_TABLE' )
                            ( fieldname = 'P_WHERE' ) ).

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING  dyname             = sy-repid
                 dynumb             = sy-dynnr
                 translate_to_upper = abap_true
                 request            = 'A'
      TABLES     dynpfields         = lt_dynfields
      EXCEPTIONS OTHERS             = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_table = lt_dynfields[ fieldname = 'P_TABLE' ]-fieldvalue.

    SELECT tabname,fieldname FROM dd03l
      WHERE tabname    = @lv_table
        AND as4local   = 'A'
        AND precfield  = ''
        AND datatype  <> 'CLNT'
      ORDER BY position
      INTO TABLE @lt_fields.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING  popup_title = 'Selection Criteria'(t01)
      TABLES     fields      = lt_fields
      EXCEPTIONS OTHERS      = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE value IS NOT INITIAL.
      IF lv_where IS NOT INITIAL.
        lv_where = |{ lv_where } AND|.
      ENDIF.
      lv_where = |{ lv_where } { <fs_field>-fieldname } = { cl_abap_dyn_prg=>quote( <fs_field>-value ) }|.
    ENDLOOP.
    lv_where = condense( lv_where ).

    lt_dynfields = VALUE #( ( fieldname = 'P_TABLE' fieldvalue = lv_table )
                            ( fieldname = 'P_WHERE' fieldvalue = lv_where ) ).

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING  dyname     = sy-repid
                 dynumb     = sy-dynnr
      TABLES     dynpfields = lt_dynfields
      EXCEPTIONS OTHERS     = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    zcl_data_explorer=>create( iv_table = iv_table
                               iv_limit = iv_limit
                               iv_where = iv_where
                               iv_dtech = iv_dtech
                               iv_optim = iv_optim )->display( ).
  ENDMETHOD.
ENDCLASS.


INITIALIZATION.
  lcl_app=>init( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_app=>modify_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_where.
  lcl_app=>build_where( ).


START-OF-SELECTION.
  lcl_app=>run( iv_table = p_table
                iv_limit = p_limit
                iv_optim = p_optim
                iv_dtech = p_dtech
                iv_where = CONV #( p_where ) ).
