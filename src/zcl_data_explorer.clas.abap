CLASS zcl_data_explorer DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_fields  TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.
    TYPES tt_fkeys   TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY.
    TYPES ty_tabname TYPE tabname.

    CONSTANTS mc_client_type   TYPE string VALUE 'CLNT'.
    CONSTANTS mc_default_limit TYPE i      VALUE 500.

    CLASS-METHODS create
      IMPORTING iv_table           TYPE ty_tabname
                iv_limit           TYPE i         DEFAULT mc_default_limit
                iv_where           TYPE string    OPTIONAL
                iv_dtech           TYPE abap_bool DEFAULT abap_false
                iv_optim           TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_data_explorer.

    METHODS display.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_report,
             table  TYPE ty_tabname,
             ttext  TYPE string,
             limit  TYPE i,
             optim  TYPE abap_bool,
             dtech  TYPE abap_bool,
             where  TYPE string,
             fields TYPE tt_fields,
             fkeys  TYPE tt_fkeys,
             data   TYPE REF TO data,
             salv   TYPE REF TO cl_salv_table,
           END OF ty_report.

    DATA ms_report TYPE ty_report.

    METHODS constructor
      IMPORTING iv_table TYPE clike
                iv_limit TYPE i
                iv_where TYPE clike
                iv_dtech TYPE abap_bool
                iv_optim TYPE abap_bool.

    METHODS get_metadata.

    METHODS get_data.

    METHODS build_salv.

    METHODS build_header.

    METHODS build_columns.

    METHODS optimize_columns.

    METHODS get_text_table
      IMPORTING iv_table        TYPE ty_tabname
      RETURNING VALUE(rv_table) TYPE ty_tabname.

    METHODS choose_table
      IMPORTING iv_main_table   TYPE ty_tabname
                iv_text_table   TYPE ty_tabname
      RETURNING VALUE(rv_table) TYPE ty_tabname.

    METHODS build_where_clause
      IMPORTING is_data         TYPE any
                iv_field        TYPE fieldname
      RETURNING VALUE(rv_where) TYPE string.

    METHODS handle_hotspot_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING sender !row !column.
ENDCLASS.



CLASS ZCL_DATA_EXPLORER IMPLEMENTATION.


  METHOD build_columns.
    ms_report-salv->get_columns( )->set_optimize( ).
    ms_report-salv->get_columns( )->set_key_fixation( ).
    LOOP AT ms_report-salv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<fs_column>).
      DATA(lo_column) = CAST cl_salv_column_table( <fs_column>-r_column ).

      ASSIGN ms_report-fields[ fieldname = <fs_column>-columnname ] TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <fs_field>-keyflag = abap_true.
        lo_column->set_key( ).
      ENDIF.

      IF <fs_field>-checktable IS NOT INITIAL.
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_color( value = VALUE lvc_s_colo( col = col_total ) ).
      ENDIF.

      IF <fs_field>-datatype = mc_client_type.
        lo_column->set_technical( ).
      ENDIF.

      IF ms_report-dtech = abap_true.
        lo_column->set_long_text( CONV #( <fs_column>-columnname ) ).
        lo_column->set_medium_text( CONV #( <fs_column>-columnname ) ).
        lo_column->set_short_text( CONV #( <fs_column>-columnname ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_header.
    ms_report-salv->get_display_settings( )->set_list_header( |{ ms_report-table } / { ms_report-ttext }| ).
  ENDMETHOD.


  METHOD build_salv.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    ASSIGN ms_report-data->* TO <fs_table>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = ms_report-salv
                                CHANGING  t_table      = <fs_table> ).

        ms_report-salv->get_functions( )->set_all( ).
        ms_report-salv->get_display_settings( )->set_striped_pattern( abap_true ).
        SET HANDLER handle_hotspot_click FOR ms_report-salv->get_event( ).
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD build_where_clause.
    LOOP AT ms_report-fkeys ASSIGNING FIELD-SYMBOL(<fs_key>) WHERE     tabname   = ms_report-table
                                                                   AND fieldname = iv_field
                                                                   AND fortable  = ms_report-table.
      IF line_exists( ms_report-fields[ fieldname = <fs_key>-forkey
                                        datatype  = mc_client_type ] ).
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <fs_key>-forkey OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc <> 0 OR <fs_value> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF rv_where IS NOT INITIAL.
        rv_where = |{ rv_where } AND|.
      ENDIF.

      rv_where = |{ rv_where } { <fs_key>-checkfield } = { cl_abap_dyn_prg=>quote( <fs_value> ) }|.
    ENDLOOP.
    rv_where = condense( rv_where ).
  ENDMETHOD.


  METHOD choose_table.
    DATA lv_answer TYPE c LENGTH 1.

    rv_table = iv_main_table.

    CALL FUNCTION 'K_KKB_POPUP_RADIO2'
      EXPORTING  i_title   = 'Which table would you like to continue with?'(t01)
                 i_text1   = |{ iv_main_table } { 'Main Table'(t02) }|
                 i_text2   = |{ iv_text_table } { 'Text Table'(t03) }|
                 i_default = '1'
      IMPORTING  i_result  = lv_answer
      EXCEPTIONS OTHERS    = 1.
    IF sy-subrc = 0 AND lv_answer = '2'.
      rv_table = iv_text_table.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    ms_report-table = iv_table.
    ms_report-limit = iv_limit.
    ms_report-optim = iv_optim.
    ms_report-dtech = iv_dtech.
    ms_report-where = iv_where.

    get_metadata( ).

    get_data( ).

    build_salv( ).

    build_header( ).

    build_columns( ).

    IF ms_report-optim = abap_true.
      optimize_columns( ).
    ENDIF.
  ENDMETHOD.


  METHOD create.
    ro_instance = NEW #( iv_table = iv_table
                         iv_limit = iv_limit
                         iv_where = iv_where
                         iv_dtech = iv_dtech
                         iv_optim = iv_optim ).
  ENDMETHOD.


  METHOD display.
    ms_report-salv->display( ).
  ENDMETHOD.


  METHOD get_data.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    TRY.
        CREATE DATA ms_report-data TYPE STANDARD TABLE OF (ms_report-table).

        ASSIGN ms_report-data->* TO <fs_table>.

        SELECT *
          FROM (ms_report-table)
          WHERE (ms_report-where)
          ORDER BY PRIMARY KEY
          INTO TABLE @<fs_table>
          UP TO @ms_report-limit ROWS.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_metadata.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING  name          = ms_report-table
      TABLES     dd03p_tab     = ms_report-fields
                 dd05m_tab     = ms_report-fkeys
      EXCEPTIONS illegal_input = 1
                 OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SELECT SINGLE ddtext FROM dd02t
      WHERE tabname    = @ms_report-table
        AND ddlanguage = @sy-langu
        AND as4local   = 'A'
      INTO @ms_report-ttext.
    IF sy-subrc <> 0.
      SELECT SINGLE ddtext FROM dd02t
        WHERE tabname    = @ms_report-table
          AND ddlanguage = 'E'
          AND as4local   = 'A'
        INTO @ms_report-ttext.
    ENDIF.
  ENDMETHOD.


  METHOD get_text_table.
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING tabname   = iv_table
      IMPORTING texttable = rv_table.
  ENDMETHOD.


  METHOD handle_hotspot_click.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    ASSIGN ms_report-data->* TO <fs_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN <fs_table>[ row ] TO FIELD-SYMBOL(<fs_data>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT column OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc <> 0 OR <fs_value> IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN ms_report-fields[ fieldname = column ] TO FIELD-SYMBOL(<fs_field>).
    IF sy-subrc <> 0 OR <fs_field>-checktable IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_table) = get_text_table( <fs_field>-checktable ).
    IF lv_table IS INITIAL.
      lv_table = <fs_field>-checktable.
    ELSE.
      lv_table = choose_table( iv_main_table = <fs_field>-checktable
                               iv_text_table = lv_table ).
    ENDIF.

    zcl_data_explorer=>create( iv_table = lv_table
                               iv_limit = ms_report-limit
                               iv_where = build_where_clause( is_data  = <fs_data>
                                                              iv_field = column )
                               iv_dtech = ms_report-dtech
                               iv_optim = ms_report-optim
    )->display( ).
  ENDMETHOD.


  METHOD optimize_columns.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
    DATA lv_where TYPE string.

    ASSIGN ms_report-data->* TO <fs_table>.
    IF sy-subrc <> 0 OR <fs_table> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ms_report-salv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<fs_column>).
      lv_where = |{ <fs_column>-columnname } IS NOT INITIAL|.
      LOOP AT <fs_table> TRANSPORTING NO FIELDS WHERE (lv_where).
      ENDLOOP.
      IF sy-subrc <> 0.
        <fs_column>-r_column->set_technical( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
