CLASS zz_cl_bs_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_protocol
      RETURNING
        VALUE(rt_protocol) TYPE bapirettab .
    METHODS log_message .
    METHODS log_exception
      IMPORTING
        !io_exception TYPE REF TO cx_root .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zz_cl_bs_log .
    METHODS log_bapiret
      IMPORTING
        !it_bapiret TYPE bapirettab .
    METHODS warning
      IMPORTING
        !iv_msg_txt TYPE char200 OPTIONAL
        !iv_msg_num TYPE symsgno OPTIONAL
        !i_msg_var1 TYPE any OPTIONAL
        !i_msg_var2 TYPE any OPTIONAL
        !i_msg_var3 TYPE any OPTIONAL
        !i_msg_var4 TYPE any OPTIONAL .
    METHODS log_caller .
    METHODS info
      IMPORTING
        !iv_msg_txt TYPE char200 OPTIONAL
        !iv_msg_num TYPE symsgno OPTIONAL
        !i_msg_var1 TYPE any OPTIONAL
        !i_msg_var2 TYPE any OPTIONAL
        !i_msg_var3 TYPE any OPTIONAL
        !i_msg_var4 TYPE any OPTIONAL .
    METHODS success
      IMPORTING
        !iv_msg_txt TYPE char200 OPTIONAL
        !iv_msg_num TYPE symsgno OPTIONAL
        !i_msg_var1 TYPE any OPTIONAL
        !i_msg_var2 TYPE any OPTIONAL
        !i_msg_var3 TYPE any OPTIONAL
        !i_msg_var4 TYPE any OPTIONAL .
    METHODS error
      IMPORTING
        !iv_msg_txt TYPE char200 OPTIONAL
        !iv_msg_num TYPE symsgno OPTIONAL
        !i_msg_var1 TYPE any OPTIONAL
        !i_msg_var2 TYPE any OPTIONAL
        !i_msg_var3 TYPE any OPTIONAL
        !i_msg_var4 TYPE any OPTIONAL .
    METHODS save
      IMPORTING
        !iv_created_log TYPE abap_bool DEFAULT abap_true.
    METHODS init
      IMPORTING
        !iv_object         TYPE balobj_d DEFAULT '/SCWM/WME'
        !iv_subobject      TYPE balsubobj
        !iv_extnumber      TYPE balnrext OPTIONAL
        !it_extnumber_list TYPE stringtab OPTIONAL
        !iv_lgnum          TYPE /scwm/lgnum
        !iv_reset          TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !ev_created        TYPE abap_bool .

  PROTECTED SECTION.
    TYPES: BEGIN OF s_log_handle,
             log_id      TYPE char8,
             log_handle  TYPE balloghndl,
             log_counter TYPE int2,
           END OF s_log_handle.
    TYPES: tt_log_handle TYPE TABLE OF s_log_handle.

    CLASS-DATA instance TYPE REF TO zz_cl_bs_log .

    DATA log_header TYPE bal_s_log .
    DATA log_handle TYPE balloghndl .
    DATA log_handles TYPE tt_log_handle .
    DATA message_text TYPE char200 .
    DATA message_type TYPE symsgty .
    DATA content_type TYPE i .
    DATA message_class TYPE symsgid .
    DATA message_params TYPE bal_s_parm .
    DATA message_number TYPE symsgno .
    DATA message_var1 TYPE symsgv .
    DATA message_var2 TYPE symsgv .
    DATA message_var3 TYPE symsgv .
    DATA message_var4 TYPE symsgv .
    DATA message_priority TYPE balprobcl .
    DATA message_context TYPE bal_s_cont .
    DATA lgnum TYPE /scwm/lgnum .
    DATA validity_in_days TYPE i VALUE 180 ##NO_TEXT.
    DATA log_protocol TYPE bapirettab .
    DATA log_counter TYPE i .
    DATA has_error TYPE abap_bool .
    DATA process_start TYPE timestampl.
    DATA process_end TYPE timestampl.

    CONSTANTS log_type_error TYPE symsgty VALUE 'E' ##NO_TEXT.
    CONSTANTS log_type_warning TYPE symsgty VALUE 'W' ##NO_TEXT.
    CONSTANTS log_type_success TYPE symsgty VALUE 'S' ##NO_TEXT.
    CONSTANTS log_type_info TYPE symsgty VALUE 'I' ##NO_TEXT.
    CONSTANTS message_text_id TYPE symsgid VALUE 'BL' ##NO_TEXT.
    CONSTANTS message_text_no TYPE symsgno VALUE '001' ##NO_TEXT.
    CONSTANTS log_process_create TYPE char4 VALUE 'CREA' ##NO_TEXT.
    CONSTANTS log_process_init TYPE char4 VALUE 'INIT' ##NO_TEXT.
    CONSTANTS log_process_save TYPE char4 VALUE 'SAVE' ##NO_TEXT.
    CONSTANTS log_process_exception TYPE char4 VALUE 'EXCP' ##NO_TEXT.

    METHODS add_msg_to_protocol
      IMPORTING
        !is_msg_handle TYPE balmsghndl .
    METHODS build_extnumber
      IMPORTING
        extnumber      TYPE balnrext OPTIONAL
        extnumber_list TYPE stringtab OPTIONAL
      CHANGING
        log_header     TYPE bal_s_log.
    METHODS error_handling
      IMPORTING
        !iv_process   TYPE char4
        !iv_subrc     TYPE sysubrc OPTIONAL
        !io_exception TYPE REF TO cx_root OPTIONAL .
    METHODS set_priority .
    METHODS set_content
      IMPORTING
        !iv_msg_txt TYPE char200
        !iv_msg_num TYPE symsgno
        !i_msg_var1 TYPE any
        !i_msg_var2 TYPE any
        !i_msg_var3 TYPE any
        !i_msg_var4 TYPE any .
    METHODS create_message
      IMPORTING
        !iv_msg_txt TYPE char200 OPTIONAL
        !iv_msg_num TYPE symsgno OPTIONAL
        !i_msg_var1 TYPE any OPTIONAL
        !i_msg_var2 TYPE any OPTIONAL
        !i_msg_var3 TYPE any OPTIONAL
        !i_msg_var4 TYPE any OPTIONAL .
    METHODS add_msg_by_message_text .
    METHODS add_msg_by_message_object .
    METHODS add_timestamp
      RETURNING
        VALUE(rv_time) TYPE symsgv .
    METHODS build_validity
      CHANGING
        log_header TYPE bal_s_log.
    METHODS set_context.
    METHODS log_duration.
    METHODS log_line.

ENDCLASS.



CLASS zz_cl_bs_log IMPLEMENTATION.


  METHOD add_msg_by_message_object.

    DATA(ls_msg) = VALUE bal_s_msg( msgty     = message_type
                                    probclass = message_priority
                                    msgid     = message_class
                                    msgno     = message_number
                                    msgv1     = message_var1
                                    msgv2     = message_var2
                                    msgv3     = message_var3
                                    msgv4     = message_var4 ).

    DATA(ls_msg_handle) = VALUE balmsghndl( ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = log_handle
        i_s_msg          = ls_msg
      IMPORTING
        e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.

      CALL METHOD error_handling
        EXPORTING
          iv_process = log_process_create
          iv_subrc   = sy-subrc.

    ELSE.

      CALL METHOD add_msg_to_protocol
        EXPORTING
          is_msg_handle = ls_msg_handle.

    ENDIF.

  ENDMETHOD.


  METHOD add_msg_by_message_text.

    DATA(ls_msg_handle) = VALUE balmsghndl( ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = log_handle
        i_msgty          = message_type
        i_probclass      = message_priority
        i_text           = message_text
        i_s_params       = message_params
      IMPORTING
        e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.

      CALL METHOD error_handling
        EXPORTING
          iv_process = log_process_create
          iv_subrc   = sy-subrc.

    ELSE.

      CALL METHOD add_msg_to_protocol
        EXPORTING
          is_msg_handle = ls_msg_handle.

    ENDIF.

  ENDMETHOD.


  METHOD add_msg_to_protocol.

    DATA(ls_msg) = VALUE bal_s_msg( ).

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = is_msg_handle
      IMPORTING
        e_s_msg        = ls_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.

    IF sy-subrc = 0.

      DATA(ls_bapiret2) = CORRESPONDING bapiret2( ls_msg ).
      APPEND ls_bapiret2 TO log_protocol.

    ENDIF.

  ENDMETHOD.


  METHOD add_timestamp.

    DATA: lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp.
    rv_time = |{ lv_timestamp }|.

    " DD.MM.YYYY, hh:mm:ss.ms
    rv_time = |{ rv_time+6(2) }.{ rv_time+4(2) }.{ rv_time(4) }, | &
              |{ rv_time+8(2) }:{ rv_time+10(2) }:{ rv_time+12(2) }{ rv_time+14 }|.

  ENDMETHOD.


  METHOD build_validity.

    DATA: ls_log_act TYPE /scwm/log_act.

    DO 2 TIMES.

      CASE sy-index.
        WHEN 1.
          " Note: Configure Z-Subobject of Object
          " /SCWM/WME in Transaction/SCWM/ACTLOG
          CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
            EXPORTING
              iv_lgnum     = lgnum
              iv_subobject = log_header-subobject
            IMPORTING
              es_log_act   = ls_log_act
            EXCEPTIONS
              not_found    = 1
              OTHERS       = 2.

        WHEN 2.
          ls_log_act-lgnum     = lgnum.
          ls_log_act-subobject = log_header-subobject.
          ls_log_act-validity  = validity_in_days.

      ENDCASE.

      IF    sy-subrc            = 0
        AND ls_log_act-validity > 0.

        " Append valid expiration date
        CALL FUNCTION '/SCWM/APP_LOG_EXPIRY_DATE_DET'
          EXPORTING
            is_log_act = ls_log_act
          CHANGING
            cs_log     = log_header.

        EXIT.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD build_extnumber.

    IF     extnumber IS SUPPLIED
       AND extnumber IS NOT INITIAL.

      log_header-extnumber = extnumber.

    ELSE.

      SELECT SINGLE * FROM balsubt
        INTO @DATA(ls_balsubt)
        WHERE spras     EQ @sy-langu
          AND object    EQ @log_header-object
          AND subobject EQ @log_header-subobject.

      log_header-extnumber = ls_balsubt-subobjtxt.

    ENDIF.

  ENDMETHOD.


  METHOD create_message.

    CALL METHOD set_content
      EXPORTING
        iv_msg_txt = iv_msg_txt
        iv_msg_num = iv_msg_num
        i_msg_var1 = i_msg_var1
        i_msg_var2 = i_msg_var2
        i_msg_var3 = i_msg_var3
        i_msg_var4 = i_msg_var4.

    CALL METHOD set_context.

    CALL METHOD set_priority.

    ADD 1 TO log_counter.

    CASE content_type.
      WHEN 1.
        CALL METHOD add_msg_by_message_text.

      WHEN 2.
        CALL METHOD add_msg_by_message_object.

    ENDCASE.

  ENDMETHOD.


  METHOD error.

    me->message_type = log_type_error.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = iv_msg_txt
        iv_msg_num = iv_msg_num
        i_msg_var1 = i_msg_var1
        i_msg_var2 = i_msg_var2
        i_msg_var3 = i_msg_var3
        i_msg_var4 = i_msg_var4.

  ENDMETHOD.


  METHOD error_handling.

    " Stellt sicher, dass Fehlerhandling pro Fehler nur
    " einmal fehlschlagen kann und keine Endlosschleife
    " entsteht.
    CHECK has_error EQ abap_false.
    has_error = abap_true.

*** Eingangsdaten sichern
    DATA(lv_msg_object) = log_header-object.
    DATA(lv_msg_subobj) = log_header-subobject.
    DATA(lv_msg_extnum) = log_header-extnumber.
    DATA(lv_msg_alddel) = log_header-aldate_del.

    DATA(lv_msg_class)  = message_class.
    DATA(lv_msg_typ)    = message_type.
    DATA(lv_msg_txt)    = message_text.
    DATA(lv_msg_nr)     = message_number.
    DATA(lv_msg_v1)     = message_var1.
    DATA(lv_msg_v2)     = message_var2.
    DATA(lv_msg_v3)     = message_var3.
    DATA(lv_msg_v4)     = message_var4.

    " Versuch zum alten Log Fehlernachricht bzgl.
    " fehlgeschlagenem Logging hinzuzufügen.
    error( iv_msg_txt = |Logging failed - please check log object /IWFND/.| ).

*** Bestehendes Log abschließen und neues für Fehlerbehandlung erzeugen
    CALL METHOD init
      EXPORTING
        iv_object    = '/IWFND/'
        iv_subobject = space
        iv_extnumber = 'Logging: Fehlerbehandlung'
        iv_lgnum     = lgnum
        iv_reset     = abap_true.

    CALL METHOD log_caller( ).

    error( iv_msg_txt = |Log-Process '{ iv_process }' couldn't be executed successfully.| ).

*** Allgemeine Log-Daten loggen
    DATA(lv_msg_txt_gen) = VALUE char200( ).
    lv_msg_txt_gen = '; OBJECT: ' && lv_msg_object && '; SUBOBJ: ' && lv_msg_subobj &&
                     '; EXTNUM: ' && lv_msg_extnum && '; ALDDEL: ' && lv_msg_alddel.

    CALL METHOD error
      EXPORTING
        iv_msg_txt = lv_msg_txt_gen.

    CASE iv_process.
      WHEN log_process_init.
        CASE iv_subrc.
          WHEN 1.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - Inconsistent Log-Header Data.| ).

          WHEN OTHERS.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log object couldn't be created.| ).

        ENDCASE.

      WHEN log_process_save.
        CASE iv_subrc.
          WHEN 1.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log object couldn't be found.| ).

          WHEN 2.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - Saving of the logs was rejected.| ).

          WHEN 3.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - Number assignment error while saving the log.| ).

          WHEN OTHERS.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log couldn't be saved.| ).

        ENDCASE.

      WHEN OTHERS.
        CASE sy-subrc.
          WHEN 1.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log object couldn't be found.| ).

          WHEN 2.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log message data is inconsistent.| ).

          WHEN 3.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The log object can't hold additional log messages.| ).

          WHEN OTHERS.
            error( iv_msg_txt = |SUBRC: { iv_subrc } - The message couldn't be added to the log.| ).

        ENDCASE.

    ENDCASE.

    log_message( ).

*** Prozessbedingte Log-Daten loggen
    CASE iv_process.
      WHEN log_process_init.
        " Nichts Besonderes zum Loggen

      WHEN log_process_create.
        IF message_text CN ' _0'.

          error( iv_msg_txt = |Message (Type: { lv_msg_typ }) with following text couldn't be written into the log:| ).

          CALL METHOD error
            EXPORTING
              iv_msg_txt = lv_msg_txt.

        ELSEIF message_number CN ' _0'.

          error( iv_msg_txt = |Message with No. { lv_msg_nr } ({ lv_msg_class }) couldn't be written into the log.| ).
          error( iv_msg_txt = |Variables: '{ lv_msg_v1 }', '{ lv_msg_v2 }', '{ lv_msg_v3 }' and '{ lv_msg_v4 }'.| ).

        ELSE.

          error( iv_msg_txt = |An empty message was tried to be written into the log.| ).

        ENDIF.

      WHEN log_process_exception.
        DATA(lo_exc_descr) = NEW cl_instance_description( the_subject = io_exception ).

        error( iv_msg_txt = |Exception '{ lo_exc_descr->class_name }' couldn't be written into the log.| ).

      WHEN log_process_save.
        " Nichts Besonderes zum Loggen

    ENDCASE.

    " Log für Fehlerbehandlung speichern
    save( ).

    " Fehlerhandling kann jetzt wieder aufgerufen werden
    has_error = abap_false.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_balsubt TYPE balsubt.

    IF instance IS NOT BOUND.

      CREATE OBJECT instance.

    ENDIF.

    ro_instance = instance.

  ENDMETHOD.


  METHOD get_protocol.

    rt_protocol = log_protocol.

  ENDMETHOD.


  METHOD info.

    me->message_type = log_type_info.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = iv_msg_txt
        iv_msg_num = iv_msg_num
        i_msg_var1 = i_msg_var1
        i_msg_var2 = i_msg_var2
        i_msg_var3 = i_msg_var3
        i_msg_var4 = i_msg_var4.

  ENDMETHOD.


  METHOD init.

    DATA: ls_log_act TYPE /scwm/log_act.

    IF    iv_reset EQ abap_true
      AND log_header IS NOT INITIAL.

      CALL METHOD save( ).

    ENDIF.

    " Nur nur neues Log erzeugen, wenn noch nicht vorhanden
    CHECK log_header IS INITIAL.

    ev_created = abap_true.

    lgnum                = iv_lgnum.
    log_header-object    = iv_object.
    log_header-subobject = iv_subobject.

    CALL METHOD build_extnumber
      EXPORTING
        extnumber      = iv_extnumber
        extnumber_list = it_extnumber_list
      CHANGING
        log_header     = log_header.

    CALL METHOD build_validity
      CHANGING
        log_header = log_header.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = log_header
      IMPORTING
        e_log_handle            = log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.

      CALL METHOD error_handling
        EXPORTING
          iv_process = log_process_init
          iv_subrc   = sy-subrc.

    ENDIF.

  ENDMETHOD.


  METHOD log_bapiret.

    DATA: lv_msg_txt TYPE c LENGTH 200.

    FIELD-SYMBOLS: <ls_bapiret> TYPE bapiret2.

    " Fehler loggen
    LOOP AT it_bapiret ASSIGNING <ls_bapiret>.

      CLEAR lv_msg_txt.
      lv_msg_txt = <ls_bapiret>-message.

      CASE <ls_bapiret>-type.
        WHEN 'E'.
          CALL METHOD me->error
            EXPORTING
              iv_msg_txt = lv_msg_txt.

        WHEN 'W'.
          CALL METHOD me->warning
            EXPORTING
              iv_msg_txt = lv_msg_txt.

        WHEN 'S'.
          CALL METHOD me->success
            EXPORTING
              iv_msg_txt = lv_msg_txt.

        WHEN 'I'.
          CALL METHOD me->info
            EXPORTING
              iv_msg_txt = lv_msg_txt.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD log_caller.

    DATA: lv_function TYPE  dbglevent,
          lv_method   TYPE  dbglevent,
          lv_class    TYPE  dbgsrepid,
          lv_report   TYPE  dbgsrepid,
          lv_msg_txt  TYPE c LENGTH 200.

    " Returns one of the following return codes:
    " 0 - Everything's fine
    " 1 - Log could not be found
    " 2 - Message is inconsistent
    " 3 - Log is full
    " 4 - Other messages (not specified by SAP)
    " 5 - Message text is empty

    CALL METHOD zz_cl_bs_session=>get_callstack
      IMPORTING
        ev_function = lv_function
        ev_method   = lv_method
        ev_class    = lv_class
        ev_report   = lv_report.

    IF lv_function IS NOT INITIAL.

      lv_msg_txt = lv_function.

    ELSEIF lv_class IS NOT INITIAL
      AND lv_method IS NOT INITIAL.

      CONCATENATE lv_class '=>' lv_method INTO lv_msg_txt.

    ELSEIF lv_report IS NOT INITIAL.

      lv_msg_txt = lv_report.

    ELSE.

      RETURN.

    ENDIF.

    me->message_type = 'S'.
    lv_msg_txt = '*****' && ` ` && lv_msg_txt && ` ` && 'at' && ` ` && add_timestamp( ) && ` ` && '*****'.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = lv_msg_txt.

  ENDMETHOD.


  METHOD log_exception.

    message_type  = log_type_error.

    " OTR-Text zu der Ausnahmeklasse ermitteln
    CALL METHOD io_exception->get_text
      RECEIVING
        result = message_text.

    " Name der Ausnahmeklasse ermitteln
    DATA(lo_exc_descr) = NEW cl_instance_description( the_subject = io_exception ).

    CLEAR: message_params.
    message_params-altext = 'SBAL_EXCEPTION_01'.
    APPEND VALUE #( parname  = 'EXCEPTION'
                    parvalue = lo_exc_descr->class_name ) TO message_params-t_par.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = message_text
        iv_msg_num = message_number
        i_msg_var1 = message_var1
        i_msg_var2 = message_var2
        i_msg_var3 = message_var3
        i_msg_var4 = message_var4.

  ENDMETHOD.


  METHOD log_message.

    message_class = sy-msgid.
    message_type  = sy-msgty.

    CALL METHOD create_message
      EXPORTING
        iv_msg_num = sy-msgno
        i_msg_var1 = sy-msgv1
        i_msg_var2 = sy-msgv2
        i_msg_var3 = sy-msgv3
        i_msg_var4 = sy-msgv4.

  ENDMETHOD.


  METHOD save.

    CHECK iv_created_log EQ abap_true.

    IF    instance IS BOUND
      AND has_error EQ abap_false
      AND log_counter > 0.

      GET TIME STAMP FIELD process_end.

      CALL METHOD log_duration( ).

      CALL METHOD log_line( ).

      DATA(lt_log_handles) = VALUE bal_t_logh( ( log_handle ) ).

      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle   = lt_log_handles
          i_save_all       = abap_true
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.

        CALL FUNCTION 'BAL_LOG_DELETE'
          EXPORTING
            i_log_handle  = log_handle
          EXCEPTIONS
            log_not_found = 1
            OTHERS        = 2.

        CLEAR: log_header, log_handle.

        CALL METHOD error_handling
          EXPORTING
            iv_process = log_process_save
            iv_subrc   = sy-subrc.

      ENDIF.

    ELSE.

      CALL FUNCTION 'BAL_LOG_DELETE'
        EXPORTING
          i_log_handle  = log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.

    ENDIF.

    CLEAR: log_header, log_handle.

  ENDMETHOD.


  METHOD set_content.

    DATA: lv_msg_var      TYPE string,
          lv_message_var1 TYPE string,
          lv_message_var2 TYPE string,
          lv_message_var3 TYPE string,
          lv_message_var4 TYPE string.

    IF iv_msg_txt IS NOT INITIAL.

      message_text = iv_msg_txt.

      DO 4 TIMES.

        CLEAR: lv_msg_var.

        CASE sy-index.
          WHEN 1.
            lv_msg_var = i_msg_var1.

          WHEN 2.
            lv_msg_var = i_msg_var2.

          WHEN 3.
            lv_msg_var = i_msg_var3.

          WHEN 4.
            lv_msg_var = i_msg_var4.

        ENDCASE.

        IF lv_msg_var IS NOT INITIAL.

          REPLACE FIRST OCCURRENCE OF '&' IN message_text WITH lv_msg_var.

        ELSE.

          EXIT.

        ENDIF.

      ENDDO.

      content_type = 1.

    ELSEIF iv_msg_num CN ' _'.

      message_number = iv_msg_num.

      message_var1 = CONV #( i_msg_var1 ).
      message_var2 = CONV #( i_msg_var2 ).
      message_var3 = CONV #( i_msg_var3 ).
      message_var4 = CONV #( i_msg_var4 ).

      content_type = 2.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD set_context.

    zz_cl_bs_session=>get_context(
      IMPORTING
        ev_program = DATA(lv_program)
        ev_blockname = DATA(lv_include)
        ev_line    = DATA(lv_line) ).

    DATA(ls_log_context) = VALUE zcas_s_log_context( program = lv_program
                                                     include = lv_include
                                                     line    = lv_line ).
    message_context-value   = ls_log_context.
    ##TODO " Global Structure must be created:
    " program : syrepid;
    " include : include;
    " line    : numc10;
    message_context-tabname = 'ZCAS_S_LOG_CONTEXT'.

  ENDMETHOD.


  METHOD set_priority.

    CASE me->message_type.
      WHEN 'I'.
        me->message_priority = '4'. " Zusatzinformationen

      WHEN 'S'.
        me->message_priority = '3'. " Mittel

      WHEN 'W'.
        me->message_priority = '2'. " Wichtig

      WHEN 'E'.
        me->message_priority = '1'. " Sehr wichtig

    ENDCASE.

  ENDMETHOD.


  METHOD success.

    me->message_type = log_type_success.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = iv_msg_txt
        iv_msg_num = iv_msg_num
        i_msg_var1 = i_msg_var1
        i_msg_var2 = i_msg_var2
        i_msg_var3 = i_msg_var3
        i_msg_var4 = i_msg_var4.

  ENDMETHOD.


  METHOD warning.

    me->message_type = log_type_warning.

    CALL METHOD me->create_message
      EXPORTING
        iv_msg_txt = iv_msg_txt
        iv_msg_num = iv_msg_num
        i_msg_var1 = i_msg_var1
        i_msg_var2 = i_msg_var2
        i_msg_var3 = i_msg_var3
        i_msg_var4 = i_msg_var4.

  ENDMETHOD.


  METHOD log_duration.

    IF    process_end   IS NOT INITIAL
      AND process_start IS NOT INITIAL.

      TRY.
          DATA(lv_duration) = cl_abap_tstmp=>subtract( tstmp1 = process_end
                                                       tstmp2 = process_start ).

          message_type = log_type_success.

          CALL METHOD create_message
            EXPORTING
              iv_msg_txt = |Process Duration: { lv_duration } Seconds|.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD log_line.

    message_type = log_type_success.

    CALL METHOD create_message
      EXPORTING
        iv_msg_txt = repeat( val = '-'
                             occ = 255 ).

  ENDMETHOD.
ENDCLASS.
