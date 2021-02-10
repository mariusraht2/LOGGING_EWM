CLASS zxxx_cl_bs_session DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_context
      EXPORTING
        !ev_program   TYPE syrepid
        !ev_blockname TYPE include
        !ev_line      TYPE int4.
    CLASS-METHODS get_callstack
      EXPORTING
        !ev_method   TYPE dbglevent
        !ev_class    TYPE dbgsrepid
        !ev_report   TYPE dbgsrepid
        !ev_function TYPE dbglevent .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zxxx_cl_bs_session IMPLEMENTATION.


  METHOD get_callstack.

    TYPES: BEGIN OF ty_abap_callstack_entry,
             mainprogram TYPE dbgsrepid,
             include     TYPE dbgsrepid,
             line        TYPE dbglinno,
             eventtype   TYPE dbglevtype,
             event       TYPE dbglevent,
             flag_system TYPE c LENGTH 1,
           END OF ty_abap_callstack_entry,

           ty_abap_callstack TYPE STANDARD TABLE OF ty_abap_callstack_entry WITH DEFAULT KEY.

    DATA: ls_aut_callstack TYPE ty_abap_callstack_entry,
          lt_aut_callstack TYPE ty_abap_callstack.

    DATA: lv_find_offset TYPE i,
          lv_mainprogram TYPE dbgsrepid.

    FIELD-SYMBOLS: <ls_aut_callstack> TYPE ty_abap_callstack_entry.

    CALL 'ABAP_CALLSTACK' ID 'DEPTH' FIELD -10 ID 'CALLSTACK' FIELD lt_aut_callstack. "#EC CI_CCALL

    LOOP AT lt_aut_callstack ASSIGNING <ls_aut_callstack>
      WHERE event NE 'GET_CALLSTACK'
        AND event NE 'LOG_CALLER'
        AND event NE 'INIT_LOG'.

      ls_aut_callstack = <ls_aut_callstack>.
      EXIT.

    ENDLOOP.

    IF ls_aut_callstack-mainprogram CS '='.

      FIND FIRST OCCURRENCE OF '=' IN ls_aut_callstack-mainprogram  MATCH OFFSET lv_find_offset.

      lv_mainprogram = ls_aut_callstack-mainprogram+0(lv_find_offset).

    ELSE.

      lv_mainprogram = ls_aut_callstack-mainprogram.

    ENDIF.

    CASE ls_aut_callstack-eventtype.
      WHEN 'FUNCTION'.
        " Funktionsname auslesen
        ev_function = ls_aut_callstack-event.

      WHEN 'METHOD'.
        " Klassenname auslesen
        ev_class = lv_mainprogram.

        " Methodenname auslesen
        ev_method = ls_aut_callstack-event.

      WHEN 'EVENT' OR 'FORM'.
        " Programmname auslesen
        ev_report = lv_mainprogram.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

  ENDMETHOD.


  METHOD get_context.

    DATA(lt_callstack) = VALUE abap_callstack( ).

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = lt_callstack.

    DELETE lt_callstack WHERE mainprogram CS 'CL_BS_LOG'
                           OR mainprogram CS 'CL_BS_SESSION'.

    ASSIGN lt_callstack[ 1 ] TO FIELD-SYMBOL(<ls_callstack>).
    ev_program   = <ls_callstack>-mainprogram.
    ev_blockname = <ls_callstack>-blockname.
    ev_line      = <ls_callstack>-line.

  ENDMETHOD.

ENDCLASS.
