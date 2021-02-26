CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !previous LIKE previous OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    if_t100_message~t100key = VALUE #( msgid = zxxx_cl_bs_log=>mc_log_subobject_log
                                       msgno = '016' ).

  ENDMETHOD.

ENDCLASS.

