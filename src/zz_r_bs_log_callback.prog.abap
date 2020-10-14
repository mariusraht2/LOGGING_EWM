*&---------------------------------------------------------------------*
*& Report zz_r_bs_log_callback
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zz_r_bs_log_callback.

FORM on_click_msg_detail TABLES i_params STRUCTURE spar.

  DATA: lv_log_number     TYPE balognr,
        lv_msg_param_id   TYPE ziot_cl_bs_log=>v_message_param_id,
        ls_structure_name TYPE dd02l-tabname.

  FIELD-SYMBOLS: <lt_outtab> TYPE STANDARD TABLE.

  " Find out the identifier for this message
  lv_log_number = VALUE #( i_params[ param = ziot_cl_bs_log=>c_log_number ]-value OPTIONAL ).
  CHECK lv_log_number IS NOT INITIAL.

  " Load specific message details from database
  DATA(lt_msg_details) = VALUE /scwm/tt_msg_details( ).
  CALL FUNCTION '/SCWM/DLV_IMPORT_LOG'
    EXPORTING
      iv_lognumber   = lv_log_number
    IMPORTING
      et_msg_details = lt_msg_details.

  CHECK lt_msg_details IS NOT INITIAL.

  lv_msg_param_id = VALUE #( i_params[ param = ziot_cl_bs_log=>c_msg_ident ]-value OPTIONAL ).
  CHECK lv_msg_param_id IS NOT INITIAL.

  " Search for those entries which belong to this message
  ASSIGN lt_msg_details[ v_id = lv_msg_param_id ] TO FIELD-SYMBOL(<ls_msg_details>).
  CHECK <ls_msg_details> IS ASSIGNED.

  IF ziot_cl_bs_log=>gui_alv_grid IS NOT INITIAL.
    ziot_cl_bs_log=>gui_alv_grid->free( ).
    CLEAR ziot_cl_bs_log=>gui_alv_grid.
  ENDIF.

  "    Show container if not visible
  " OR Hide container if detail to same message was again being selected
  IF    ziot_cl_bs_log=>gui_docking_container IS BOUND
    AND ziot_cl_bs_log=>sel_message_param_id EQ lv_msg_param_id.

    ziot_cl_bs_log=>gui_docking_container->free( ).
    CLEAR: ziot_cl_bs_log=>gui_docking_container,
           ziot_cl_bs_log=>sel_message_param_id.

  ELSEIF ziot_cl_bs_log=>gui_docking_container IS NOT BOUND.

    CREATE OBJECT ziot_cl_bs_log=>gui_docking_container
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = '120'.
    ziot_cl_bs_log=>gui_docking_container->set_visible( abap_true ).

  ENDIF.

  CHECK ziot_cl_bs_log=>gui_docking_container IS BOUND.

  CREATE OBJECT ziot_cl_bs_log=>gui_alv_grid
    EXPORTING
      i_parent = ziot_cl_bs_log=>gui_docking_container.

  DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = 'X'
                                      sel_mode   = 'D' ).
  DATA(lt_alv_fcodes_excl) = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_graph )
                                                 ( cl_gui_alv_grid=>mc_fc_info )
                                                 ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

  IF <ls_msg_details>-t_input_parameter IS NOT INITIAL.
    ls_structure_name = '/SCWM/RSRA_S_PARAMETER'.
    ASSIGN <ls_msg_details>-t_input_parameter TO <lt_outtab>.
  ELSEIF <ls_msg_details>-t_doc_selection IS NOT INITIAL.
    ls_structure_name = '/SCWM/S_MSG_SELECTION'.
    ASSIGN <ls_msg_details>-t_doc_selection TO <lt_outtab>.
  ENDIF.

  CHECK <lt_outtab> IS ASSIGNED.

  ziot_cl_bs_log=>gui_alv_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name     = ls_structure_name
      is_layout            = ls_layout
      it_toolbar_excluding = lt_alv_fcodes_excl
    CHANGING
      it_outtab            = <lt_outtab> ).

  ziot_cl_bs_log=>sel_message_param_id = lv_msg_param_id.

ENDFORM.
