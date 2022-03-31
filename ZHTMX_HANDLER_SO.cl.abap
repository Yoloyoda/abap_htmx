CLASS ZHTMX_HANDLER_SO DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES IF_HTTP_EXTENSION.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF TYP_W_SAP_TABLES,
        VBELN  TYPE VBAK-VBELN,
        AUART  TYPE VBAK-AUART,
        VKORG  TYPE VBAK-VKORG,
        POSNR  TYPE VBAP-POSNR,
        MATNR  TYPE VBAP-MATNR,
        KWMENG TYPE VBAP-KWMENG,
        VRKME  TYPE VBAP-VRKME,
      END OF TYP_W_SAP_TABLES,

      TYP_IT_SAP_TABLES TYPE STANDARD TABLE OF TYP_W_SAP_TABLES WITH EMPTY KEY,

      BEGIN OF TYP_W_DATA_STRING,
        VBELN  TYPE STRING,
        AUART  TYPE STRING,
        VKORG  TYPE STRING,
        POSNR  TYPE STRING,
        MATNR  TYPE STRING,
        KWMENG TYPE STRING,
        VRKME  TYPE STRING,
      END OF TYP_W_DATA_STRING,

      TYP_IT_BAPIRET2 TYPE STANDARD TABLE OF BAPIRET2 WITH EMPTY KEY,
      TYP_R_AUART     TYPE RANGE OF AUART,
      TYP_R_VKORG     TYPE RANGE OF VKORG.


    DATA:GV_PATH            TYPE STRING,
         GV_SO_KEY       TYPE STRING,
         GV_STATUS_MSG   TYPE STRING,
         GT_FORM         TYPE TIHTTPNVP,
         GV_AUART        TYPE AUART,
         GV_VKORG        TYPE VKORG,
         GT_RETURN       TYPE TYP_IT_BAPIRET2,
         GFLG_ERROR      TYPE FLAG,
         GV_PAGE            TYPE I.

    METHODS GET_SELECTION_PARAMETER IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                                    CHANGING  AUART       TYPE TYP_R_AUART
                                              VKORG       TYPE TYP_R_VKORG.

    METHODS HTML_CANCEL_EDIT  IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                              RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_EDIT_ROWS  IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                            RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_PAGE RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_SAVE_EDIT IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                           EXPORTING STATUS_MSG  TYPE STRING
                           RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_SHELLBAR RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_SELECTION RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_TABLE  IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                        RETURNING VALUE(HTML) TYPE STRING.

    METHODS HTML_TABLE_ROWS IMPORTING VALUE(FORM) TYPE TIHTTPNVP
                            RETURNING VALUE(HTML) TYPE STRING.

    METHODS SAP_TABLE_GETCOUNT IMPORTING VALUE(FORM)  TYPE TIHTTPNVP
                               RETURNING VALUE(COUNT) TYPE I.

    METHODS SAP_TABLE_GETLIST  IMPORTING VALUE(FORM)       TYPE TIHTTPNVP
                               RETURNING VALUE(SAP_TABLES) TYPE TYP_IT_SAP_TABLES.

    METHODS TAB_ROW_EDIT_MODE IMPORTING DATA       TYPE TYP_W_DATA_STRING
                                        STATUS_MSG TYPE STRING
                              CHANGING  HTML       TYPE STRING.

    METHODS TAB_ROW_DISP_MODE IMPORTING DATA       TYPE TYP_W_DATA_STRING
                                        STATUS_MSG TYPE STRING
                              CHANGING  HTML       TYPE STRING.

    METHODS CONV_VAL_EXT      IMPORTING DATA        TYPE TYP_W_SAP_TABLES
                              CHANGING  DATA_STRING TYPE TYP_W_DATA_STRING.

    METHODS UPDATE_DB_SO IMPORTING DATA   TYPE TYP_W_DATA_STRING
                         EXPORTING RETURN TYPE TYP_IT_BAPIRET2
                                   ERROR  TYPE FLAG.

    METHODS SEL_DB_SO_SINGLE IMPORTING VBELN           TYPE VBELN
                                       POSNR           TYPE POSNR
                             RETURNING VALUE(DATA_OUT) TYPE TYP_W_SAP_TABLES.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZHTMX_HANDLER_SO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->CONV_VAL_EXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        TYP_W_SAP_TABLES
* | [<-->] DATA_STRING                    TYPE        TYP_W_DATA_STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONV_VAL_EXT.
    DATA_STRING-VBELN = |{ DATA-VBELN ALPHA = OUT }|.
    DATA_STRING-POSNR = |{ DATA-POSNR ALPHA = OUT }|.

    CALL FUNCTION 'CONVERSION_EXIT_AUART_OUTPUT'
      EXPORTING
        INPUT  = DATA_STRING-AUART
      IMPORTING
        OUTPUT = DATA_STRING-AUART.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        INPUT  = DATA_STRING-MATNR
      IMPORTING
        OUTPUT = DATA_STRING-MATNR.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = DATA_STRING-VRKME
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = DATA_STRING-VRKME
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->GET_SELECTION_PARAMETER
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-->] AUART                          TYPE        TYP_R_AUART
* | [<-->] VKORG                          TYPE        TYP_R_VKORG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_SELECTION_PARAMETER.
    DATA:LV_AUART TYPE AUART,
         LV_VKORG TYPE VKORG.
    FIELD-SYMBOLS:
      <LW_R_AUART> TYPE LINE OF TYP_R_AUART,
      <LW_R_VKORG> TYPE LINE OF TYP_R_VKORG.

    "Get selection paramters
    LOOP AT FORM ASSIGNING FIELD-SYMBOL(<LW_FORM>)
      WHERE VALUE IS NOT INITIAL.
      CASE <LW_FORM>-NAME.
        WHEN 'document type'.

          "Convert to internal value
          CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
            EXPORTING
              INPUT  = <LW_FORM>-VALUE
            IMPORTING
              OUTPUT = LV_AUART.

          GV_AUART = LV_AUART.
          APPEND INITIAL LINE TO AUART ASSIGNING <LW_R_AUART>.
          <LW_R_AUART> = VALUE #( SIGN   = 'I'
                                  OPTION = 'EQ'
                                  LOW    = LV_AUART ).

        WHEN 'sales org'.

          GV_VKORG = <LW_FORM>-VALUE.
          APPEND INITIAL LINE TO VKORG ASSIGNING <LW_R_VKORG>.
          <LW_R_VKORG> = VALUE #( SIGN   = 'I'
                                  OPTION = 'EQ'
                                  LOW    = <LW_FORM>-VALUE ).

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    "If there is no form data, check selection parameter values passed on the query string
    IF GV_AUART IS NOT INITIAL
      AND AUART IS INITIAL.

      APPEND INITIAL LINE TO AUART ASSIGNING <LW_R_AUART>.
      <LW_R_AUART> = VALUE #( SIGN   = 'I'
                              OPTION = 'EQ'
                              LOW    = GV_AUART ).
    ENDIF.

    IF GV_VKORG IS NOT INITIAL
      AND VKORG IS INITIAL.

      APPEND INITIAL LINE TO VKORG ASSIGNING <LW_R_VKORG>.
      <LW_R_VKORG> = VALUE #( SIGN   = 'I'
                              OPTION = 'EQ'
                              LOW    = GV_VKORG ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_CANCEL_EDIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_CANCEL_EDIT.

    DATA:LW_DATA_STRING TYPE TYP_W_DATA_STRING,
         LV_VBELN       TYPE VBELN,
         LV_POSNR       TYPE POSNR.

    IF STRLEN( GV_SO_KEY ) >= 16.
      "Zero padding(convert to internal)
      LV_VBELN = GV_SO_KEY+0(10).
      LV_VBELN = |{ LV_VBELN ALPHA = IN }|.
      LV_POSNR = GV_SO_KEY+10(6).
    ENDIF.

    DATA(LW_SAP_TABLE) = SEL_DB_SO_SINGLE( EXPORTING VBELN  = LV_VBELN
                                                     POSNR  = LV_POSNR  ).

    MOVE-CORRESPONDING LW_SAP_TABLE TO LW_DATA_STRING.

    "Convert to external
    CONV_VAL_EXT( EXPORTING DATA              = LW_SAP_TABLE
                  CHANGING  DATA_STRING       = LW_DATA_STRING ).

    "Append the row in display mode
    TAB_ROW_DISP_MODE( EXPORTING DATA       = LW_DATA_STRING
                                 STATUS_MSG = ''
                       CHANGING  HTML       = HTML ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_EDIT_ROWS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_EDIT_ROWS.

    DATA:LW_DATA_STRING TYPE TYP_W_DATA_STRING,
         LV_VBELN       TYPE VBELN,
         LV_POSNR       TYPE POSNR.

    IF STRLEN( GV_SO_KEY ) >= 16.
      "Zero padding(convert to internal)
      LV_VBELN = GV_SO_KEY+0(10).
      LV_VBELN = |{ LV_VBELN ALPHA = IN }|.
      LV_POSNR = GV_SO_KEY+10(6).
    ENDIF.

    DATA(LW_SAP_TABLE) = SEL_DB_SO_SINGLE( EXPORTING VBELN  = LV_VBELN
                                                     POSNR  = LV_POSNR  ).

    MOVE-CORRESPONDING LW_SAP_TABLE TO LW_DATA_STRING.

    "Convert to external
    CONV_VAL_EXT( EXPORTING DATA              = LW_SAP_TABLE
                  CHANGING  DATA_STRING       = LW_DATA_STRING ).

    "Append the row in edit mode
    TAB_ROW_EDIT_MODE( EXPORTING DATA       = LW_DATA_STRING
                                 STATUS_MSG = ''
                       CHANGING  HTML       = HTML ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_PAGE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_PAGE.

    CONCATENATE `https://sap.github.io/fundamental-styles/`
                `theming-base-content/content/Base/baseLib/baseTheme/fonts`
                INTO DATA(FONTS_URL).

    CONCATENATE
    `<!DOCTYPE html>`
    `<html lang="en-US">`
      `<head>`
        `<meta charset="utf-8">`
        `<meta name="viewport" content="width=device-width, initial-scale=1">`
        `<title>` 'Sales order update app'(tfi) `</title>`
        `<link rel="icon" href="https://www.sap.com/favicon.ico">`
        `<link href='https://unpkg.com/fundamental-styles@0.22.0/dist/fundamental-styles.css' rel='stylesheet'>`
*        `<link rel="stylesheet" href="https://unpkg.com/fundamental-styles`
*                                     `@latest/dist/fundamental-styles.css" >`
        `<style>`
          `@font-face { font-family: '72'; `
                       `src: url('` FONTS_URL `/72-Regular.woff') format('woff');`
                       `font-weight: normal; font-style: normal;}`
          `@font-face { font-family: '72'; `
                       `src: url('` FONTS_URL `/72-Bold.woff') format('woff');`
                       `font-weight: 700; font-style: normal;}`
          `@font-face { font-family: 'SAP-icons'; `
                       `src: url('` FONTS_URL `/SAP-icons.woff') format('woff');`
                       `font-weight: normal; font-style: normal;}`
        `</style>`
        `<script src="https://unpkg.com/htmx.org@latest/dist/htmx.js"></script>`
        `<meta name="htmx-config" content='{"defaultSwapStyle":"outerHTML"}'>`
      `</head>`
      `<body>` INTO HTML.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_SAVE_EDIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<---] STATUS_MSG                     TYPE        STRING
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_SAVE_EDIT.

    DATA:LW_DATA_STRING     TYPE TYP_W_DATA_STRING,
         LW_DATA_STRING_NEW TYPE TYP_W_DATA_STRING,
         LV_VBELN           TYPE VBELN,
         LV_POSNR           TYPE POSNR,
         LV_MSG             TYPE STRING,
         LV_HTML_MSG        TYPE STRING.

    GV_PAGE = COND #( WHEN GV_PAGE IS INITIAL THEN 1 ELSE GV_PAGE ).

    DATA(NEXT_PAGE) = |{ GV_PAGE + 1 }|.

    "Get the input data
    LOOP AT FORM ASSIGNING FIELD-SYMBOL(<LW_FORM>).
      CASE <LW_FORM>-NAME.
        WHEN 'auart'.
          LW_DATA_STRING-AUART  = <LW_FORM>-VALUE.
        WHEN 'vkorg'.
          LW_DATA_STRING-VKORG  = <LW_FORM>-VALUE.
        WHEN 'matnr'.
          LW_DATA_STRING-MATNR = <LW_FORM>-VALUE.
        WHEN 'kwmeng'.
          LW_DATA_STRING-KWMENG = <LW_FORM>-VALUE.
        WHEN 'vrkme'.
          LW_DATA_STRING-VRKME  = <LW_FORM>-VALUE.
        WHEN 'k'.
          IF STRLEN( <LW_FORM>-VALUE ) >= 16.
            "Zero padding(convert to internal)
            LV_VBELN = GV_SO_KEY+0(10).
            LV_VBELN = |{ LV_VBELN ALPHA = IN }|.
            LV_POSNR = GV_SO_KEY+10(6).

            LW_DATA_STRING-VBELN = LV_VBELN.
            LW_DATA_STRING-POSNR = LV_POSNR.
            GV_SO_KEY = LW_DATA_STRING-VBELN && LW_DATA_STRING-POSNR.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    "Call FM to update sales order
    UPDATE_DB_SO( EXPORTING DATA = LW_DATA_STRING
                  IMPORTING RETURN = GT_RETURN
                            ERROR  = GFLG_ERROR
                     ).

    "Get the new value
    DATA(LW_SAP_TABLE_NEW) = SEL_DB_SO_SINGLE( EXPORTING VBELN  = LV_VBELN
                                                         POSNR  = LV_POSNR  ).
    MOVE-CORRESPONDING LW_SAP_TABLE_NEW TO LW_DATA_STRING_NEW.

    "Convert to external
    CONV_VAL_EXT( EXPORTING DATA              = LW_SAP_TABLE_NEW
                  CHANGING  DATA_STRING       = LW_DATA_STRING_NEW ).


    IF GFLG_ERROR IS NOT INITIAL.
      "Create error message
      LOOP AT GT_RETURN ASSIGNING FIELD-SYMBOL(<LW_RETURN>)
        WHERE TYPE = 'E' OR TYPE = 'A'.

        CLEAR:LV_MSG.

        MESSAGE
             ID <LW_RETURN>-ID
           TYPE <LW_RETURN>-TYPE
         NUMBER <LW_RETURN>-NUMBER
           WITH <LW_RETURN>-MESSAGE_V1
                <LW_RETURN>-MESSAGE_V2
                <LW_RETURN>-MESSAGE_V3
                <LW_RETURN>-MESSAGE_V4
           INTO LV_MSG.

        CONCATENATE LV_MSG
                    STATUS_MSG
               INTO STATUS_MSG SEPARATED BY '. '.
      ENDLOOP.

      CONCATENATE
      `<td class="fd-table__cell">` STATUS_MSG `</td>`
      `</td>` INTO LV_HTML_MSG.

      "Append the row in edit mode
      TAB_ROW_EDIT_MODE( EXPORTING DATA       = LW_DATA_STRING_NEW
                                   STATUS_MSG = LV_HTML_MSG
                         CHANGING  HTML       = HTML ).

    ELSE.

      LV_HTML_MSG = 'Updated succesfully!'.

      "Append the row in display mode
      TAB_ROW_DISP_MODE( EXPORTING DATA       = LW_DATA_STRING_NEW
                                   STATUS_MSG = LV_HTML_MSG
                         CHANGING  HTML       = HTML ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_SELECTION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_SELECTION.

    CONCATENATE
     `<form class="fd-form__item">`
       `<label class="fd-form__label" for="auart"> Document type </label>`
       `<input id="auart" class="fd-input fd-input-group__input" style="max-width: 100px;" type="text" name="Document type" >`
       `<label class="fd-form__label" for="vkorg"> Sales Org </label>`
       `<input id="vkorg" class="fd-input fd-input-group__input" style="max-width: 100px;" type="text" name="Sales Org" >`
         `<button type="button" class="fd-button" `
                  `aria-label="clear search" `
                  `data-hx-get="` GV_PATH `" `
                  `data-hx-headers='{"app-action": "search_init"}'>`
            `<i class="sap-icon--clear-all"></i>`
          `</button>`
         `<button type="button" class="fd-button" `
                  `aria-label="saerch" `
                  `data-hx-target="#sap_tables" `
                  `data-hx-swap-oob="true" `
                  `data-hx-put="` GV_PATH `" `
                  `data-hx-headers='{"app-action": "search"}'>`
            `<i class="sap-icon--search"></i>`
          `</button>`
     `</form>`
    INTO HTML.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_SHELLBAR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_SHELLBAR.

    CONCATENATE
    `<div style="height:45px">`
      `<div class="fd-shellbar">`
        `<div class="fd-shellbar__group fd-shellbar__group--product">`
          `<span class="fd-shellbar__logo">`
            `<img src="https://unpkg.com/fundamental-styles/dist/images/sap-logo@4x.png" `
                 `width="48" height="24" alt="logo">`
          `</span>`
          `<span class="fd-shellbar__title">` 'Sales Order update app'(tfi) `</span>`
        `</div>`
      `</div>`
    `</div>` INTO HTML.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_TABLE.

    DATA(TABLE_COUNT) = |{ SAP_TABLE_GETCOUNT( FORM = FORM ) NUMBER = USER }|.
    DATA(TABLE_ROWS) = HTML_TABLE_ROWS( FORM = FORM ).

    CONCATENATE
    `<table id="sap_tables" `
           `class="fd-table fd-table--responsive fd-table--no-horizontal-borders `
                  `fd-table--compact">`
      `<thead class="fd-table__header">`
        `<tr class="fd-table__row">`
          `<th class="fd-table__cell" scope="col">`
            'Sales Order No.'(tab)
            `<span class="fd-info-label fd-info-label--numeric `
                         `fd-info-label--accent-color-6" style="margin:0.2rem">`
              `<span class="fd-info-label__text">` TABLE_COUNT `</span>`
            `</span>`
          `</th>`
          `<th class="fd-table__cell" scope="col">` 'Document type' `</th>`
          `<th class="fd-table__cell" scope="col">` 'Sales Org' `</th>`
          `<th class="fd-table__cell" scope="col">` 'Item No.'(des) `</th>`
          `<th class="fd-table__cell" scope="col">` 'Material' `</th>`
          `<th class="fd-table__cell" scope="col">` 'Quantity' `</th>`
          `<th class="fd-table__cell" scope="col">` 'UoM' `</th>`
          `<th class="fd-table__cell" scope="col">` 'Edit' `</th>`
          `<th class="fd-table__cell" scope="col">` 'Status' `</th>`
        `</tr>`
      `</thead>`
      `<tbody class="fd-table__body" hx-target="closest tr" hx-swap="outerHTML">`
         TABLE_ROWS
      `</tbody>`
    `</table>` INTO HTML.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->HTML_TABLE_ROWS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HTML_TABLE_ROWS.

    DATA:LW_DATA_STRING TYPE TYP_W_DATA_STRING.

    GV_PAGE = COND #( WHEN GV_PAGE IS INITIAL THEN 1 ELSE GV_PAGE ).

    DATA(SAP_TABLES) = SAP_TABLE_GETLIST( FORM = FORM ).

    IF LINES( SAP_TABLES ) > 0.

      DATA(NEXT_PAGE) = |{ GV_PAGE + 1 }|.

      CONCATENATE
      `<tr data-hx-trigger="revealed" `
          `data-hx-get="` GV_PATH `" `
          `data-hx-vals='{"a": "` GV_AUART `", "v": "` GV_VKORG `" }' `
          `data-hx-headers='{"app-action": "scroll", "app-page": "` NEXT_PAGE `"}' `
          `data-hx-target="closest tbody" `
          `data-hx-swap="beforeend">`
        `<td colspan="2"></td>`
      `</tr>` INTO HTML.

      LOOP AT SAP_TABLES INTO DATA(SAP_TABLE).
        MOVE-CORRESPONDING SAP_TABLE TO LW_DATA_STRING.

        "Convert to external
        CONV_VAL_EXT( EXPORTING DATA              = SAP_TABLE
                      CHANGING  DATA_STRING       = LW_DATA_STRING ).

        GV_SO_KEY = LW_DATA_STRING-VBELN && LW_DATA_STRING-POSNR.

        "Append the row in display mode
        TAB_ROW_DISP_MODE( EXPORTING DATA       = LW_DATA_STRING
                                     STATUS_MSG = ''
                           CHANGING  HTML       = HTML ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZHTMX_HANDLER_SO->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.

    "Get Request parameters
    ME->GV_PATH = SERVER->REQUEST->GET_HEADER_FIELD(
                 IF_HTTP_HEADER_FIELDS_SAP=>PATH_TRANSLATED_EXPANDED ).

    "Get the query value Order type
    ME->GV_AUART = ESCAPE( VAL = SERVER->REQUEST->GET_FORM_FIELD( `a` )
                                  FORMAT = CL_ABAP_FORMAT=>E_HTML_TEXT ).
    "Get the query value Sales Org
    ME->GV_VKORG = ESCAPE( VAL = SERVER->REQUEST->GET_FORM_FIELD( `v` )
                                  FORMAT = CL_ABAP_FORMAT=>E_HTML_TEXT ).

    "Get the SO number user is operating
    ME->GV_SO_KEY = ESCAPE( VAL = SERVER->REQUEST->GET_FORM_FIELD( `k` )
                                  FORMAT = CL_ABAP_FORMAT=>E_HTML_TEXT ).

    "Get form data
    CALL METHOD SERVER->REQUEST->GET_FORM_FIELDS( CHANGING FIELDS = GT_FORM ).


    "Get method
    IF SERVER->REQUEST->GET_HEADER_FIELD(
         IF_HTTP_HEADER_FIELDS_SAP=>REQUEST_METHOD ) = `GET`.

      IF SERVER->REQUEST->GET_HEADER_FIELD( `hx-request` ) IS INITIAL.

        SERVER->RESPONSE->APPEND_CDATA( HTML_PAGE( ) ).
        SERVER->RESPONSE->APPEND_CDATA( HTML_SHELLBAR( ) ).
        SERVER->RESPONSE->APPEND_CDATA( HTML_SELECTION( ) ).
        SERVER->RESPONSE->APPEND_CDATA( HTML_TABLE( EXPORTING FORM = GT_FORM ) ).

      ELSE.

        CASE SERVER->REQUEST->GET_HEADER_FIELD( `app-action` ).

          WHEN `search_init`.
            "Implement Search Init logic of your choice

          WHEN `scroll`.

            GV_PAGE = SERVER->REQUEST->GET_HEADER_FIELD( `app-page` ).
            SERVER->RESPONSE->APPEND_CDATA( HTML_TABLE_ROWS( EXPORTING FORM = GT_FORM ) ).

          WHEN `edit`.
            SERVER->RESPONSE->APPEND_CDATA( HTML_EDIT_ROWS( EXPORTING FORM = GT_FORM ) ).

          WHEN `cancel`.
            SERVER->RESPONSE->APPEND_CDATA( HTML_CANCEL_EDIT( EXPORTING FORM = GT_FORM ) ).

        ENDCASE.

        DATA(LV_ACTION) = SERVER->REQUEST->GET_HEADER_FIELD( `app-action` ).

      ENDIF.

      SERVER->RESPONSE->SET_STATUS( CODE = 200
                                    REASON = IF_HTTP_STATUS=>REASON_200 ).
      SERVER->RESPONSE->SET_CONTENT_TYPE( `text/html` ).

    ENDIF.

    "Put Method
    IF SERVER->REQUEST->GET_HEADER_FIELD(
               IF_HTTP_HEADER_FIELDS_SAP=>REQUEST_METHOD ) = `PUT`.
      CASE SERVER->REQUEST->GET_HEADER_FIELD( `app-action` ).

        WHEN `search`.
          SERVER->RESPONSE->APPEND_CDATA( HTML_TABLE( EXPORTING FORM = GT_FORM ) ).

        WHEN `save`.
          SERVER->RESPONSE->APPEND_CDATA( HTML_SAVE_EDIT( EXPORTING FORM = GT_FORM
                                                          IMPORTING STATUS_MSG = GV_STATUS_MSG ) ).
      ENDCASE.

      SERVER->RESPONSE->SET_STATUS( CODE = 200
                                    REASON = IF_HTTP_STATUS=>REASON_200 ).
      SERVER->RESPONSE->SET_CONTENT_TYPE( `text/html` ).
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->SAP_TABLE_GETCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] COUNT                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SAP_TABLE_GETCOUNT.
    DATA:LT_R_AUART TYPE TYP_R_AUART,
         LT_R_VKORG TYPE TYP_R_VKORG.

    GET_SELECTION_PARAMETER( EXPORTING FORM  = FORM
                             CHANGING  AUART = LT_R_AUART
                                       VKORG = LT_R_VKORG ).

    SELECT COUNT( DISTINCT VBAP~VBELN )
      FROM VBAK
     INNER JOIN VBAP
        ON VBAP~VBELN = VBAK~VBELN
     WHERE VBAK~AUART IN @LT_R_AUART
       AND VBAK~VKORG IN @LT_R_VKORG
      INTO @COUNT.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->SAP_TABLE_GETLIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM                           TYPE        TIHTTPNVP
* | [<-()] SAP_TABLES                     TYPE        TYP_IT_SAP_TABLES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SAP_TABLE_GETLIST.
    DATA:LT_R_AUART TYPE TYP_R_AUART,
         LT_R_VKORG TYPE TYP_R_VKORG.

    GET_SELECTION_PARAMETER( EXPORTING FORM  = FORM
                             CHANGING  AUART = LT_R_AUART
                                       VKORG = LT_R_VKORG ).

    "Display 100 rows each. User has to scroll down to get the next 100
    DATA(OFFSET) = 100 * ( GV_PAGE - 1 ).

    SELECT VBAK~VBELN,
           VBAK~AUART,
           VBAK~VKORG,
           VBAP~POSNR,
           VBAP~MATNR,
           VBAP~KWMENG,
           VBAP~VRKME
      FROM VBAK
     INNER JOIN VBAP
        ON VBAP~VBELN = VBAK~VBELN
     WHERE VBAK~AUART IN @LT_R_AUART
       AND VBAK~VKORG IN @LT_R_VKORG
     ORDER BY
           VBAK~VBELN,
           VBAP~POSNR
      INTO CORRESPONDING FIELDS OF TABLE @SAP_TABLES
     UP TO 100 ROWS
     OFFSET @OFFSET.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->SEL_DB_SO_SINGLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBELN                          TYPE        VBELN
* | [--->] POSNR                          TYPE        POSNR
* | [<-()] DATA_OUT                       TYPE        TYP_W_SAP_TABLES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SEL_DB_SO_SINGLE.

    SELECT SINGLE
           VBAK~VBELN,
           VBAK~AUART,
           VBAK~VKORG,
           VBAP~POSNR,
           VBAP~MATNR,
           VBAP~KWMENG,
           VBAP~VRKME
      FROM VBAK
     INNER JOIN VBAP
        ON VBAP~VBELN = VBAK~VBELN
      INTO @DATA_OUT
     WHERE VBAP~VBELN = @VBELN
       AND VBAP~POSNR = @POSNR.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->TAB_ROW_DISP_MODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        TYP_W_DATA_STRING
* | [--->] STATUS_MSG                     TYPE        STRING
* | [<-->] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD TAB_ROW_DISP_MODE.

    "Put the row to display mode
    CONCATENATE HTML
    `<tr class="fd-table__row fd-table__row--hoverable">`
      `<td class="fd-table__cell">` DATA-VBELN `</td>`
      `<td class="fd-table__cell">` DATA-AUART `</td>`
      `<td class="fd-table__cell">` DATA-VKORG `</td>`
      `<td class="fd-table__cell">` DATA-POSNR `</td>`
      `<td class="fd-table__cell">` DATA-MATNR `</td>`
      `<td class="fd-table__cell">` DATA-KWMENG `</td>`
      `<td class="fd-table__cell">` DATA-VRKME `</td>`
      `<td>`
        `<button class="btn btn-danger"`
                `data-hx-vals='{"k": "` GV_SO_KEY `"}' `
                `data-hx-get="` GV_PATH `" `
                `data-hx-headers='{"app-action": "edit"}' >`
          `Edit`
        `</button>`
      `</td>`
      `<td class="fd-table__cell fd-info-label--accent-color-6" style="margin:0.2rem"">` STATUS_MSG `</td>`
    `</tr>` INTO HTML.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->TAB_ROW_EDIT_MODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        TYP_W_DATA_STRING
* | [--->] STATUS_MSG                     TYPE        STRING
* | [<-->] HTML                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD TAB_ROW_EDIT_MODE.

    CONCATENATE HTML
      `<tr data-hx-trigger='cancel' class='editing' data-hx-get="` GV_PATH `">`
      `<td class="fd-table__cell">` DATA-VBELN `</td>`
      `<td class="fd-table__cell">` DATA-AUART `</td>`
      `<td class="fd-table__cell">` DATA-VKORG `</td>`
      `<td class="fd-table__cell">` DATA-POSNR `</td>`
      `<td class="fd-table__cell">` `<input name='matnr' value=` `'` DATA-MATNR `'` `</td>`
      `<td class="fd-table__cell">` `<input name='kwmeng' value=` `'` DATA-KWMENG `'` `</td>`
      `<td class="fd-table__cell">` `<input name='vrkme' value=` `'` DATA-VRKME `'` `</td>`
      `<td>`
      `<button class="btn btn-danger" `
                     `data-hx-vals='{"k": "` GV_SO_KEY `"}' `
                     `data-hx-get="` GV_PATH `" `
                     `data-hx-headers='{"app-action": "cancel"}' >`
        `Cancel`
      `</button>`
      `<button class="btn btn-danger"`
                     `data-hx-vals='{"k": "` GV_SO_KEY `"}' `
                     `data-hx-put="` GV_PATH `"`
                     `data-hx-headers='{"app-action": "save"}' `
                     `hx-include="closest tr">`
        `Save`
      `</button>`
    `</td>`
    `<td class="fd-table__cell fd-info-label--accent-color-6" style="margin:0.2rem"">` STATUS_MSG `</td>`
  `</tr>` INTO HTML.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZHTMX_HANDLER_SO->UPDATE_DB_SO
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        TYP_W_DATA_STRING
* | [<---] RETURN                         TYPE        TYP_IT_BAPIRET2
* | [<---] ERROR                          TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPDATE_DB_SO.

    DATA:LV_VBELN            TYPE BAPIVBELN-VBELN,
         LW_ORDER_HEADER_INX TYPE BAPISDH1X,
         LT_ORDER_ITEM_IN    TYPE STANDARD TABLE OF BAPISDITM,
         LT_ORDER_ITEM_INX   TYPE STANDARD TABLE OF BAPISDITMX,
         LT_SCHEDULE_LINES   TYPE STANDARD TABLE OF BAPISCHDL,
         LT_SCHEDULE_LINESX  TYPE STANDARD TABLE OF BAPISCHDLX.

    LW_ORDER_HEADER_INX-UPDATEFLAG = 'U'.

    APPEND INITIAL LINE TO LT_ORDER_ITEM_IN ASSIGNING FIELD-SYMBOL(<LW_ORDER_ITEM_IN>).
    <LW_ORDER_ITEM_IN>-ITM_NUMBER    = DATA-POSNR.
    <LW_ORDER_ITEM_IN>-MATERIAL_LONG = DATA-MATNR.
    <LW_ORDER_ITEM_IN>-TARGET_QU     = DATA-VRKME.

    APPEND INITIAL LINE TO LT_ORDER_ITEM_INX ASSIGNING FIELD-SYMBOL(<LW_ORDER_ITEM_INX>).
    <LW_ORDER_ITEM_INX>-ITM_NUMBER    = DATA-POSNR.
    <LW_ORDER_ITEM_INX>-UPDATEFLAG    = 'U'.
    <LW_ORDER_ITEM_INX>-MATERIAL_LONG = ABAP_TRUE.
    <LW_ORDER_ITEM_INX>-TARGET_QU     = ABAP_TRUE.

    APPEND INITIAL LINE TO LT_SCHEDULE_LINES ASSIGNING FIELD-SYMBOL(<LW_SCHEDULE_LINES>).
    <LW_SCHEDULE_LINES>-ITM_NUMBER    = DATA-POSNR.
    <LW_SCHEDULE_LINES>-REQ_QTY       = DATA-KWMENG.
    <LW_SCHEDULE_LINES>-SCHED_LINE    = '0001'.

    APPEND INITIAL LINE TO LT_SCHEDULE_LINESX ASSIGNING FIELD-SYMBOL(<LW_SCHEDULE_LINESX>).
    <LW_SCHEDULE_LINESX>-ITM_NUMBER    = DATA-POSNR.
    <LW_SCHEDULE_LINESX>-REQ_QTY       = ABAP_TRUE.
    <LW_SCHEDULE_LINESX>-UPDATEFLAG    = 'U'.
    <LW_SCHEDULE_LINESX>-SCHED_LINE    = '0001'.


    LV_VBELN = DATA-VBELN.
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        SALESDOCUMENT    = LV_VBELN
*       ORDER_HEADER_IN  =
        ORDER_HEADER_INX = LW_ORDER_HEADER_INX
*       SIMULATION       =
*       BEHAVE_WHEN_ERROR           = ' '
*       INT_NUMBER_ASSIGNMENT       = ' '
*       LOGIC_SWITCH     =
*       NO_STATUS_BUF_INIT          = ' '
      TABLES
        RETURN           = RETURN
        ORDER_ITEM_IN    = LT_ORDER_ITEM_IN
        ORDER_ITEM_INX   = LT_ORDER_ITEM_INX
*       PARTNERS         =
*       PARTNERCHANGES   =
*       PARTNERADDRESSES =
*       ORDER_CFGS_REF   =
*       ORDER_CFGS_INST  =
*       ORDER_CFGS_PART_OF          =
*       ORDER_CFGS_VALUE =
*       ORDER_CFGS_BLOB  =
*       ORDER_CFGS_VK    =
*       ORDER_CFGS_REFINST          =
        SCHEDULE_LINES   = LT_SCHEDULE_LINES
        SCHEDULE_LINESX  = LT_SCHEDULE_LINESX
*       ORDER_TEXT       =
*       ORDER_KEYS       =
*       CONDITIONS_IN    =
*       CONDITIONS_INX   =
*       EXTENSIONIN      =
*       EXTENSIONEX      =
*       NFMETALLITMS     =
      .
    IF SY-SUBRC = 0.
      IF LINE_EXISTS( RETURN[ TYPE = 'E' ] )
        OR LINE_EXISTS( RETURN[ TYPE = 'A' ] ).

        "Prompt error and rollback work
        ROLLBACK WORK.
        ERROR = ABAP_TRUE.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.

    ELSE.
      ROLLBACK WORK.
      ERROR = ABAP_TRUE.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
