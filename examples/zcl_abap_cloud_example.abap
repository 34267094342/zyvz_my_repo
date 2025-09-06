*"* This example demonstrates cloud-ready ABAP development practices
*"* Using only released APIs and modern ABAP syntax

CLASS zcl_abap_cloud_example DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Public methods demonstrating cloud-ready patterns
    METHODS: 
      " System information using XCO library
      get_system_info RETURNING VALUE(rs_info) TYPE string,
      
      " Random number generation using released APIs
      generate_random_number 
        IMPORTING iv_min TYPE i DEFAULT 1
                  iv_max TYPE i DEFAULT 100
        RETURNING VALUE(rv_number) TYPE i,
      
      " Date/time operations using XCO
      get_formatted_datetime RETURNING VALUE(rv_datetime) TYPE string,
      
      " Data access using CDS entities (example)
      get_timezone_info 
        IMPORTING iv_timezone_id TYPE string DEFAULT 'UTC'
        RETURNING VALUE(rs_timezone) TYPE string.

  PRIVATE SECTION.
    " Private methods for internal operations
    METHODS: validate_input 
               IMPORTING iv_min TYPE i
                         iv_max TYPE i
               RETURNING VALUE(rv_valid) TYPE abap_bool.

ENDCLASS.

CLASS zcl_abap_cloud_example IMPLEMENTATION.

  METHOD get_system_info.
    " Using XCO library to get system information (released API)
    DATA(current_user) = xco_cp=>sy->user( )->value.
    DATA(current_date) = xco_cp=>sy->date( 
      xco_cp_time=>time_zone->user 
    )->as( xco_cp_time=>format->iso_8601_extended )->value.
    
    " Construct response string
    rs_info = |Current User: { current_user }, Date: { current_date }|.
  ENDMETHOD.

  METHOD generate_random_number.
    " Validate input parameters
    IF validate_input( iv_min = iv_min iv_max = iv_max ) = abap_false.
      RETURN.
    ENDIF.
    
    " Using released API for random number generation
    rv_number = cl_abap_random_int=>create( 
      seed = cl_abap_random=>seed( )
      min  = iv_min
      max  = iv_max 
    )->get_next( ).
  ENDMETHOD.

  METHOD get_formatted_datetime.
    " Get current date and time using XCO library
    DATA(user_date) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
                        )->as( xco_cp_time=>format->iso_8601_extended
                        )->value.
    
    DATA(user_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                        )->as( xco_cp_time=>format->iso_8601_extended
                        )->value.
    
    rv_datetime = |{ user_date }T{ user_time }|.
  ENDMETHOD.

  METHOD get_timezone_info.
    " Example of using a released CDS entity
    " Note: Actual implementation would depend on available CDS entities
    TRY.
        SELECT SINGLE TimeZoneID, Description 
          FROM I_TimeZone 
          WHERE TimeZoneID = @iv_timezone_id
          INTO @DATA(ls_timezone).
        
        IF sy-subrc = 0.
          rs_timezone = |Timezone: { ls_timezone-TimeZoneID }, Description: { ls_timezone-Description }|.
        ELSE.
          rs_timezone = |Timezone { iv_timezone_id } not found|.
        ENDIF.
        
      CATCH cx_sy_open_sql_db.
        rs_timezone = |Error accessing timezone information|.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_input.
    " Simple validation logic
    rv_valid = COND #( WHEN iv_min < iv_max THEN abap_true ELSE abap_false ).
  ENDMETHOD.

ENDCLASS.