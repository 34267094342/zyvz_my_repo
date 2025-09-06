# ABAP Cloud Development - Quick Reference

## Essential APIs and Patterns

### System Information (XCO Library)
```abap
" Current user
DATA(user) = xco_cp=>sy->user( )->value.

" Current date/time
DATA(date) = xco_cp=>sy->date( xco_cp_time=>time_zone->user )->value.
DATA(time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->value.

" Formatted date/time
DATA(iso_date) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
                   )->as( xco_cp_time=>format->iso_8601_extended )->value.
```

### Random Number Generation
```abap
" Random integer
DATA(random_int) = cl_abap_random_int=>create(
  seed = cl_abap_random=>seed( )
  min  = 1
  max  = 100
)->get_next( ).
```

### Data Access Patterns
```abap
" Use CDS entities instead of direct table access
SELECT SINGLE * FROM I_TimeZone
  WHERE TimeZoneID = 'UTC'
  INTO @DATA(timezone_info).

" Proper error handling
TRY.
    " Database operations
  CATCH cx_sy_open_sql_db INTO DATA(sql_exception).
    " Handle database errors
ENDTRY.
```

### Modern ABAP Syntax
```abap
" Inline declarations
DATA(result) = method_call( ).

" Constructor expressions
DATA(list) = VALUE string_table( ( 'item1' ) ( 'item2' ) ).

" Conditional expressions
DATA(status) = COND string( WHEN sy-subrc = 0 THEN 'SUCCESS' ELSE 'ERROR' ).

" String templates
DATA(message) = |Hello { user }, today is { date }|.
```

## Do's and Don'ts

### ✅ DO Use
- Released APIs only
- XCO library for system operations
- CDS entities for data access
- Modern ABAP syntax
- Proper exception handling
- RAP for transactional scenarios

### ❌ DON'T Use
- Direct table access (non-released)
- Classic dynpro
- SAP GUI transactions
- Non-released system classes
- Classic list processing (WRITE statements)
- Client-dependent operations

## Cloud Readiness Validation
```bash
# In ADT: Right-click → Run As → ABAP Test Cockpit With...
# Check variant: ABAP_CLOUD_READINESS
```

## Quick Migration Checklist
- [ ] Replace direct table access with CDS entities
- [ ] Use XCO library for system information
- [ ] Update to released APIs
- [ ] Remove classic UI elements
- [ ] Implement proper error handling
- [ ] Add unit tests
- [ ] Run ATC cloud readiness check
- [ ] Switch ABAP language version
- [ ] Test in cloud environment