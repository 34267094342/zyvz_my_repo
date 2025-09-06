# ABAP Development Guidelines

This document provides ABAP development guidelines based on the **DSAG ABAP-Leitfaden** and modern ABAP best practices.

## Table of Contents

1. [Clean ABAP Principles](#clean-abap-principles)
2. [Modern ABAP Syntax](#modern-abap-syntax)
3. [Naming Conventions](#naming-conventions)
4. [Object-Oriented Design](#object-oriented-design)
5. [Code Examples](#code-examples)

## Clean ABAP Principles

### Core Principles

Following the [SAP Clean ABAP Guidelines](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md):

- **Express yourself through code, not comments**
- **Use meaningful names**
- **Keep methods short and focused**
- **Prefer functional to procedural language constructs**
- **Use inline declarations**

### Meaningful Names

```abap
" ❌ Poor naming
DATA: lv_var TYPE string,
      lv_tab TYPE table_type.

" ✅ Clean naming  
DATA: customer_name TYPE string,
      active_orders TYPE order_table_type.
```

### Comments vs. Self-Documenting Code

```abap
" ❌ Comments explaining what code does
" Calculate total price including tax
lv_total = lv_price * ( 1 + lv_tax_rate ).

" ✅ Self-documenting code
DATA(total_price_with_tax) = calculate_price_with_tax( 
  base_price = order_price 
  tax_rate = current_tax_rate 
).
```

## Modern ABAP Syntax

### Inline Declarations

```abap
" ❌ Traditional declarations
DATA: customer_name TYPE string,
      order_count TYPE i.
customer_name = get_customer_name( customer_id ).
order_count = count_orders( customer_id ).

" ✅ Inline declarations
DATA(customer_name) = get_customer_name( customer_id ).
DATA(order_count) = count_orders( customer_id ).
```

### Functional Language Constructs

```abap
" ❌ Imperative style
TRANSLATE customer_name TO UPPER CASE.
DESCRIBE TABLE orders LINES order_count.

" ✅ Functional style
customer_name = to_upper( customer_name ).
order_count = lines( orders ).
```

### Constructor Expressions

```abap
" ❌ Traditional object creation
CREATE OBJECT order_processor TYPE zcl_order_processor.

" ✅ Modern constructor
DATA(order_processor) = NEW zcl_order_processor( ).
```

### String Templates

```abap
" ❌ String concatenation
CONCATENATE 'Order' order_id 'for customer' customer_name 
            INTO message SEPARATED BY space.

" ✅ String templates
DATA(message) = |Order { order_id } for customer { customer_name }|.
```

### Table Expressions

```abap
" ❌ Traditional table reading
READ TABLE customers WITH KEY id = customer_id INTO DATA(customer).
IF sy-subrc = 0.
  customer_name = customer-name.
ENDIF.

" ✅ Modern table expressions
DATA(customer_name) = customers[ id = customer_id ]-name.
" or with optional for safe access
DATA(customer_name) = VALUE #( customers[ id = customer_id ]-name OPTIONAL ).
```

## Naming Conventions

### Repository Objects

Use meaningful, descriptive names with appropriate prefixes:

- Classes: `ZCL_<MODULE>_<PURPOSE>` (e.g., `ZCL_SD_ORDER_PROCESSOR`)
- Interfaces: `ZIF_<MODULE>_<PURPOSE>` (e.g., `ZIF_SD_ORDER_VALIDATOR`)
- Programs: `Z<MODULE>_<PURPOSE>` (e.g., `ZSD_ORDER_REPORT`)

### Variables and Methods

```abap
" ✅ Good naming examples
DATA(is_valid_order) = validate_order( order ).
DATA(total_amount) = calculate_total( line_items ).
DATA(processed_orders) = filter_processed_orders( all_orders ).

METHOD validate_customer_data.
  " Clear purpose from method name
ENDMETHOD.

METHOD calculate_discount_percentage.
  " Returns what the name suggests
ENDMETHOD.
```

## Object-Oriented Design

### Class Structure

```abap
"! Order processing class following clean ABAP principles
"! Handles order validation, calculation, and processing
CLASS zcl_order_processor DEFINITION PUBLIC FINAL.

  PUBLIC SECTION.
    "! Process a single order
    "! @parameter order | Order to process
    "! @parameter result | Processing result
    METHODS: process_order
      IMPORTING order         TYPE zorder
      RETURNING VALUE(result) TYPE zorder_result.

  PRIVATE SECTION.
    METHODS: validate_order
      IMPORTING order         TYPE zorder
      RETURNING VALUE(result) TYPE abap_bool,
      
      calculate_totals
      IMPORTING order         TYPE zorder
      RETURNING VALUE(result) TYPE zorder_totals.

ENDCLASS.
```

### Error Handling

```abap
CLASS zcl_order_processor IMPLEMENTATION.

  METHOD process_order.
    " Use exceptions for error handling
    IF NOT validate_order( order ).
      RAISE EXCEPTION NEW zcx_invalid_order( 
        order_id = order-id 
        message = 'Order validation failed'
      ).
    ENDIF.
    
    " Process order logic here
    result = VALUE #( 
      order_id = order-id
      status = 'PROCESSED'
      timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) )
    ).
  ENDMETHOD.

ENDCLASS.
```

## Code Examples

### Modern ABAP Table Processing

```abap
METHOD process_order_items.
  " Modern table processing with VALUE expressions
  DATA(processed_items) = VALUE order_items_table( 
    FOR item IN order_items 
    WHERE ( item-status = 'ACTIVE' )
    ( VALUE #( 
        item_id = item-id
        quantity = item-quantity
        unit_price = get_current_price( item-product_id )
        total_price = item-quantity * get_current_price( item-product_id )
      ) )
  ).
  
  " Calculate order total using REDUCE
  DATA(order_total) = REDUCE currency( 
    INIT total = 0
    FOR item IN processed_items
    NEXT total = total + item-total_price 
  ).
  
ENDMETHOD.
```

### Exception Handling

```abap
METHOD get_customer_orders.
  TRY.
    DATA(customer) = customers[ id = customer_id ].
    result = get_orders_for_customer( customer ).
    
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW zcx_customer_not_found( customer_id = customer_id ).
    
  ENDTRY.
ENDMETHOD.
```

## Tools and Quality Assurance

### Recommended Tools

1. **ABAP Cleaner**: Automatic code formatting and optimization
2. **Code Pal for ABAP**: Static code analysis in ABAP Test Cockpit
3. **ABAP Doc**: Documentation generation
4. **ABAP Unit**: Unit testing framework

### Quality Gates

- All code must pass Code Pal checks
- Unit test coverage minimum 80%
- All public methods must have ABAP Doc documentation
- Code reviews mandatory for all changes

## References

- [DSAG ABAP Guidelines](https://1dsag.github.io/ABAP-Leitfaden) (German)
- [SAP Clean ABAP Guidelines](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
- [ABAP Feature Matrix](https://software-heroes.com/en/abap-feature-matrix)
- [ABAP for Cloud Development](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/19_ABAP_for_Cloud_Development.md)

---

*This document is based on the DSAG ABAP-Leitfaden by the German SAP User Group (DSAG), available under CC BY 4.0 license.*