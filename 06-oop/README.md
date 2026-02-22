# 06 — Object Oriented ABAP (OOP)

## Quick Reference

| Concept | Old Style | Modern Style |
|---|---|---|
| Create Object | `CREATE OBJECT lr_obj.` | `DATA(lr_obj) = NEW zcl_class( ).` |
| Static Call | `zcl_class=>method( ).` | Same |
| Cast | `MOVE lr_obj TO lr_sub.` | `DATA(lr_sub) = CAST zcl_sub( lr_obj ).` |
| Check Type | `IS INSTANCE OF` | Same |
| Anonymous Object | ❌ | `NEW zcl_class( )->method( ).` |
| Constructor | `CONSTRUCTOR` method | Same |
| Interface | `INTERFACES zif_name.` | Same |
| Abstract Class | `CLASS zcl DEFINITION ABSTRACT.` | Same |

---

## Old vs New — Side by Side

### Creating an Object
```abap
" ❌ Old
DATA lr_employee TYPE REF TO zcl_employee.
CREATE OBJECT lr_employee.

" ✅ New
DATA(lr_employee) = NEW zcl_employee( ).

" ✅ With constructor parameters
DATA(lr_employee) = NEW zcl_employee(
  iv_id   = '001'
  iv_name = 'Kedar'
).
```

### Calling Methods
```abap
" ❌ Old
DATA lv_salary TYPE p DECIMALS 2.
CALL METHOD lr_employee->get_salary
  RECEIVING rv_salary = lv_salary.

" ✅ New
DATA(lv_salary) = lr_employee->get_salary( ).

" ✅ Chaining
DATA(lv_dept) = lr_employee->get_department( )->get_name( ).
```

### Anonymous Object (New Only)
```abap
" ✅ Create and use without storing reference
DATA(lv_result) = NEW zcl_calculator( )->add( iv_a = 10 iv_b = 20 ).
```

### Casting
```abap
" ❌ Old
DATA lr_animal TYPE REF TO zcl_animal.
DATA lr_dog    TYPE REF TO zcl_dog.
lr_animal = lr_dog.
MOVE lr_animal TO lr_dog.

" ✅ New
DATA(lr_dog) = CAST zcl_dog( lr_animal ).
```

### Class Definition Structure
```abap
CLASS zcl_employee DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_person.

    METHODS:
      constructor
        IMPORTING
          iv_id   TYPE string
          iv_name TYPE string,
      get_salary
        RETURNING
          VALUE(rv_salary) TYPE p DECIMALS 2,
      get_name
        RETURNING
          VALUE(rv_name) TYPE string.

  PRIVATE SECTION.
    DATA mv_id     TYPE string.
    DATA mv_name   TYPE string.
    DATA mv_salary TYPE p DECIMALS 2.

ENDCLASS.

CLASS zcl_employee IMPLEMENTATION.

  METHOD constructor.
    mv_id   = iv_id.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.

  METHOD get_salary.
    rv_salary = mv_salary.
  ENDMETHOD.

ENDCLASS.
```

### Interface
```abap
INTERFACE zif_person.
  METHODS:
    get_name RETURNING VALUE(rv_name) TYPE string,
    get_id   RETURNING VALUE(rv_id)   TYPE string.
ENDINTERFACE.
```

### Inheritance
```abap
" Base class
CLASS zcl_animal DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS speak RETURNING VALUE(rv_sound) TYPE string.
ENDCLASS.

" Child class
CLASS zcl_dog DEFINITION PUBLIC
  INHERITING FROM zcl_animal.
  PUBLIC SECTION.
    METHODS speak REDEFINITION.
ENDCLASS.

CLASS zcl_dog IMPLEMENTATION.
  METHOD speak.
    rv_sound = 'Woof!'.
  ENDMETHOD.
ENDCLASS.
```

### Exception Handling
```abap
" ✅ Modern Exception Handling
TRY.
  DATA(lr_emp) = NEW zcl_employee( iv_id = '001' iv_name = 'Kedar' ).
  DATA(lv_salary) = lr_emp->get_salary( ).

CATCH zcx_employee_not_found INTO DATA(lx_error).
  WRITE lx_error->get_text( ).

CATCH cx_root INTO DATA(lx_general).
  WRITE lx_general->get_text( ).
ENDTRY.
```

---

## 💡 Key Takeaways
- Always use **NEW** instead of `CREATE OBJECT` — cleaner and inline
- Use **CAST** instead of manual type moves
- Use **method chaining** where possible — reduces temp variables
- Always use **TRY-CATCH** with class-based exceptions — never use `sy-subrc` for OOP errors
- Prefer **interfaces** over direct inheritance for loose coupling
- Name conventions: Classes `ZCL_`, Interfaces `ZIF_`, Exceptions `ZCX_`
