# 01 — Data Declarations

## 1. Elementary Variables

| | Old Syntax | New Syntax |
|---|---|---|
| Variable | `DATA lv_name TYPE string.` | Same, but preferred inline |
| Inline | ❌ Not supported | `DATA(lv_name) = 'Kedar'.` |
| Constant | `CONSTANTS lc_max TYPE i VALUE 100.` | Same |
| Field Symbol | `FIELD-SYMBOLS <fs> TYPE any.` | Same |
| Reference | `DATA lr_obj TYPE REF TO zcl_class.` | Same |

---

## 2. Old vs New — Side by Side

### Variable Declaration
```abap
" ❌ Old
DATA lv_name TYPE string.
lv_name = 'Kedareswar'.

" ✅ New (Inline Declaration)
DATA(lv_name) = 'Kedareswar'.
```

### Loop with Field Symbol
```abap
" ❌ Old
FIELD-SYMBOLS <fs_line> TYPE zs_employee.
LOOP AT lt_employees ASSIGNING <fs_line>.
  WRITE <fs_line>-name.
ENDLOOP.

" ✅ New
LOOP AT lt_employees ASSIGNING FIELD-SYMBOL(<fs_line>).
  WRITE <fs_line>-name.
ENDLOOP.
```

### Reference Assignment
```abap
" ❌ Old
DATA lr_employee TYPE REF TO zs_employee.
CREATE OBJECT lr_employee.

" ✅ New
DATA(lr_employee) = NEW zs_employee( ).
```

---

## 💡 Key Takeaways
- Use **inline declarations** `DATA(var)` wherever possible — cleaner and less boilerplate
- Use `FIELD-SYMBOL` inline in loops — avoids separate declaration
- Use `NEW` instead of `CREATE OBJECT` for modern OOP style
