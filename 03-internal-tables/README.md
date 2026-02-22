# 03 — Internal Tables

## Quick Reference

| Operation | Old Syntax | New Syntax |
|---|---|---|
| Declare Table | `DATA lt_list TYPE TABLE OF zs_emp.` | Same + inline `DATA(lt_list)` |
| Append | `APPEND ls_emp TO lt_list.` | `lt_list = VALUE #( ... ).` |
| Read | `READ TABLE lt_list INTO ls_emp WITH KEY id = 1.` | `DATA(ls_emp) = lt_list[ id = 1 ].` |
| Loop | `LOOP AT lt_list INTO ls_emp.` | `LOOP AT lt_list INTO DATA(ls_emp).` |
| Modify | `MODIFY lt_list FROM ls_emp INDEX lv_idx.` | `lt_list[ id = 1 ]-name = 'X'.` |
| Delete | `DELETE lt_list WHERE id = 1.` | Same |
| Check exists | `READ TABLE... TRANSPORTING NO FIELDS.` | `IF line_exists( lt_list[ id = 1 ] ).` |
| Count | `DESCRIBE TABLE lt_list LINES lv_cnt.` | `DATA(lv_cnt) = lines( lt_list ).` |

---

## Old vs New — Side by Side

### Filling a Table
```abap
" ❌ Old
DATA ls_emp TYPE zs_emp.
DATA lt_list TYPE TABLE OF zs_emp.
ls_emp-id   = 1.
ls_emp-name = 'Kedar'.
APPEND ls_emp TO lt_list.

" ✅ New — VALUE operator
DATA(lt_list) = VALUE ztt_emp(
  ( id = 1  name = 'Kedar' )
  ( id = 2  name = 'Arjun' )
).
```

### Reading a Single Record
```abap
" ❌ Old
DATA ls_emp TYPE zs_emp.
READ TABLE lt_list INTO ls_emp WITH KEY id = 1.
IF sy-subrc = 0.
  WRITE ls_emp-name.
ENDIF.

" ✅ New — Table Expression
TRY.
  DATA(ls_emp) = lt_list[ id = 1 ].
  WRITE ls_emp-name.
CATCH cx_sy_itab_line_not_found.
  WRITE 'Not found'.
ENDTRY.
```

### Loop
```abap
" ❌ Old
DATA ls_emp TYPE zs_emp.
LOOP AT lt_list INTO ls_emp.
  WRITE ls_emp-name.
ENDLOOP.

" ✅ New — Inline
LOOP AT lt_list INTO DATA(ls_emp).
  WRITE ls_emp-name.
ENDLOOP.

" ✅ New — Field Symbol (better performance)
LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<fs_emp>).
  <fs_emp>-name = to_upper( <fs_emp>-name ).
ENDLOOP.
```

### Check if Line Exists
```abap
" ❌ Old
READ TABLE lt_list TRANSPORTING NO FIELDS WITH KEY id = 1.
IF sy-subrc = 0.
  WRITE 'Exists'.
ENDIF.

" ✅ New
IF line_exists( lt_list[ id = 1 ] ).
  WRITE 'Exists'.
ENDIF.
```

### Count Lines
```abap
" ❌ Old
DATA lv_count TYPE i.
DESCRIBE TABLE lt_list LINES lv_count.

" ✅ New
DATA(lv_count) = lines( lt_list ).
```

### COLLECT & SORT
```abap
" Sorting — same but cleaner
SORT lt_list BY id ASCENDING name DESCENDING.

" Delete duplicates
DELETE ADJACENT DUPLICATES FROM lt_list COMPARING id.
```

---

## 💡 Key Takeaways
- Use **VALUE #( )** to fill tables inline — no need for separate work area append
- Use **table expressions** `lt_list[ key = val ]` for single reads — but always wrap in TRY-CATCH
- Use **FIELD-SYMBOL** in loops when modifying records — avoids copying data
- Use `line_exists()` and `lines()` — replaces old DESCRIBE and READ with NO FIELDS
