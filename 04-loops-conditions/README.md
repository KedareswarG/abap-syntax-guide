# 04 — Loops & Conditions

## Quick Reference

| Operation | Old Syntax | New Syntax |
|---|---|---|
| Loop into | `LOOP AT lt INTO ls.` | `LOOP AT lt INTO DATA(ls).` |
| Loop assign | `LOOP AT lt ASSIGNING <fs>.` | `LOOP AT lt ASSIGNING FIELD-SYMBOL(<fs>).` |
| Loop with WHERE | `LOOP AT lt INTO ls WHERE id = 1.` | Same |
| Loop with GROUP BY | ❌ Not supported | `LOOP AT lt INTO DATA(ls) GROUP BY ls-dept.` |
| IF condition | `IF lv_x = 1.` | Same + `SWITCH` / `COND` |
| CASE | `CASE lv_x. WHEN 1.` | Same + inline `SWITCH #( )` |
| DO loop | `DO 5 TIMES.` | Same |
| WHILE loop | `WHILE lv_x < 10.` | Same |

---

## Old vs New — Side by Side

### Basic Loop
```abap
" ❌ Old
DATA ls_emp TYPE zs_emp.
LOOP AT lt_emp INTO ls_emp.
  WRITE ls_emp-name.
ENDLOOP.

" ✅ New
LOOP AT lt_emp INTO DATA(ls_emp).
  WRITE ls_emp-name.
ENDLOOP.
```

### Loop with Condition
```abap
" ❌ Old
LOOP AT lt_emp INTO ls_emp WHERE dept = 'IT'.
  WRITE ls_emp-name.
ENDLOOP.

" ✅ New — same but with inline declaration
LOOP AT lt_emp INTO DATA(ls_emp) WHERE dept = 'IT'.
  WRITE ls_emp-name.
ENDLOOP.
```

### IF with COND (New)
```abap
" ❌ Old
DATA lv_status TYPE string.
IF lv_score >= 90.
  lv_status = 'Excellent'.
ELSEIF lv_score >= 70.
  lv_status = 'Good'.
ELSE.
  lv_status = 'Average'.
ENDIF.

" ✅ New — COND operator
DATA(lv_status) = COND string(
  WHEN lv_score >= 90 THEN 'Excellent'
  WHEN lv_score >= 70 THEN 'Good'
  ELSE 'Average'
).
```

### CASE with SWITCH (New)
```abap
" ❌ Old
DATA lv_label TYPE string.
CASE lv_type.
  WHEN 'A'. lv_label = 'Active'.
  WHEN 'I'. lv_label = 'Inactive'.
  WHEN OTHERS. lv_label = 'Unknown'.
ENDCASE.

" ✅ New — SWITCH operator
DATA(lv_label) = SWITCH string( lv_type
  WHEN 'A' THEN 'Active'
  WHEN 'I' THEN 'Inactive'
  ELSE 'Unknown'
).
```

### GROUP BY (Modern ABAP — New Only)
```abap
" ✅ Group employees by department
LOOP AT lt_emp INTO DATA(ls_emp)
  GROUP BY ls_emp-dept
  INTO DATA(lv_group).

  WRITE |Dept: { lv_group }|.

  LOOP AT GROUP lv_group INTO DATA(ls_member).
    WRITE ls_member-name.
  ENDLOOP.
ENDLOOP.
```

### FOR Loop — Table Filling (New Only)
```abap
" ✅ Fill a table using FOR loop inline
DATA(lt_names) = VALUE string_table(
  FOR ls_emp IN lt_emp
  WHERE ( dept = 'IT' )
  ( ls_emp-name )
).
```

---

## 💡 Key Takeaways
- Use **COND** instead of IF-ELSEIF chains for value assignment — cleaner one-liners
- Use **SWITCH** instead of CASE for value mapping
- Use **GROUP BY** in loops to avoid manual grouping logic
- Use **FOR** operator to filter and fill tables inline — replaces LOOP + APPEND pattern
