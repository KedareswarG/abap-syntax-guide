# 02 — String Operations

## Quick Reference

| Operation | Old Syntax | New Syntax |
|---|---|---|
| Concatenate | `CONCATENATE s1 s2 INTO lv_result.` | `lv_result = s1 && s2.` |
| Length | `lv_len = STRLEN( lv_str ).` | Same |
| To Upper | `TRANSLATE lv_str TO UPPER CASE.` | `lv_str = to_upper( lv_str ).` |
| To Lower | `TRANSLATE lv_str TO LOWER CASE.` | `lv_str = to_lower( lv_str ).` |
| Replace | `REPLACE ALL OCCURRENCES OF 'x' IN lv_str WITH 'y'.` | `lv_str = replace( val = lv_str sub = 'x' with = 'y' occ = 0 ).` |
| Contains | ❌ Manual check | `IF contains( val = lv_str sub = 'abc' ).` |
| Substring | `lv_sub = lv_str+0(3).` | `lv_sub = substring( val = lv_str off = 0 len = 3 ).` |
| Strip Spaces | `CONDENSE lv_str.` | `lv_str = condense( lv_str ).` |

---

## Old vs New — Side by Side

### Concatenation
```abap
" ❌ Old
DATA lv_result TYPE string.
CONCATENATE 'Hello' ' ' 'Kedar' INTO lv_result.

" ✅ New
DATA(lv_result) = 'Hello' && ' ' && 'Kedar'.

" ✅ New — String Templates (most modern)
DATA(lv_name) = 'Kedar'.
DATA(lv_msg) = |Hello { lv_name }, welcome!|.
```

### Upper / Lower Case
```abap
" ❌ Old
TRANSLATE lv_str TO UPPER CASE.
TRANSLATE lv_str TO LOWER CASE.

" ✅ New
lv_str = to_upper( lv_str ).
lv_str = to_lower( lv_str ).
```

### Replace
```abap
" ❌ Old
REPLACE ALL OCCURRENCES OF 'SAP' IN lv_str WITH 'ABAP'.

" ✅ New
lv_str = replace( val = lv_str sub = 'SAP' with = 'ABAP' occ = 0 ).
```

### Contains Check
```abap
" ❌ Old
IF lv_str CS 'ABAP'.
  WRITE 'Found'.
ENDIF.

" ✅ New
IF contains( val = lv_str sub = 'ABAP' ).
  WRITE 'Found'.
ENDIF.
```

### String Templates (Most Powerful — New Only)
```abap
DATA(lv_amount) = 1000.
DATA(lv_currency) = 'INR'.

" Embed variables directly
DATA(lv_msg) = |Total amount is { lv_amount } { lv_currency }|.

" With formatting
DATA(lv_date) = |Today: { sy-datum DATE = USER }|.
DATA(lv_upper) = |{ lv_name ALPHA = OUT }|.
```

---

## 💡 Key Takeaways
- Always prefer **string templates** `| |` over `CONCATENATE` — cleaner and more powerful
- Use **built-in string functions** like `to_upper()`, `contains()`, `replace()` instead of old keywords
- String templates support **inline formatting** for dates, numbers, and alignment
