# 05 — Select Statements

## Quick Reference

| Operation | Old Syntax | New Syntax |
|---|---|---|
| Single record | `SELECT SINGLE * FROM ...` | `SELECT SINGLE ... INTO @DATA(ls_rec).` |
| Multiple records | `SELECT * FROM ... INTO TABLE lt.` | `SELECT * FROM ... INTO TABLE @DATA(lt).` |
| With JOIN | `SELECT a~f1 b~f2 FROM a JOIN b ...` | Same + `@DATA` inline |
| Aggregate | `SELECT COUNT(*) FROM ...` | Same + inline |
| Subquery | Limited | Full support |
| FOR ALL ENTRIES | `SELECT ... FOR ALL ENTRIES IN lt ...` | Same — but CDS preferred |
| Into work area | `SELECT SINGLE * INTO ls.` | `SELECT SINGLE * INTO @DATA(ls).` |

---

## Old vs New — Side by Side

### Select Single
```abap
" ❌ Old
DATA ls_user TYPE zs_user.
SELECT SINGLE * FROM zuser
  INTO ls_user
  WHERE id = '001'.

" ✅ New
SELECT SINGLE *
  FROM zuser
  INTO @DATA(ls_user)
  WHERE id = '001'.
```

### Select Multiple Records
```abap
" ❌ Old
DATA lt_orders TYPE TABLE OF vbak.
SELECT * FROM vbak
  INTO TABLE lt_orders
  WHERE kunnr = '1000'.

" ✅ New
SELECT *
  FROM vbak
  INTO TABLE @DATA(lt_orders)
  WHERE kunnr = '1000'.
```

### Select Specific Fields
```abap
" ❌ Old
DATA: lv_name TYPE kna1-name1,
      lv_city TYPE kna1-ort01.

SELECT SINGLE name1 ort01
  FROM kna1
  INTO (lv_name, lv_city)
  WHERE kunnr = '1000'.

" ✅ New
SELECT SINGLE name1, ort01
  FROM kna1
  INTO @DATA(ls_cust)
  WHERE kunnr = '1000'.
```

### JOIN
```abap
" ❌ Old
SELECT a~vbeln a~kunnr b~name1
  FROM vbak AS a
  INNER JOIN kna1 AS b
  ON a~kunnr = b~kunnr
  INTO TABLE lt_result.

" ✅ New
SELECT a~vbeln, a~kunnr, b~name1
  FROM vbak AS a
  INNER JOIN kna1 AS b
    ON a~kunnr = b~kunnr
  INTO TABLE @DATA(lt_result).
```

### FOR ALL ENTRIES
```abap
" ❌ Old — still valid but use carefully
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE lt_items
    FOR ALL ENTRIES IN lt_orders
    WHERE vbeln = lt_orders-vbeln.
ENDIF.

" ✅ Better Alternative — Subquery
SELECT *
  FROM vbap
  INTO TABLE @DATA(lt_items)
  WHERE vbeln IN ( SELECT vbeln FROM vbak WHERE kunnr = '1000' ).
```

### Aggregates
```abap
" ✅ New
SELECT COUNT(*), MAX( netwr ), MIN( netwr ), AVG( netwr )
  FROM vbak
  INTO @DATA(ls_agg)
  WHERE kunnr = '1000'.

WRITE |Count: { ls_agg-count } Max: { ls_agg-max }|.
```

### UNION (New — ABAP 7.50+)
```abap
" ✅ Combine results from two tables
SELECT vbeln AS doc, 'ORDER' AS type FROM vbak
  WHERE kunnr = '1000'
UNION
SELECT vbeln AS doc, 'INVOICE' AS type FROM vbrk
  WHERE kunrg = '1000'
  INTO TABLE @DATA(lt_docs).
```

---

## ⚠️ Important Rules

- Always use `@DATA` for inline declarations in SELECT
- Always check `sy-subrc` after SELECT SINGLE
- Avoid `SELECT *` in production — select only needed fields
- Avoid `FOR ALL ENTRIES` on large tables — use JOINs or subqueries
- Prefer **CDS Views** over complex JOINs in SELECT for reusability

---

## 💡 Key Takeaways
- The `@` symbol before `DATA` is **mandatory** in newer ABAP syntax for host variables
- Use **subqueries** instead of FOR ALL ENTRIES where possible
- Use **inline declarations** `@DATA(lt)` — no need to pre-declare result variables
- For complex reporting, always prefer **CDS Views** over SELECT joins
