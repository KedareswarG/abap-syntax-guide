# 07 — CDS Views

## Quick Reference

| Concept | Classic ABAP | CDS Views |
|---|---|---|
| Data Model | SE11 View / SELECT JOIN | `DEFINE VIEW` in DDL |
| Join | SELECT with JOIN | Built-in JOIN in CDS |
| Reusability | Copy-paste SQL | Reuse CDS as source |
| Annotations | ❌ | `@Annotation: value` |
| Associations | ❌ | `association to target` |
| OData Exposure | Manual Gateway | `@OData.publish: true` |
| Access Control | Manual AUTH check | DCL (Data Control Language) |
| Parameters | ❌ | `with parameters` |

---

## Structure of a CDS View
```abap
@AbapCatalog.sqlViewName: 'ZVSALESORDER'        " SE11 view name (max 16 chars)
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Sales Order CDS View'

define view ZCDS_SALES_ORDER as select from vbak
{
    key vbeln   as SalesOrder,
        kunnr   as Customer,
        audat   as OrderDate,
        netwr   as NetValue,
        waerk   as Currency
}
```

---

## JOIN in CDS
```abap
@AbapCatalog.sqlViewName: 'ZVSALESCUST'
@EndUserText.label: 'Sales Order with Customer'

define view ZCDS_SALES_WITH_CUSTOMER as select from vbak
  inner join kna1 on vbak.kunnr = kna1.kunnr
{
    key vbak.vbeln  as SalesOrder,
        vbak.kunnr  as CustomerID,
        kna1.name1  as CustomerName,
        kna1.ort01  as City,
        vbak.netwr  as NetValue
}
```

---

## Associations (Lazy JOIN — Best Practice)
```abap
define view ZCDS_SALES_ORDER as select from vbak

  " Define association — only executed when accessed
  association [1..1] to kna1 as _Customer
    on $projection.Customer = _Customer.kunnr

  association [0..*] to vbap as _Items
    on $projection.SalesOrder = _Items.vbeln
{
    key vbeln   as SalesOrder,
        kunnr   as Customer,
        audat   as OrderDate,
        netwr   as NetValue,

        " Expose associations for consumers
        _Customer,
        _Items
}
```

---

## Annotations

### General
```abap
@AbapCatalog.sqlViewName: 'ZVMYVIEW'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'My CDS View'
@ClientHandling.algorithm: #SESSION_VARIABLE
```

### Semantic Annotations
```abap
@Semantics.amount.currencyCode: 'Currency'
netwr as NetValue,

@Semantics.currencyCode: true
waerk as Currency,

@Semantics.quantity.unitOfMeasure: 'Unit'
kwmeng as Quantity,

@Semantics.unitOfMeasure: true
vrkme as Unit
```

### OData Exposure
```abap
@OData.publish: true   " Auto expose as OData service
```

### UI Annotations (Fiori)
```abap
@UI.lineItem: [{ position: 10, label: 'Sales Order' }]
vbeln as SalesOrder,

@UI.selectionField: [{ position: 10 }]
kunnr as Customer
```

---

## Parameters
```abap
@AbapCatalog.sqlViewName: 'ZVORDBYDATE'
@EndUserText.label: 'Orders by Date'

define view ZCDS_ORDERS_BY_DATE
  with parameters
    p_date : dats

as select from vbak
{
    key vbeln as SalesOrder,
        kunnr as Customer,
        audat as OrderDate
}
where audat = $parameters.p_date
```

### Consuming with Parameters in ABAP
```abap
SELECT * FROM ZCDS_ORDERS_BY_DATE( p_date = '20240101' )
  INTO TABLE @DATA(lt_orders).
```

---

## CDS View Extensions
```abap
" Extend an existing CDS view without modifying it
@AbapCatalog.sqlViewAppendName: 'ZVEXT'
extend view ZCDS_SALES_ORDER with ZCDS_SALES_ORDER_EXT
{
    vbak.vsbed as ShippingCondition
}
```

---

## Access Control (DCL)
```abap
@MappingRole: true
define role ZCDS_SALES_ORDER
{
  grant select on ZCDS_SALES_ORDER
    where ( kunnr ) = aspect pfcg_auth(
      zauth_object, zfield, actvt = '03'
    );
}
```

---

## CDS vs SELECT — When to Use What

| Scenario | Use |
|---|---|
| Simple one-time query | SELECT |
| Reusable data model | CDS View |
| Fiori / OData service | CDS View |
| Embedded Analytics | CDS View |
| RAP Business Object | CDS View |
| Complex runtime logic | ABAP + SELECT |

---

## 💡 Key Takeaways
- Always expose **associations** instead of hardcoded JOINs — more flexible for consumers
- Use **semantic annotations** for currency and quantity fields — mandatory for Fiori
- Prefer **CDS over SELECT JOINs** for any reusable reporting or Fiori scenarios
- Use **DCL** for field-level access control — cleaner than manual AUTH checks in code
- CDS Views are the **foundation of RAP** — mastering CDS = mastering modern ABAP
