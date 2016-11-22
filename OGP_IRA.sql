-- This query creates a list of donors who have made an IRA donation
-- and then submits the list to the subsequent query to return all transactions
-- per ID as well as relevant biographic details.
with ira_id as (
select dgd.hard_credit_donr_entity_id_nbr as entity_id
from cdw.donor_giving_detail_f_v dgd
where dgd.giving_appeal_cd     in ( '48467' )
and dgd.paid_basis_flg in ( 'Y' )
group by dgd.hard_credit_donr_entity_id_nbr
)

select
  dgd.hard_credit_donr_entity_id_nbr as "id",
  entity.person_or_org as "person",
  entity.class_campaign_year as "class_year",
  entity.record_status_code as "status",
    FIRST_VALUE(demo.weight) OVER (PARTITION BY dgd.hard_credit_donr_entity_id_nbr ORDER BY demo.weight DESC) AS "p_weight",
  FIRST_VALUE(demo.dp_interest_desc) OVER (PARTITION BY dgd.hard_credit_donr_entity_id_nbr ORDER BY demo.weight desc) as "p_desc",
  --demo.dp_rating_type_code as "rating_code",
  --demo.dp_interest_desc as "int_code",
  entity.capacity_rating_code as "capacity_code",
  dgd.cads_giving_receipt_nbr as "nbr",
    (case when dgd.giving_appeal_cd in ( '48467' )
    then 1
    else 0 end)as "ira_gift",
  dgd.primary_purpose_code as "prim_purp",
  entity.degree_holder_type as "degree", 
  to_date(extract(month from dgd.giving_record_dt) || '-' || extract(day from dgd.giving_record_dt) || '-' || extract(year from dgd.giving_record_dt), 'mm-dd-yyyy') as "trans_date",
  dgd.transaction_legal_credited_amt as "amt"
  
from cdw.d_entity_mv entity
inner join cdw.donor_giving_detail_f_v dgd
on entity.entity_id = dgd.donor_entity_id_nbr
inner join ira_id
on ira_id.entity_id = entity.entity_id
left outer join cdw.d_bio_demographic_profile_mv demo
ON entity.entity_id = demo.entity_id
and demo.dp_rating_type_code = 'MGS'
where dgd.paid_basis_flg in ('Y')
  and entity.entity_id != '33729'
  and dgd.giving_record_dt >= to_date('12/31/1996', 'MM/DD/YYYY')
