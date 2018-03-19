data predsr;
input site$ otoSr;
if site='OHRU' then waterSr=;
if site='OHRL' then waterSR=;
if site='MMRU' then waterSr=;
if site='MMRL' then waterSR=;
if site='CBRU' then waterSR=;
if site='CBRL' then waterSR=;
if site='GRRU' then waterSR=;
if site='GRRL' then waterSR=;
if site='KYR' then waterSR=;
if site='STR' then waterSR=;
if site='TNRU' then waterSR=;
if site='TNRL' then waterSR=;
if site='TWRU' then waterSR=;
if site='TWRL' then waterSR=;
if site='WBRU' then waterSR=;
if site='WBRL' then waterSR=;
if site='SD' then waterSR=;
if site='URL' then waterSR=;
if site='F' then waterSR=;
if site='KIN' then waterSR=;
if site='RR' then waterSR=;
if site='K' then waterSR=;
datalines;
SD .514472
URL .31502

"Insert your data
in place of the restricted
datalines I have provided"

WBRU .
WBRL .
;
proc glm data=predsr; model otoSr=waterSr/ p clm;
proc plot; plot otoSr*waterSr;
run;
quit;
