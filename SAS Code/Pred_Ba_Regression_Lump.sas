data predba;
input site$ otoBa;
if site='OHRU' then waterBa=;
if site='OHRL' then waterBa=;
if site='MMRU' then waterBa=;
if site='MMRL' then waterBa=;
if site='TRBU' then waterBa=;
if site='TRBL' then waterBa=;
if site='C1' then waterBa=;
if site='C2' then waterBa=;
if site='C3' then waterBa=;
if site='C4' then waterBa=;
if site='C5' then waterBa=;
if site='C6' then waterBa=;
if site='C7' then waterBa=;
if site='C8' then waterBa=;
if site='C9' then waterBa=;
if site='C10' then waterBa=;
if site='C11' then waterBa=;
if site='C12' then waterBa=;
if site='C13' then waterBa=;
if site='C14' then waterBa=;
if site='C15' then waterBa=;
if site='C16' then waterBa=;
datalines;
C1 .0035
C2 .003

"Insert your data
in place of the restricted
datalines I have provided"

TRBU .
TRBL .
;
proc glm data=predba; model otoBa=waterBa/ p clm;
proc plot; plot otoBa*waterBa;
run;
quit;
