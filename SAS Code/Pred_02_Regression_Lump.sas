data pred02;
input site$ oto02;
if site='OHRU' then water02=;
if site='OHRL' then water02=;
if site='MMRU' then water02=;
if site='MMRL' then water02=;
if site='TRBU' then water02=;
if site='TRBL' then water02=;
if site='1' then water02=;
if site='2' then water02=.;
if site='3' then water02=;
if site='4' then water02=;
datalines;
1 -7.39
2 -3.48

"Insert your data
in place of the restricted
datalines I have provided"

TRBU .
TRBL .
;
proc glm data=pred02; model oto02=water02/ p clm;
proc plot; plot oto02*water02;
run;
quit;
