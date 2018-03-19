data waterba;
input value site$;
datalines;
0.3200 OHR
0.3400 OHR

"Insert your data
in place of the restricted
datalines I have provided"

0.5800 MMR
0.4100 MMR
;
proc glm data=waterba;
class site;
model value = site;
lsmeans site /  cl lines adjust=Tukey;
output out=resids p=pred r=resid;
proc print;
goptions reset;
symbol value=dot;
proc gplot;
plot pred*resid;
run;
proc univariate noprint data=resids;
qqplot resid / normal;
run;
quit;
