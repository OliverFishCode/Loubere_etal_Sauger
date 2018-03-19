TITLE1 'SRCA_Fish_Bounds2';
options ps=512 ls=120 nocenter nodate nonumber;

ODS HTML style=minimal body='tracking2.HTML' ;


run;

data move;
set cart.alex2017;
if origin="A" then delete;
run;

proc univariate data=move;
var movment;
run;

Proc glimmix data=move method=laplace;
class ID Age Year_class Year Origin Pool;
model movment= age pool origin/ dist=binomial link=logit solution;
lsmeans age/ adjust=tukey lines;
lsmeans origin/ adjust=tukey lines;
lsmeans pool/ adjust=tukey lines;
output out=moveout pred( noblup ilink)=PredProb pred(blup ilink)=PredProb_PA  pred(ilink blup)= pred lcl(ilink blup)=lcl ucl(ilink blup)= ucl;
run;
data natal;
input pool$ age count total prop;
datalines;
n	0	37	40	0.925
n	1	3	40	0.075

"Insert your data
in place of the restricted
datalines I have provided"

mc	4	0	26	0
mc	5	0	26	0
;
run;
proc glimmix data=natal method=laplace;
class pool age;
model prop=age/dist=beta link=logit solution ;
lsmeans age /adjust=tukey lines;
output out=dispersal pred( noblup ilink)=PredProb pred(blup ilink)=PredProb_PA  pred(ilink blup)= pred lcl(ilink blup)=lcl ucl(ilink blup)= ucl;
run;
data origin;
input count	pool$ yearc ori$;
datalines;
9	n	2012	o
8	n	2013	o

"Insert your data
in place of the restricted
datalines I have provided"

3	mc	2013	t
8	mc	2014	t
;
run;
proc glimmix data=origin method=laplace;
class pool ori yearc;
model count= pool|ori yearc|ori /dist=negbin link=log solution ;
output out=ori pred( noblup ilink)=PredProb pred(blup ilink)=PredProb_PA  pred(ilink blup)= pred lcl(ilink blup)=lcl ucl(ilink blup)= ucl;
run;
