
DATA WORK.ReglementSinistre;
INFILE DATALINES4
DLM=' '
MISSOVER
DSD ;
INPUT
'1'n             : BEST32.
'2'n             : BEST32.
'3'n             : BEST32.
'4'n             : BEST32.
'5'n             : BEST32.
'6'n             : BEST32.
'7'n             : BEST32.
'8'n             : BEST32.
'9'n             : BEST32.
'10'n            : BEST32. ;
DATALINES4;
432782 898124 997331 1027219 1035496 1030840 1038087 1042345 1042297 1039601
441759 913991 1022900 1077197 1093877 1105416 1106969 1113688 1128980 .
419449 848016 925269 979045 1001307 1018956 1042093 1041417 . .
407962 876043 999854 1118598 1231642 1227681 1244795 . . .
432585 847593 961562 1009070 1117569 1124601 . . . .
409188 864592 960768 1040363 1064804 . . . . .
424180 864312 977930 1026643 . . . . . .
442586 905978 1016550 . . . . . . .
455111 955309 . . . . . . . .
462761 . . . . . . . . .
;;;;;;;;;;;;;;;;;;

DATA WORK.ReglementSinistre2;
INFILE DATALINES4
DLM=' '
MISSOVER
DSD ;
INPUT
'1'n             : BEST32.
'2'n             : BEST32.
'3'n             : BEST32.
'4'n             : BEST32.
'5'n             : BEST32.
'6'n             : BEST32.
'7'n             : BEST32.
'8'n             : BEST32.
'9'n             : BEST32.
'10'n            : BEST32. ;
DATALINES4;
1245296 1393172 1283063 1222759 1164264 1105788 1095652 1089077 1081644 1077726
1457612 1650747 1481550 1380109 1339858 1289025 1266666 1256568 1247416
1516436 1449934 1291319 1203932 1153978 1095327 1070724 1061630
1556421 1660104 1593591 1522083 1476118 1449604 1427291
1278959 1387655 1320005 1239109 1206974 1175470
1084325 1327360 1282673 1226133 1198003
1318756 1640878 1496936 1365069
1141778 1367948 1330510
1467315 1732294
1327945
;;;;;;;;;;;;;;;;;;;
run; 

proc iml;
use ReglementSinistre2;
read all var _all_ into Tgle; /* matrice cumulée*/  /* lire la table sas*/
print Tgle;
start ChainLadder(Tgle,N= YEAR(Date()),TotalProvision,CLmatrix,ultime,CuprodF_i,F_i);  /* commencer un module*/
if isskipped(N) then print "By default the current year is " N;
N=floor(N);
nrTgle=nrow(Tgle);
ncTgle=ncol(Tgle);
if nrTgle ^= ncTgle then do;
print "no square matrix";
end;
/* récupérer la diagonale à partir de première colonne sur IML*/
AntiDiagidx=do((nrTgle-1)*ncTgle+1,ncTgle, -ncTgle+1);
Adiag=Tgle[AntiDiagidx];

/* récupérer les sommes par colonne */
f_i=Tgle[+,2:ncTgle]/(Tgle[+,1:(ncTgle-1)]-t(Adiag[1:(ncTgle-1)]));

f_i=t(f_i||1);

/* la produit de coefficient de développement*/
CuprodF_i=cuprod(f_i[ncTgle:1])[ncTgle:1];
print   Adiag f_i CuprodF_i;

CLmatrix=Tgle;
/* compléter la tableau inférieur*/
do i=1 to nrTgle;
do j=1 to ncTgle-1;
if Tgle[i,(j+1)]<0 then CLmatrix[i,(j+1)]=CLmatrix[i,j]*f_i[j];
end;
end;

/* export de la transposée**/
*CLmatrix=t(CLmatrix);
varnames=compress("k" + char(1)):compress("k" + char(ncTgle));
Years=t(compress(char(N-nrTgle+1)):compress( char(N)));
print CLmatrix[ rowname=Years colname=varNames];

create CLMatrix from CLmatrix [ rowname=Years colname=varNames];
append from CLmatrix[ rowname=Years];
*CLmatrix=t(CLmatrix);
close CLMatrix;

/* le calcule de la provision*/
ultime=CLmatrix[,ncTgle];
print ultime;
/* à partir de la diagonal,
la charge totale estimée est multiplié par les f_i où i est supérieur*/
provision=Adiag #(CuprodF_i-1);
provision=provision[(ncTgle-1):1];
/***** Effectuer la table synthétique*/
create table_Chainladder var {ultime provision Adiag F_i CuprodF_i};
append ;
close table_Chainladder;
TotalProvision=sum(provision);
print provision;
finish;
run ChainLadder(Tgle,2019,TotalProvision,CLmatrix,ultime,CuprodF_i,F_i);
store _all_;
store module= _all_;
quit;

ods graphics on;

ods graphics on;

%output(CL)
ods graphics on;

/* cette étape pour faire une graphique du triangle*/
PROC TRANSPOSE DATA=CLMatrix
OUT=tCLMatrix(LABEL="WORK.CLMATRIX transposée" rename=(col1=ChgeSin))
NAME=k
LABEL='Libellé'n
;
by Years;
VAR k:;
RUN;

proc sgplot data=tCLMatrix;
series X=k Y=ChgeSin /group=Years;
XAXIS LABEL='Année de survenance' ;
Yaxis Label='RAA';
TITLE 'Développement des règlements des sinistres Méthode Chan Ladder';
run;
title;
ods graphics off;