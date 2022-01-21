libname p "C:\SAS\Master2\Actuariat\Non vie\MBFA\Projet";


%LET libname=C:\SAS\Master2\Actuariat\Non vie\MBFA\Projet;
%LET table=simulquant;
%LET lib=p;


%MACRO IMPORT;
PROC IMPORT OUT=&lib..&table
			DATAFILE="&libname.\&table..csv";
			GETNAMES=yes;
			DELIMITER=',';			
RUN;
%MEND;

%IMPORT;





DATA p.paiement;
length 	Annee $12;
input Annee $ Developpement $ Montant;
cards;
2010	1	432782
2010	2	465342
2010	3	99207
2010	4	29888
2010	5	8277
2010	6	-4656
2010	7	7247
2010	8	4258
2010	9	-48
2010	10	-2696
2011	1	441759
2011	2	472232
2011	3	108909
2011	4	54297
2011	5	16680
2011	6	11539
2011	7	1553
2011	8	6719
2011	9	15292
2012	1	419449
2012	2	428567
2012	3	77253
2012	4	53776
2012	5	22262
2012	6	17649
2012	7	23137
2012	8	-676
2013	1	407962
2013	2	468081
2013	3	123811
2013	4	118744
2013	5	113044
2013	6	-3961
2013	7	17114
2014	1	432585
2014	2	415008
2014	3	113969
2014	4	47508
2014	5	108499
2014	6	7032
2015	1	409188
2015	2	455404
2015	3	96176
2015	4	79595
2015	5	24441
2016	1	424180
2016	2	440132
2016	3	113618
2016	4	48713
2017	1	442586
2017	2	463392
2017	3	110572
2018	1	455111
2018	2	500198
2019	1	462761
;
RUN;




*--------------------------------------------------------------------------------;
*------------------------------MODELE GENERAL------------------------------------;
*--------------------------------------------------------------------------------;




PROC GENMOD DATA =p.paiement;
CLASS Annee(REF="2010") Developpement (REF="1");
MODEL Montant = Annee Developpement / DIST=NORMAL link=log type3;;
RUN;


PROC GENMOD DATA = p.paiement;
CLASS Annee Developpement ;
MODEL Montant = Annee Developpement / DIST=NORMAL link=log type3;;
RUN;


*--------------------------------------------------------------------------------;
*------------------------------MODELE 2------------------------------------------;
*--------------------------------------------------------------------------------;


data p.paiement2;
set p.paiement;
if Annee in ("2010"."2011") then Annee="2010-2011";
if Developpement in ("1"."2") then Developpement="1-2";
run;

PROC GENMOD DATA = p.paiement2 ;
CLASS Annee(REF="2010-2011") Developpement (REF="1-2");
MODEL Montant = Annee Developpement / DIST=NORMAL link=log type3;;
RUN;




*--------------------------------------------------------------------------------;
*------------------------------MODELE 3------------------------------------------;
*--------------------------------------------------------------------------------;

data p.paiement3;
set p.paiement;
if Developpement in ("1"."2") then Developpement="1-2";
run;

PROC GENMOD DATA = p.paiement3 plots=all;
CLASS Annee(REF="2010") Developpement (REF="1-2");
MODEL Log_Montant = Annee Developpement / DIST=NORMAL link=log type3;;
RUN;




PROC GENMOD DATA = p.paiement plot =all;
CLASS Annee(REF="2010") Developpement (REF="1");
MODEL Montant = Annee Developpement / DIST=NORMAL link=log ;
bayes diag=all;
RUN;




*--------------------------------------------------------------------------------;
*------------------------------LES QUANTILES-------------------------------------;
*--------------------------------------------------------------------------------;


%MACRO quantile(variable);
PROC UNIVARIATE DATA=p.Simulquant NOPRINT;
  VAR &variable;
  OUTPUT OUT=&variable.quantile PCTLPRE=q PCTLPTS=50 75 80 90 95 99 99.5;
RUN ;
%MEND;

%quantile(provision);
%quantile(x1);
%quantile(x2);
%quantile(x9);
%quantile(x3);
%quantile(x4);
%quantile(x5);
%quantile(x6);
%quantile(x7);
%quantile(x8);


data p.finale_quantile;
set x1quantile x2quantile x3quantile x4quantile x5quantile 
	x6quantile x7quantile x8quantile x9quantile provisionquantile;
run;

proc print data=p.finale_quantile;
run;
