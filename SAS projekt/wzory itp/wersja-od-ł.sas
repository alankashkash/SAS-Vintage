/*Lokalizacja pliku dane.cpo */
filename dane "C:\Users\lenovo\Desktop\SAS projekt\dane\dane.cpo";
/*Wczytanie i konwersja danych do odpowiedniego systemu operacyjnego */
proc cimport file=dane lib=work;/* Biblioteke mo¿a podac inn¹ :)*/
run;
/*algorytm tree tworzenia kateogryzacji zmiennych ci¹g³ych wzglêdem zmiennej */
/*(c) Karol Przanowski*/
/*kprzan@sgh.waw.pl*/


%let dir_projekt=C:\Users\lenovo\Desktop\SAS projekt\dane; 
%let dir=C:\Users\lenovo\Desktop\SAS projekt\tree;   
libname wej "&dir_projekt" compress=yes; 
libname wyj "&dir" compress=yes;   
/*makro licz¹ce zmienne vintage (zero-jedynkowe)      
zmienna vitage zale¿na jest od:                                   
m_prod - miesi¹c w którym wszystkie kredyty uruchomiono        
m - liczba miesiêcy po uruchomieniu                              
due - min. liczba opóŸnionych rat ustalana przez nas na poziomie 1,2,3  
ostatecznie otrzymujemy 3 zmienne vin: vin1, vin2, vin3*/   
%macro DataPreparation();   
data wyj.DaneDoVintage;               
	set wej.Transactions;            
	seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));   
	/*intck - zwraca liczbê interwa³ów pomiêdzy danymi okresami czasowymi*/     
	/*liczenie ile miesiêcy jest pmiêdzy period a fin period*/         
	/*fin_period - data udzielenia kredytu , period - data raportowa*/   

vin3=(due_installments>=3);                  
vin2=(due_installments>=2);                   
vin1=(due_installments>=1); *klient nie sp³aci³ conajmniej jednej raty; 
output;
 
	if status in ('B','C') and period<='200812' then do;      
	/*B - bankrupcy, C - closed*, A - available*/                
		n_steps=intck('month',input(period,yymmn6.),input('200812',yymmn6.));      
		do i=1 to n_steps;                              
			period=put(intnx('month',input(period,yymmn6.),1,'end'),yymmn6.);     
			/*intnx - zwiêksza czas o podan¹ wartoœæ */                       
			seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.)); 
			output;                        
		end;             
	end;      
	keep vin3 vin2 vin1 seniority aid fin_period period; 
run; 
%do i=1 
%to 3; 
proc means data=wyj.DaneDoVintage noprint nway;  
	class fin_period seniority;    
	var vin&i; 
	output out=wyj.vintagr_vin&i(drop=_freq_ _type_) n()=production mean()=vin&i; 
	format vin&i percentn10.;
run;  
proc means data=wyj.DaneDoVintage noprint nway;    
	class fin_period; 
	var vin&i;   
	output out=wyj.production(drop=_freq_ _type_) n()=production;    
	where seniority=0; 
run;  

proc transpose data=wyj.vintagr_vin&i out=wyj.vintage&i prefix=months_after_; 
	by fin_period;     
	var vin&i;        
	id seniority; 
run; 

data wyj.vintage&i;  
	set wyj.vintage&i;     
	drop _NAME_; 
run;  

proc sql noprint;  
	create table wyj.Seniority_vin&i as  
	select aid,seniority, vin&i   
	from wyj.DaneDoVintage    
	where seniority in (3,6,9,12)   
	order by aid, seniority; 
run; 
proc transpose data=wyj.Seniority_vin&i out=wyj.Seniority_trans_vin&i prefix=vin&i._;   
	by aid;     
	var vin&i;   
	id seniority;
run;   

proc sql noprint;   
	create table wyj.production_vin&i as    
	select a.*, b.*  
	from wej.production a    
	left join wyj.Seniority_trans_vin&i b 
	on a.aid=b.aid; 
quit; 

/*eksport vintage do xlsx*/ 
%let arkusz = vintage&i; 
proc export data = wyj.vintage&i dbms=xlsx outfile="D:\SGH\saslukasz" replace; 
sheet=&arkusz; 
run;   

%end;  
 
%mend;   
%DataPreparation();   
/*Kategoryzacja zmiennych Usuniêcie/przekodowanie zmiennych tekstowych: 
app_char_branch app_char_gender app_char_job_code app_char_marital_status 
app_char_city app_char_home_status app_char_cars */ 
data wyj.vin; 
set wyj.Production_vin3; run;     
%let zb=wyj.vin; 
%put &zb; 
%let tar=vin3_12; 
%put &tar;   
proc contents 
data= wej.Production 
noprint out=varlist 
(keep = name); run;   
/*Wykluczenie zmiennych, które nie maj¹ byæ kategoryzowane*/ 
data varlist; set varlist; 
where name not in ('cid', 'aid', 'product', 'period', 'app_char_branch', 'app_char_cars', 
'app_char_city', 'app_char_gender', 'app_char_home_status', 
'app_char_job_code', 'app_char_marital_status'); 
run;   /*Utworzenie listy zmiennych*/     
/*slect into - zapis do zmienne_int_ord*/
proc sql noprint; 
select name into :zmienne_int_ord separated by ' ' from varlist; 
quit;     
/*Kategoryzacja zmiennych uwzglêdniaj¹ca algorytm tree*/   
data wyj.vin; set wyj.Production_vin3; 
run; %let zb=wyj.Production_vin3; 
%put &zb; 
%let tar=vin3_12; 
%put &tar; 
%let il_zm = &sqlobs;   
%put ***&il_zm***&zmienne_int_ord;   

/*maksymalna liczba podzia³ów minus 1*/ 
%let max_il_podz=2; 

/*minimalna liczba obs w liœciu*/ 
%let min_percent=3;   

/*Odwo³anie siê do makra tree.sas*/ 
%include "&dir.tree.sas" / source2;  
 
/*Zdefiniowanie warunków podzia³u*/   

data wyj.warunek; 
set wyj.podzialy_int_niem; 
low = scan(war,1,'<'); 
if substr(low,1,1) = 'n' then low = '1=1and'; 
/*substr - wycina kawa³ek tekstu*/ 
if substr(low,1,3) ne '1=1' then low = catt(lowcase(zmienna),'>',low,'and'); 
/*ne - not exist*/ 
high = scan(war,2,'='); 
if high='' then high ='1=1'; 
else high = catt(lowcase(zmienna),'=<',high); 
if substr(war,1,1) = 'n' then miss = cats('not missing(',lowcase(zmienna),')and'); 
warunek = catt(miss,low,high); 
warunek = transtrn(warunek,'and',' and '); 
warunek = transtrn(warunek,'=<',' =< '); 
warunek = transtrn(warunek,'>',' > '); 
klucz=catt(lowcase(zmienna),'#',grp,'#',warunek); 
keep zmienna grp warunek klucz; 
run;   

proc sql noprint; 
select klucz into :warunki separated by '^' 
from wyj.warunek; 
quit;   

%put &warunki;   

/*Przyporz¹dkowanie obserwacji do grup zmiennych*/ 
%macro groups; 
data wyj.vin_groups;   
set wyj.vin nobs=n_obs;
%do i = 1 %to &sqlobs; 
%let warunek = %scan(%scan(&warunki,&i,'^'),3,'#'); 
%let zmienna = %scan(%scan(&warunki,&i,'^'),1,'#'); 
%let grupa = %scan(%scan(&warunki,&i,'^'),2,'#'); 
if &warunek then &zmienna=&grupa; 
%end; 
output; 
run; 
%mend; 
%groups; 

/*Badanie zaleÅ¼noÅ›ci pomiÄ™dzy zmiennymi a zmiennÄ… vin3_12 poprzez wspÃ³Å‚czynnik VCramera*/ 

%let groups = act#ags#agr#app#; 

*4 nazwy w jednej makrozmiennej, do wybrania; 
%macro vcram; %do i = 1 %to 4; 
%let j = %scan(&groups,&i,'#');   
data wyj.vin_groups_&j;
set wyj.vin_groups; keep vin3_12 &j.:; 

*wszystkie zmienne z prefixem act; 

run;   
proc contents data=wyj.vin_groups_&j out=varlist_&j noprint; 
run;  
proc sql noprint; select name into:zm_&j separated by '#' from varlist_&j; 
quit;   
%let liczba=&sqlobs;  
data vcram_&j; 
length vcram _cramv_ 8 zmienna $30; 

*8 znaków ka¿da zmienne; 

run;   

/* Obliczanie wspÃ³Å‚czynnika chi2 i V-Cramera */ 

%do k = 1 %to (&liczba-1); 
%let zm=%scan(&&zm_&j,&k,'#'); 
proc freq data = wyj.vin_groups_&j noprint; 
tables vin3_12*&zm/chisq; 

*chisq - dodaje nam wartoœci do vcramera;

output out = vcram_&zm cramv; 
run;  
data vcram_&zm;
set vcram_&zm;
zmienna = "&zm";
vcram = abs(_cramv_); *wartoœc bezwzglêna ze wspó³czynnika vcramera; 
run; 

data vcram_&j; 
set vcram_&j vcram_&zm; 
run; 
%end;  

/* Sortowanie wedÅ‚ug malejÄ…cej wartoÅ›ci wspoÅ‚czynnika V-Cramera */   
proc sort data = vcram_&j out = wyj.vcram_&j; 
by descending vcram; run;   
/*WybÃ³r 5 najbardziej skorelowanych zmiennych z kaï¿½dej grupy*/ 
data wyj.vcram_&j; 
set wyj.vcram_&j (obs = 5); 
run;  
%end;
%mend; 
%vcram; *;  
%let b = 'ins#css#12#'; 
%let c = '3#6#9#12#';  
%macro tabulate;
%do i = 1 %to 4;
%let typ = %scan(&groups,&i,'#');  
proc sql; 
select zmienna into :zmienne separated by ' ' 
from wyj.vcram_&typ; quit; 
data tabulate_&typ; 
run; 
%do j = 1 %to 3;
/*%do k = 1 %to 3;*/ /*%let kred = %scan(&b,&k,'#');*/
%do l = 1 %to 4; 
%let sen = %scan(&c,&l,'#');  
*sen - seniority;   
proc tabulate data = wyj.Production_vin&j out = tabulate_&typ._vin&j._sen&sen; 
class vin&j._&sen.; 
var &zmienne;
table (&zmienne), vin&j._&sen.*mean;
run;  
data tabulate_&typ._vin&j._sen&sen; 
length zmienna $15; 
set tabulate_&typ._vin&j._sen&sen; 

zmienna = "vin&j._&sen.";  
due = &j; sen = &sen;
run;   
data tabulate_&typ; 
set tabulate_&typ tabulate_&typ._vin&j._sen&sen; 
run;  
/*%end;*/ 
%end;
%end;  
proc export data = tabulate_&typ dbms=xlsx outfile  = "C:\Users\Eryk\Desktop\dane\testy.xlsx" replace; 
sheet = tabulate_&typ;
run;  
%end;
%mend;
%tabulate;   

/*Analiza vintage zbiorczo i w podziale na grupy produktów - export excel*/
%macro vintage_zbiorczy;
%do i=1 %to 3; 
%let arkusz = vintage&i; 
proc export data=wyj.vintage&i dbms=xlsx 
outfile="C:\Users\Agata\Desktop\projekt zaliczeniowy sas\zbiorczy.xlsx" replace; 
sheet=&arkusz;
run; 
%end;
%mend;
%vintage_zbiorczy;   
/* PROGNOZA dla Vintage3_12 */ 
 
/* Stworzenie oddzielnego zbioru bazowego do modelu regresji logistycznej */ 
data wyj.dane_do_regresji;
set wej.Transactions; 
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.)); 
*liczba okresów miêdzy datami - miesiêcy; 
vin3=(due_installments>=3);
output; 
if status in ('B','C') and period<='200812' then do;   
n_steps=intck('month',input(period,yymmn6.),input('200812',yymmn6.));
*ró¿nica miêdzy dat¹ raportow¹ a dat¹ prognozy;     
do i=1 to n_steps;            
period=put(intnx('month',input(period,yymmn6.),1,'end'),yymmn6.);
*zwiêksza datê o keden, 'end' ¿e do koñca okresu;        
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.)); 
output; 

end; 
end; 
keep vin3 seniority aid fin_period; 
run; *;   
/* Sortowanie zbioru z danymi do regresji oraz zbioru zawierï¿½cego chorakterystyki klientï¿½w */ 
proc sort data=wyj.dane_do_regresji; 
by aid; 
run;   
proc sort data=wej.Production(keep= aid act_CCss_Acp5y act_call_n_loan act_cins_min_seniority act_ccss_n_loans_act 
		act_CAll_Acp5y agr12_Mean_CMaxA_Days agr9_Mean_CMaxA_Days agr12_Min_CMaxA_Days agr9_Min_CMaxA_Days 
		agr6_Mean_CMaxA_Days ags12_Mean_CMaxC_Days ags9_Mean_CMaxC_Days ags12_Min_CMaxC_Days ags12_Mean_CMaxA_Days 
		ags9_Min_CMaxC_Days app_char_branch app_loan_amount app_installment app_n_installments app_income) 
out=prod_regresja; 
by aid; 
run;   
/* laczenie zbiorï¿½w */
data wyj.vin_regresja; 
merge wyj.dane_do_regresji(in=z) prod_regresja; 
/*lacza dwa zbiory*/ by aid; 
if z; 
/*to oznacza de facto left join*/ 
run; 
/* Dodanie do bazy danych zmiennych okreï¿½lajï¿½cych rodzaj produktu oraz numer miesiï¿½ca */ 
data wyj.vin_reg; 
set wyj.vin_regresja; nazwa_produktu = substr(aid, 1, 3); 
/*tworza nazwe produktu wycinajac ja z aid*/
if substr(fin_period, 5,1) = 0 then miesiac = substr(fin_period,6,1); 
/*wyciagaja tylko miesiac*/    
else miesiac = substr(fin_period,5,2); run;  
/* data wyj.vin_reg2; set wyj.vin_reg; if fin_period<='200712' then vin3 = vin3;  
else vin3 = .; run; */   
data wyj.kamil_trenuje_all; 
set wyj.vin_reg;
if fin_period<='200712';
run;  
data wyj.kamil_testuje_all; 
set wyj.vin_reg; 
if fin_period>'200712'; 
run; 

%macro product(name);   
data wyj.trenuje_&name;   
set wyj.kamil_trenuje_all;   
where nazwa_produktu = "&name";
run;  
data wyj.testuje_&name;  
set wyj.kamil_testuje_all; 
where nazwa_produktu = "&name"; 
run;  
%mend;
%product (css); 
%product (ins);  
/* Procedura regresji logistyzcnej z metoda selekcji krokowej */
proc logistic data=wyj.trenuje_css; 
class app_char_branch;
model vin3 (Event="1") = act_CCss_Acp5y act_call_n_loan act_cins_min_seniority act_ccss_n_loans_act 
act_CAll_Acp5y agr12_Mean_CMaxA_Days agr9_Mean_CMaxA_Days agr12_Min_CMaxA_Days agr9_Min_CMaxA_Days 
agr6_Mean_CMaxA_Days ags12_Mean_CMaxC_Days ags9_Mean_CMaxC_Days ags12_Min_CMaxC_Days ags12_Mean_CMaxA_Days 
ags9_Min_CMaxC_Days app_char_branch app_loan_amount app_installment app_n_installments app_income /selection=stepwise;

score data=wyj.testuje_css out=wyj.prediction_css;
run;  
proc logistic data=wyj.trenuje_ins;
class app_char_branch; 
model vin3 (Event="1") = act_CCss_Acp5y act_call_n_loan act_cins_min_seniority act_ccss_n_loans_act 
act_CAll_Acp5y agr12_Mean_CMaxA_Days agr9_Mean_CMaxA_Days agr12_Min_CMaxA_Days agr9_Min_CMaxA_Days 
agr6_Mean_CMaxA_Days ags12_Mean_CMaxC_Days ags9_Mean_CMaxC_Days ags12_Min_CMaxC_Days ags12_Mean_CMaxA_Days 
ags9_Min_CMaxC_Days app_char_branch app_loan_amount app_installment app_n_installments app_income /selection=stepwise; 
score data=wyj.testuje_ins out=wyj.prediction_ins;
run;  
*act_CCss_Acp5y act_call_n_loan act_cins_min_seniority act_ccss_n_loans_act act_CAll_Acp5y 
agr12_Mean_CMaxA_Days agr9_Mean_CMaxA_Days agr12_Min_CMaxA_Days agr9_Min_CMaxA_Days agr6_Mean_CMaxA_Days 
ags12_Mean_CMaxC_Days ags9_Mean_CMaxC_Days ags12_Min_CMaxC_Days ags12_Mean_CMaxA_Days ags9_Min_CMaxC_Days 
app_char_branch app_loan_amount app_installment app_n_installments app_income ; /*
cutoff values: 
css - 0.155 
ins - 0.051
*/

data wyj.prediction_css_labeled;
set wyj.prediction_css;
if P_1 ^=.;
if P_1 > 0.02 then LABEL = 1;
else LABEL = 0;
run;

data wyj.prediction_ins_labeled;
set wyj.prediction_ins;
if P_1 ^=.;
if P_1 >=0.035 then LABEL = 1;
else LABEL = 0;
run;

*predykcja vintage 3 w 2008;
*css;
proc means data=wyj.prediction_css_labeled noprint nway; *noprint - bez raportu; *nway -;
	class fin_period seniority;
	var LABEL;
	output out=wyj.prediction_css_vintage (drop=_freq_ _type_) n()=production
mean()=vintage3_p_css;
	format LABEL nlpct12.2;
	run;
proc transpose data=wyj.prediction_css_vintage out=wyj.prediction_css_vintage2 prefix=months_after_;
	by fin_period;
	var vintage3_p_css;
	id seniority;
run;

*ins;
proc means data=wyj.prediction_ins_labeled noprint nway; *noprint-bez raportu; *nway -;
	class fin_period seniority;
	var LABEL; 
	output out=wyj.prediction_ins_vintage (drop=_freq_ _type_) n()=production
mean()=vintage3_p_ins;
	format LABEL nlpct12.2;
run;

proc transpose data=wyj.prediction_ins_vintage out=wyj.prediction_ins_vintage2 prefix=months_after_;
	by fin_period;
	var vintage3_p_ins;
	id seniority;
run;
*all=ins+css;
*wyj.kamil_trenuje_all;
*wyj.kamil_testuje_all;

proc logistic data=wyj.kamil_trenuje_all;
class app_char_branch;
model vin3 (Event="1") = act_CCss_Acp5y act_call_n_loan act_cins_min_seniority act_ccss_n_loans_act
act_CAll_Acp5y agr12_Mean_CMaxA_Days
agr9_Mean_CMaxA_Days agr12_Min_CMaxA_Days agr9_Min_CMaxA_Days agr6_Mean_CMaxA_Days
ags12_Mean_CMaxC_Days ags9_Mean_CMaxC_Days
ags12_Min_CMaxC_Days ags12_Mean_CMaxA_Days ags9_Min_CMaxC_Days app_char_branch
app_loan_amount app_installment app_n_installments
app_income /selection=stepwise;
score data=wyj.kamil_testuje_all out=wyj.prediction_all;
run;
*cutoff=0.22;

data wyj.prediction_all_labeled;
set wyj.prediction_all;
if P_1 ^=.;
if P_1 >= 0.22 then LABEL = 1;
else LABEL = 0;
run;

proc means data=wyj.prediction_all_labeled noprint nway; *noprint - bez raportu; *nway -;
	class fin_period seniority;
	var LABEL; 
	output out=wyj.prediction_all_vintage (drop=_freq_ _type_) n()=production
mean()=vintage3_p_all;
	format LABEL nlpct12.2;
run;

proc export
data=wyj.prediction_all_vintage2
dbms=xlsx 
outfile="C:\link\all_pred_vintage3.xlsx"
replace;
sheet="Predykcja";
run;

*tabela production do wkyresów;
proc freq data=wej.Production;
tables product* period / nocum nopercent norow nocol;
run;