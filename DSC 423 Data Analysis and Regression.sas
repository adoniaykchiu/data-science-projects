
title "Analysis and Prediction Airbnb Melbourne Australia";
proc import datafile = "S:\Final\Ying_Airbnb.csv" out=price replace;
getnames=yes;
delimiter=',';
run;

proc print;
run;

data price;
set price;
dHRT1=(host_response_time='within an hour'); /*base='N/A'*/
dHRT2=(host_response_time='within a few hours');
dHRT3=(host_response_time='within a day');
dHRT4=(host_response_time='a few days or more');
dHIS=(host_is_superhost='t');
dRT1=(room_type='Private room'); /*base='Entire home/apt'*/
dRT2=(room_type='Shared room');
dBT1=(bed_type='Futon'); /*base='Real Bed'*/
dBT2=(bed_type='Pull-out Sofa');
dBT3=(bed_type='Airbed');
dBT4=(bed_type='Couch');
dCP1=(cancellation_policy='moderate'); /*base='Strict'*/
dCP2=(cancellation_policy='flexible');
dRG1=(Region='NSM'); /*base='IM'*/
dRG2=(Region='SEM');
dRG3=(Region='EM');
dRG4=(Region='WM');
HRT_superhost = dHRT1*dHIS;
run;

proc print;
run;

title "Histogram for Distribution of Price";
proc univariate normal;
var price;
histogram / normal (mu=est sigma=est);
inset median mean min max range Q1 Q3 kurtosis skewness /pos = ne;
run;

data price;
set price;
Inprice = log(price);

proc print;
run;

title "Histogram for Distribution of Inprice";
proc univariate normal;
var Inprice;
histogram / normal (mu=est sigma=est);
inset median mean min max range Q1 Q3 kurtosis skewness /pos = ne;
run;

title "Boxplot - dRT1 and Inprice";
proc sort;
by dRT1;
run;

proc boxplot;
plot Inprice*dRT1;
run;

title "Boxplot - dRG4 and Inprice";
proc sort;
by dRG4;
run;

proc boxplot;
plot Inprice*dRG4;
run;

proc sgscatter;
title "Scatterplot Matrix for Inprice and other variables";
matrix Inprice host_total_listings_count accommodates security_deposit cleaning_fee review_scores_rating HRT_superhost;
run;

/*Full Model*/
proc reg corr;
title "Full Model - Residual Plots";
model Inprice = host_total_listings_count accommodates security_deposit cleaning_fee review_scores_rating dHRT1-dHRT4 dHIS dRT1 dRT2 dBT1-dBT4 dCP1 dCP2 dRG1-dRG4 HRT_superhost /vif r influence stb;
plot student.*(host_total_listings_count accommodates security_deposit cleaning_fee review_scores_rating dHRT1-dHRT4 dHIS dRT1 dRT2 dBT1-dBT4 dCP1 dCP2 dRG1-dRG4 HRT_superhost predicted.);
plot npp.*student.;
run;

data price_new;
set price;
if _n_ in (413) then delete;
run;

/*Model Validation*/
title "Train and Test Sets for Airbnb Prices";
proc surveyselect data=price_new out=xv_all seed=495857
samprate=0.75 outall;
run;
proc print;
run;

data xv_all;
set xv_all;
if selected then new_y=Inprice;
run;
proc print;
run;

title "Model Selection";
proc reg data=xv_all;
model new_y = host_total_listings_count accommodates security_deposit cleaning_fee review_scores_rating dHRT1-dHRT4 dHIS dRT1 dRT2 dBT1-dBT4 dCP1 dCP2 dRG1-dRG4 HRT_superhost /selection=forward;
run;

proc reg data=xv_all;
model new_y = host_total_listings_count accommodates security_deposit cleaning_fee review_scores_rating dHRT1-dHRT4 dHIS dRT1 dRT2 dBT1-dBT4 dCP1 dCP2 dRG1-dRG4 HRT_superhost /selection=backward;
run;

*removed dRG2;
title "Train Set";
proc reg corr;
model new_y = accommodates dHRT4 dRT1 dRT2 dRG1 dRG4 /vif r influence stb;
plot student.*( accommodates dHRT4 dRT1 dRT2 dRG1 dRG4 predicted.);
plot npp.*student.;
run;

data price_new2;
set price_new2;
if _n_ in (639) then delete;
run;

title "Validation - Test Set";
proc reg data=price_new2;
model new_y = accommodates dHRT4 dRT1 dRT2 dRG1 dRG4;
output out=outm(where=(new_y=.)) p=yhat;
run;
proc print;
run;

title "Difference between Observed and Predicted in Test set";
data outm_sum;
set outm;
d=Inprice-yhat;
absd=abs(d);
run;

proc summary data=outm_sum;
var d absd;
output out=outm_stats std(d)=rmse mean(absd)=mae;
run;
proc print data=outm_stats;
title "Validation Statistics for Model";
run;
proc corr data=outm;
var Inprice yhat;
run;

title "Final Model";
proc reg data=price_new2;
model new_y = accommodates dHRT4 dRT1 dRT2 dRG1 dRG4 /vif r influence stb;
model new_y = accommodates dHRT4 dRT1 dRT2 dRG1 dRG4;
plot student.*(accommodates dHRT4 dRT1 dRT2 dRG1 dRG4 predicted.);
plot npp.*student.;
run;

title "Prediction for Final model";
data price_pred;
input new_y accommodates dHRT4 dRT1 dRT2 dRG1 dRG4;
datalines;
. 5 1 1 0 0 1
. 2 0 0 1 1 0
;

data price_comb;
set price_pred price_new2;

proc print;
run;

proc reg;
model new_y = accommodates dHRT4 dRT1 dRT2 dRG1 dRG4 /p clm cli alpha=0.05;
run;
