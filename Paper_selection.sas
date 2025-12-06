%macro import_csv(dataset);
PROC IMPORT OUT= WORK.&dataset
            DATAFILE= "D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\&dataset..csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data &dataset;
set &dataset;
dataset="&dataset";
primary_title=lowcase(primary_title);

if primary_title="" then delete;
if index(doi, "10.") > 0 then doi = substr(doi, index(doi, "10."));
  if length(primary_title) > 0 and substr(primary_title, length(primary_title)) = "." then 
        primary_title = substr(primary_title, 1, length(primary_title)-1);

primary_title2=substr(primary_title,1,100); /**if for the title the first 100 characters are the same we think they are the same paper**/

primary_title3 = compress(primary_title2, , 'kasd'); /* 'k' = keep, 'a' = alphabetic, 's' = space,  'd' = digits — add it if you want to keep numbers */

if substr(primary_title, 1, 2) = "ab" and 
      anydigit(substr(primary_title, 3, 1)) > 0 then 
        abstrac_poster = 1;

if substr(primary_title, 1, 3) = "abs" and 
      anydigit(substr(primary_title, 4, 1)) > 0 then 
        abstrac_poster = 1;

if substr(primary_title, 1, 3) = "pos" and 
      anydigit(substr(primary_title, 4, 1)) > 0 then 
        abstrac_poster = 1;

if substr(primary_title, 1, 3) = "sat" and 
      anydigit(substr(primary_title, 4, 1)) > 0 then 
        abstrac_poster = 1;

if substr(primary_title, 3, 1) = "?" then 
        non_English = 1;


run;
%mend import_csv;

%import_csv(ovid);
%import_csv(pubmed);
%import_csv(ScienceDirect);
%import_csv(Scopus);
%import_csv(WebofScience);
%import_csv(google_scholar);
%import_csv(medrxiv);
data all;
set ovid pubmed sciencedirect webofscience scopus google_scholar medrxiv;
run;
proc freq data=all;
table dataset;
run;
data all2;
set all;
run;
proc sort data=all2 out=unique_title nodupkey dupout=duplicates_title;
by primary_title3;
run;

libname l'D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results';
data l.unique_title;
set unique_title;
run;

PROC EXPORT DATA= WORK.UNIQUE_title 
            OUTFILE= "D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\unique_title.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

data WORK.UNIQUE_ANNOTATED    ;
      infile 'D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\unique_title_annotated.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
         informat type_of_reference $4. ;
         informat primary_title $300. ;
         informat paper_type $30. ;
         informat doi $100. ;
         informat authors $300. ;
         informat year best32. ;
         informat abstract $3000. ;
         informat dataset $4. ;
         format type_of_reference $20. ;
         format primary_title $300. ;
         format paper_type $30. ;
         format doi $100. ;
         format authors $300. ;
         format year best12. ;
         format abstract $3000. ;
         format dataset $4. ;
      input
                  type_of_reference  $
                  primary_title  $
                  paper_type  $
                  doi  $
                  authors  $
                  year
                  abstract  $
                  dataset  $
      ;
run;
data unique_annotated2;
set unique_annotated;
if type_of_reference='CONF' then paper_type="abstract";
run;

proc freq data=unique_annotated2;
table paper_type;
run;

data full_text_to_read;
set UNIQUE_ANNOTATED;
where paper_type="to read";
run;

PROC EXPORT DATA= WORK.full_text_to_read 
            OUTFILE= "D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\full_text_to_read.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC IMPORT OUT= WORK.full_annotated
            DATAFILE= "D:\flinders\REMS the accuracy of diagnostic tests for Osteoporosis\Search results\full_text_to_read_annotated.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc freq data=full_annotated;
table information;
run;
