dm "log; clear; odsresults; clear;"; 

Data trtNum;                           
	INPUT trt;
	DATALINES;
	2
	3
	5
	7
	10
	12
	13
	16
	17
	20
	21
	24
	25
	27
	30
	32
	34
	35
	;
RUN;

PROC OPTEX DATA = trtNum seed=73462
	CODING = ORTH;
    CLASS trt;                      
    MODEL trt;                     
    BLOCKS STRUCTURE = (20)6;    
   	OUTPUT OUT = BIBD BLOCKNAME = block; 
RUN;

PROC PRINT DATA = BIBD;
RUN;

PROC FREQ DATA = BIBD;
	TABLE trt / NOCUM NOPCT NOROW NOCOL;
RUN;
