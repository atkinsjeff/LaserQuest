data resist;
input Rep $ Sev Trt $ funct $ R;
rugsev = rug*sev;
datalines;
A	45	B	NPP	0.15068395
B	45	B	NPP	0.29402819
C	45	B	NPP	0.04964153
D	45	B	NPP	-0.0816373
A	65	B	NPP	-0.3609759
B	65	B	NPP	-0.4326911
C	65	B	NPP	0.57604989
D	65	B	NPP	-0.2200318
A	85	B	NPP	0.37056376
B	85	B	NPP	-0.764535
C	85	B	NPP	0.13833424
D	85	B	NPP	-0.3334193
A	45	T	NPP	-0.3467131
B	45	T	NPP	0.24444991
C	45	T	NPP	-1.0014264
D	45	T	NPP	0.52829981
A	65	T	NPP	0.21614881
B	65	T	NPP	0.46731797
C	65	T	NPP	0.44162056
D	65	T	NPP	-0.648554
A	85	T	NPP	0.3376207
B	85	T	NPP	0.31979015
C	85	T	NPP	-0.0977297
D	85	T	NPP	-0.8098977
A	45	B	Rs	-0.4337122
B	45	B	Rs	-0.0666828
C	45	B	Rs	0.09043256
D	45	B	Rs	0.25882789
A	65	B	Rs	-0.2340971
B	65	B	Rs	-0.0128425
C	65	B	Rs	-0.2763593
D	65	B	Rs	-0.1242338
A	85	B	Rs	-0.5038356
B	85	B	Rs	-0.0947318
C	85	B	Rs	-0.2053073
D	85	B	Rs	0.08846977
A	45	T	Rs	-0.5115401
B	45	T	Rs	0.02184044
C	45	T	Rs	-0.1917836
D	45	T	Rs	0.11574361
A	65	T	Rs	-0.3760084
B	65	T	Rs	0.1108828
C	65	T	Rs	-0.3225766
D	65	T	Rs	-0.1195279
A	85	T	Rs	-0.6499326
B	85	T	Rs	0.0253068
C	85	T	Rs	-0.2298558
D	85	T	Rs	-0.0531311
A	45	B	Amax	-0.0051317
B	45	B	Amax	0.08003866
C	45	B	Amax	-0.3304037
D	45	B	Amax	-0.021624
A	65	B	Amax	0.23401957
B	65	B	Amax	-0.0008884
C	65	B	Amax	-0.2469517
D	65	B	Amax	0.09189235
A	85	B	Amax	0.03570278
B	85	B	Amax	0.14109026
C	85	B	Amax	-0.3513238
D	85	B	Amax	0.23994057
A	45	T	Amax	-0.1803662
B	45	T	Amax	-0.0735211
C	45	T	Amax	-0.3366647
D	45	T	Amax	0.30473442
A	65	T	Amax	0.0217908
B	65	T	Amax	0.06850467
C	65	T	Amax	-0.0860412
D	65	T	Amax	0.29598898
A	85	T	Amax	-0.4095025
B	85	T	Amax	0.28743149
C	85	T	Amax	-0.442289
D	85	T	Amax	0.20406384
;

**Q1: Do key carbon cycling processes respond similarly to disturbance?;
proc sort; by funct; run;
proc glm;
class rep sev trt funct;
model R = rep sev trt;
means sev/lsd alpha = 0.1; by funct;
run;

proc glm;
class rep sev trt funct;
model R = rep sev trt;
means trt/lsd alpha = 0.1; by funct;
run;


