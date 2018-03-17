6000 'entrada de coordenadas
6010 INPUT "numero de estaciones:",n
6020 FOR i=0 TO 2
6030 PRINT " X";CHR$(65+i);:INPUT":",coo(i,0):GOSUB 990
6040 PRINT " Y";CHR$(65+i);:INPUT":",coo(i,1):GOSUB 990
6050 NEXT
6060 'entrada de lecturas
6070 FOR i=0 TO n-1
6080 FOR j= 0TO2
6090 PRINT"LHp";i;: IF j=1 THEN PRINT"b" ELSE PRINT"p;i+j
6100 INPUT"",lh(i,j)
6110 NEXT:NEXT
6120 'calculo de angulos
6130 FOR i=0TO n-1
6140 FOR j=0 TO2
6150 ang(i,j)=lh(i,j)-lh(i,j+1)
6160 PRINT CHR$(136+j);i;"=LHp";i+1
6170 NEXT:NEXT
6180 'calculo de azimutes y distancias por diferencia de coordenadas conocidas
6190 CLS:PRINT "Calculo de dist y azi entre vert"
6200 FOR i=0 TO 1
6210 z=  POL (coo(i+1,0)-coo(i,0),coo(i+1,1)-coo(i,1):dst(i)=X:azi(i)=100-Y
6220 IF azi(i)<400 THEN azi(i)=azi(i)+400
6230 IF azi(i)>400 THEN azi(i)=azi(i)-400
6240 PRINT"D";CHR$(97+i);"æ";CHR$(98+i);"=‚((X";CHR$(98+i);"-X";CHR$(97+i);")’+(Y";CHR$(98+i);"-Y";CHR$(97+i);")’)=";TAB(0);dst(i)
6250 PRINT"Œ";CHR$(97+i);"æ";CHR$(98+i);"=arctg((X";CHR$(98+i);"-X";CHR$(97+i);")/(Y";CHR$(98+i);"-Y";CHR$(97+i);"))=";TAB(0);azi(i) 
6260 NEXT
6270 'calculo de los angulos ABC
6280 B=ABS(azi(0,0)-azi(1,0=
6290 FOR i=0 TOn-1
6300 a=a*SIN(ang(i,0)
6310 b=b*SIN(ang(i,1)
6320 NEXT
6330 M=dst(1,0)/dst(0,0)*a/b
6340 FOR i=0TOn-1
6350 FORj=0TO1
6360 c=c+ang(i,j)
6370 N=(n+1)*200-(B+c)
6380 A=ATN((M*SINN)/(1+M*COSN))
6390 C=N-A
6400 FOR i=0TOn
6410 B(i)=200-ang(i-1,1)+ang(i,0)
6420 IF i=0 THEN B(0)=200-ang(0,0)+A
6430 IF i=n THEN B(n)=200-ang(n+1,1)+C
6440 NEXT
6450 'calculo de azimut y distancia de los P
6460 FOR i=0 TO n-1
6470 dstp(i,0)=(SINang(i-1,1)*dstp(i-1,0)/SIN ang(i,0)
6480 IF i=0 THEN dstp=(SINA*dst(0))/SIN ang(0,0)
6490 NEXT
6500 FOR i=n-1 TO 0
6510 dstp(i,0)=(SINang(i+2,0)*dstp(i+2,1)/SIN ang(i-1,0)
6520 IF i=0 THEN dstp=(SINC*dst(1))/SIN ang(n-1,1)
6530 NEXT
6540 FOR i=0 TO n-1
6550 dstp(i,2)=(dstp(i,0)+dstp(i,1))/2
6560 NEXT
6570 azi=azi(0)-200
6580 IF azi<0 THEN azi=azi+400
6590 FOR i=0 TOn
6600 azip(i)=azi(i-1)+B(i)
6610 IF i=0 azip(i)=azi+B(i)
6620 IF azip(i)>400 THEN azip(i)=azip(i)-400
6630 NEXT
6640 'calculo de coordenadas de p
6650 CLS:PRINT "Calculo de coordenadas de V"
6660 FOR i=0 TO n-1
6670 z= REC (dstp(i,2),azip(i):coov(i,0)=coo(1,0)+Y:coov(i,1)=coo(1,1)+X
6680 PRINT"X=";coov(i,0);
6690 PRINT"Y=";coov(i,1)
6700 NEXT:NEXT
