7000 CLS:PRINT "Metodo HANSEN": DIM coo(1,1),lh(1,2),ang(1,1),angg(3),dstp(1,1),azip(1,1),coop(1,1)
7010 'entrada de coordenadas
7020 FOR i=0 TO 1
7030 PRINT " X";CHR$(65+i);:INPUT":",coo(i,0):GOSUB 990
7040 PRINT " Y";CHR$(65+i);:INPUT":",coo(i,1):GOSUB 990
7050 NEXT
7060 'entrada de lecturas
7070 FOR i=0 TO1
7080 RESTORE (7140+i)
7090 FOR j=0 TO 2
7100 READ a$
7110 PRINT" LHp";CHR$(49+i);"æ";a$;
7120 INPUT ":",lh(i,j)
7130 NEXT:NEXT
7140 DATA a,b,p2
7141 DATA p1,a,b
7150 'calculo de distancia y azimut por diferencia de coord
7160 CLS:PRINT "Calculo de dist y azi entre vert"
7180 z=  POL (coo(1,0)-coo(0,0),coo(1,1)-coo(0,1):dst=X:azi=100-Y
7190 IF azi<400 THEN azi=azi+400
7200 IF azi>400 THEN azi=azi-400
7210 PRINT"Daæb=‚((Xb-Xa)’+(Yb-Ya)’)=";TAB(0);dst
7220 PRINT"Œaæb=arctg((Xb-Xa)/(Yb-Ya))=";TAB(0);azi
7230 'calculo de angulos por diferencia de lecturas
7235 CLS:PRINT "Calculo de angulos"
7240 RESTORE 7310
7250 FOR i=0TO1
7260 FOR j=0TO1
7270 READ a$
7280 ang(i,j)=lh(i,j+1)-lh(i,j):IF ang(i,j)<0 THEN ang(i,j)=400+ang(i,j)
7285 IFang(i,j)>200 THEN ang(i,j)=400-ang(i,j)
7290 PRINT CHR$(136+j);CHR$(49+i;"=LHp";CHR$(49+i;a$;"-LHp";CHR$(49+i;:READa$:PRINT a$;"=";ang(i,j)
7300 NEXT:NEXT
7310 DATA b,a,p2,b,a,p1,b,a
7320 'calculo de los otros angulos
7330 M=(SINang(0,1)*SINang(1,1)*SIN(ang(0,0)+ang(0,1)+ang(1,0)))/(SINang(0,0)*SINang(1,0)*SIN(ang(0,1)+ang(1,0)+ang(1,1)))
7335 PRINT "M=  (Sen‰1 Sen‰2 Sen(ˆ1+‰1+ˆ2)/ (Senˆ1 Senˆ2 Sen(‰1+ˆ2+‰2)","M=";M
7340 N=ang(0,1)+ang(1,0):PRINT "N=‰1+ˆ2=";N
7350 I=1+M*COSN:PRINT"I=1+M CosN=";I
7360 J=M*SINN:PRINT "J=M SenN=";J
7370 angg(2)=ATN(J/I):PRINT "Š3=ArcTg(J/I)=";angg(2)
7380 angg(3)=N-angg(2):PRINT "Š4=N-Š3=";angg(3)
7390 angg(0)=200-(ang(0,0)+ang(1,0)+ang(0,1):PRINT "Š1=200-(ˆ1+ˆ2+‰1)=";angg(0)
7400 angg(1)=200-(ang(0,1)+ang(1,1)+ang(1,0):PRINT "Š2=200-(‰1+‰2+ˆ2)=";angg(1)
7410 'calculo de azimutes y distancias
7420 dstp(0,0)=(dst*SIN angg(3))/SIN ang(0,0):PRINT dstp(0,0)
7430 dstp(0,1)=(dst*SIN (angg(0)+angg(2)))/SIN ang(0,0):PRINT dstp(0,1)
7440 dstp(1,1)=(dst*SIN angg(2))/SIN ang(1,1):PRINT dstp(1,1)
7450 dstp(1,0)=(dst*SIN (angg(3)+angg(1)))/SIN ang(1,1):PRINT dstp(1,0)
7455 azii=azi-200:IF azii<0 THEN azii=azii+400
7460 azip(0,0)=azi+angg(2)+angg(0):PRINT azip(0,0)
7470 azip(0,1)=azii-angg(3):PRINT azip(0,1)
7480 azip(1,0)=azi+angg(2):PRINT azip(1,0)
7490 azip(1,1)=azii-angg(3)-angg(1):PRINT azip(1,1)
7500 'calculo de coordenadas
7510 CLS:PRINT "Calculo de coordenadas de V"
7520 FOR i=0 TO 1
7530 FOR j=0TO1
7540 z= REC (dstp(i,j),azip(i,j):coop(i,0)=coo(j,0)+Y:coop(i,1)=coo(j,1)+X:PRINT dstp(i,j);azip(i,j)
7550 PRINT"X=";coop(i,0);
7560 PRINT"Y=";coop(i,1)
7570 NEXT:NEXT
