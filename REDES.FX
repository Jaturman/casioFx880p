120 a$=INPUT$(1):ON VAL(a$) GOTO4000,4495,2999,9100:IF a$="0" THEN GOTO50
130 GOTO110
140 CLS:PRINT "Limpio memoria....";:FORi=1TO800:NEXT:CLEAR :GOTO10
190 E N T R A D A
200 CLS:PRINT "[ REDES(2D) ]:Itinerario";TAB(0);:INPUT"n de ejes:",n:LOCATE0,0: DIMiti(n+1,13)
210 'cerr=0:GOSUB 990:PRINT "‘[abierta] ’[cerrada]";:a$=INPUT$(1):IF a$="2" THEN cerr=1
215 'entrada LH,LV,Dst
217 CLS:PRINT "[eje AæRef]";TAB(0)"LHdir:";:INPUT "",iti(0,1):LOCATE0,0
218 'cerr=0: GOSUB 990:PRINT " ‘[abierta] ’[cerrada]";:a$=INPUT$(1):IF a$="2" THEN cerr=1
219 'IF a$="1" OR a$="2"THEN GOTO220 ELSE GOTO218
220 FOR i=1TO n
230 RESTORE5000:CLS:READ a$:PRINT "[eje ";CHR$(64+i);"æ";CHR$(65+i);"] ";
240 FOR j=1TO3
245 GOSUB 990:IFj=3 THEN PRINT " <0> calcular,";
250 READ a$:PRINT" ";a$;:INPUT":",iti(i,j):LOCATE0,0:IF iti(i,j)=0 THEN GOSUB4500:iti(i,3)=g*SINlv^2
260 NEXT:NEXT
265 CLS:PRINT "[eje ";CHR$(65+n);"æRef]";TAB(0)"LHdir:";:INPUT "",iti(n+1,1):LOCATE0,0
270 'entrada X,Y vert salida y llegada
280 FOR i= 0 TO 1
290 FOR j= 0TO 1
300 CLS:PRINT CHR$(88+j);CHR$(97+i*(n);:INPUT":",iti(i*(n+1),10+j
310 NEXT:NEXT
315 'entrada de azimutes salida llegada
320 CLS:INPUT "Œsalida:",azsal
330 INPUT "Œllegada:",azlle
390 ' A Z I M U T E S
400 'corrida de azimutes
405 CLS:PRINT "Corrida de azimutes"
410 des=azsal-iti(0,1)
420 PRINT"„A=";azsal;"-";iti(0,1);"=";TAB(0);des
430 FOR i=1 TO n
440 iti(i,4)=des+iti(i,1):IF iti(i,4)<0 THEN iti(i,4)=iti(i,4)+400
445 IF iti(i,4)>400 THEN iti(i,4)=iti(i,4)-400
450 PRINT "Œ"; CHR$(64+i); "æ"; CHR$(65+i); "=";des;"+";iti(i,1);"=";TAB(0);iti(i,4)
460 azrec=iti(i,4)-200:IF azrec<0 THEN azrec=azrec+400
465 IF  azrec>400 THEN azrec=azrec-400
470 PRINT"Œ";CHR$(65+i);"æ"CHR$(64+i);"=";iti(i,4);"ù 200 =";TAB(0);azrec
480 des=azrec-iti(i,2)
490 PRINT"„";CHR$(65+i);"=";azrec;"-";iti(i,2);"=";TAB(0);des
500 NEXT:iti(n+1,4)=des+iti(n+1,1):IF iti(n+1,4)<0 THEN iti(n+1,4)=iti(n+1,4)+400
501 IF iti(n+1,4)>400 THEN iti(n+1,4)=iti(n+1,4)-400
502 PRINT "Œ"; CHR$(65+n); "æ"; CHR$(66+n); "=";des;"+";iti(n+1,1);"=";TAB(0);iti(n+1,4)
505 'calculo del ec azimutal
510 ec=(iti(n+1,4)-azlle)*10000
520 PRINT"ec=";iti(n+1,4);"-";azlle;"=";ec;"cc"
525 'calculo de tolerancia
527 INPUT"ea:",ea:IF ea=0 THEN GOSUB 2999
530 T=ea*SQR(2*n):PRINT "Tolerancia=";T
540 IF ec>T THEN CLS:PRINT"Error de cierre no tolerable"
545 'compensacion azimutal
548 CLS:PRINT "Compensacion de azimutes"
550 FOR i=1 TO n+1
560 corr=(i*ec)/(n+1)*-1
570 PRINT"[eje ";CHR$(64+i);"æ"CHR$(65+i);"]";TAB(0)"corr=";corr;" Œcorr=";
580 iti(i,5)=iti(i,4)+corr/1e4
590 PRINT iti(i,5)
600 NEXT
602 'C O O R D E N A D A S
605 'calculo de coordenadas parciales
608 CLS:PRINT "Calculo de coordenadas parciales"
610 FOR i=1TO n
620 z= REC (iti(i,3),iti(i,5)):iti(i,6)=Y:iti(i,7)=X
630 PRINT"(parciales)";TAB(0)"X";CHR$(65+i);"=D";CHR$(64+i);CHR$(65+i);" SenŒ";CHR$(64+i;CHR$(65+i);"=";Y:PRINT"Y";CHR$(65+i);"=D";CHR$(64+i);CHR$(65+i);" CosŒ";CHR$(64+i);CHR$(65+i)"=";X
650 NEXT
655 'calculo del error de cierre en coordenadas
657 CLS:PRINT "Calculo ec en calculo de coord."
660 sumx=0:sumy=0
670 FOR i=1 TO n
680 sumx=sumx+iti(i,6)
690 sumy=sumy+iti(i,7)
700 NEXT:PRINT "„x=";sumx;TAB(0)"„y=";sumy
710 ecx=sumx-(iti(n+1,10)-iti(0,10)):PRINT "ecx=";ecx;TAB(0);
720 ecy=sumy-(iti(n+1,11)-iti(0,11)):PRINT "ecy=";ecy
725 'calculo de tolerancia
727 CLS:PRINT "Calculo de la Tolerancia";TAB(0);
730 PRINT"‘mira estad. ’est. total";:a$=INPUT$(1):ON VAL(a$) GOTO 735,825
735 'mira
740 sumabx=0:sumaby=0
750 FOR i=1 TO n+1
760 sumabx=sumabx+ABS(iti(i,6)
770 sumaby=sumaby+ABS(iti(i,7)
775 NEXT
780 CLS:PRINT "„x=";sumabx;TAB(0);
790 PRINT "„y=";sumaby
800 Tx=1/300*sumabx:PRINT"Tx=";Tx;TAB(0);
810 Ty=1/300*sumaby:PRINT"Ty=";Ty
820 IF ecx>Tx THEN ecy>Ty THEN CLS:PRINT"Error de cierre no tolerable"
822 GOTO897
825 'estacion
830 sumdst=0
840 FOR i=1 TO n+1
850 sumdst=sumdst+iti(i,4)
860 NEXT
870 PRINT"„Dst=";sumdst
880 exy=SQR(ecx^2+ecy^2)
890 IF exy>1/500*sumdst THEN CLS:PRINT"Error de cierre no tolerable"
895 'compensacion de coordenadas
897 CLS:PRINT "Compensacion de coordenadas"
900 ON VAL(a$) GOTO 905,955
905 'mira
910 FOR i=1 TO n
920 corrx=(iti(i,6)*ecx)/sumx*-1:iti(i,8)=iti(i,6)+corrx
930 corry=(iti(i,7)*ecy)/sumy*-1:iti(i,9)=iti(i,7)+corry
940 PRINTCHR$(65+i);" corrx=";corrx;"  corry=";corry;TAB(0);"Xcorr=";iti(i,8);" Ycorr=";iti(i,9)
950 NEXT:GOTO960
955 FORi=1TOn 'estacion
956 corrx=(iti(i,7)*ecx)/sumy*-1:iti(i,8)=iti(i,6)+corrx
957 corry=(iti(i,6)*ecy)/sumx*-1:iti(i,9)=iti(i,7)+corry
958 PRINTCHR$(65+i);" corrx=";corrx;"  corry=";corry;TAB(0);"Xcorr=";iti(i,8);" Ycorr=";iti(i,9)
959 NEXT
960 'calculo de coordenadas absolutas
962 CLS:PRINT "Calculo de coordenadas absolutas"
970 FOR i=1TOn
975 iti(i,10)=iti(i-1,10)+iti(i,8):iti(i,11)=iti(i-1,11)+iti(i,9)
980 PRINT CHR$(65+i);" X=";iti(i,10);TAB(0);"  Y=";iti(i,11)
985 NEXT:GOTO50
990 LOCATE31,0:PRINT "                                ";:LOCATE31,0:RETURN
1000 LOCATE15,0:PRINT ".Directa     ";:GOSUB 990 'ENTRADA
1010 'entrada de coordenadas conocidas
1020 INPUT " numero de estaciones:",n:GOSUB 990:DIMcoo(n-1,1),lh(n-1,2),ang(n-2,2),azi(2*n-2),dst(n-2),aziv(n-1,1),dstv(n-1,1),coov(n-1,1)
1030 FOR i=0 TO n-1
1040 PRINT " X";CHR$(65+i);:INPUT":",coo(i,0):GOSUB 990
1050 PRINT " Y";CHR$(65+i);:INPUT":",coo(i,1):GOSUB 990
1060 NEXT
1070 'entrada de lecturas azimutales
1080 FOR i=0TOn-1
1090 FOR j=0TO2
1100 IF (i=0 AND j=0) OR (i=n-1 AND j=2)THEN1140
1110 GOSUB 990:PRINT" LH";CHR$(65+i);"æ";
1120 IF j=1 THEN PRINT"V"; ELSE PRINT CHR$(64+i+j);
1130 INPUT":", lh(i,j):LOCATE0,0
1140 NEXT:NEXT 
1150 'CALCULO 
1160 'calculo de angulos
1165 CLS:PRINT "Calculo de angulos"
1170 FOR i=0 TO n-2
1180 FOR j=0 TO 1
1190 ang(i,j)=lh(i+j,1-j)-lh(i+j,2-j):IF ang(i,j)>200 THEN 400-ang(i,j)=ang(i,j)
1200 b$="":c$="-":IFlh(i+j,1-j)-lh(i+j,2-j)>0 THEN b$="-":c$="+"
1210 SETn:PRINT CHR$(136+j);i+1;"=";b$;"LH";CHR$(97+i+j);CHR$(98+i-j);c$;"LH";CHR$(97+i+j);"v=";:SETF3:PRINT ABS(ang(i,j))
1220 NEXT:ang(i,2)=200-ABS(ang(i,0))-ABS(ang(i,1):SETn:PRINT "v";i+1;"=200-(";CHR$(136);i+1;"+";CHR$(137);i+1;")=";:SETf3:PRINT ang(i,2):NEXT
1230 'calculo de azimutes y distancias por diferencia de coordenadas conocidas
1235 CLS:PRINT "Calculo de dist y azi entre vert"
1240 FOR i=0 TO n-2
1250 z=  POL (coo(i+1,0)-coo(i,0),coo(i+1,1)-coo(i,1):dst(i)=X:azi(i)=100-Y
1255 c$="":IFcoo(i+1,1)-coo(i,1)<0THENc$="+200" ELSE IFcoo(i+1,0)-coo(i,0)>0 THEN c$="+400"
1260 b$="":IF azi(i)<0 THEN azi(i)=azi(i)+400:b$="+400"
1270 IF azi(i)>400 THEN azi(i)=azi(i)-400:b$="-400"
1280 PRINT"D";CHR$(97+i);"æ";CHR$(98+i);"=‚((X";CHR$(98+i);"-X";CHR$(97+i);")’+(Y";CHR$(98+i);"-Y";CHR$(97+i);")’)=";TAB(0);dst(i)
1290 PRINT"Œ";CHR$(97+i);"æ";CHR$(98+i);"=arctg((X";CHR$(98+i);"-X";CHR$(97+i);")/(Y";CHR$(98+i);"-Y";CHR$(97+i);"))";c$;b$;TAB(0);"=";azi(i) :NEXT
1300 'calculo de azimutes (estæV)
1305 CLS:PRINT "Calculo de azimutes"
1310 FOR i=0TOn-1
1320 FORj=0TO1:IF (i=0 AND j=0)OR(i=n-1 AND j=1) THEN1380
1330 azi=azi(i+j-1)+200*(1-j)
1340 IF azi>400 THEN azi=azi-400
1345 IF azi<0 THEN azi=azi+400
1350 IF j=1 THEN k=1 ELSE k=-1
1360 aziv(i,j)=azi+ang(i+j-1,1-j)*k:IF aziv(i,j)<0 THEN aziv(i,j)=aziv(i,j)+400
1365 IF aziv(i,j)>400 THENaziv(i,j)=aziv(i,j)-400
1370 PRINT "Œ";CHR$(97+i);"æ";"v=Œ";CHR$(97+i);CHR$(96+i +2*j);:IF SGN(k)=-1 THEN PRINT"+"; ELSE PRINT"-";
1375 SETn:PRINTCHR$(136+(1-j));i+j;"=";:SETf3:PRINTaziv(i,j)
1380 NEXT:NEXT 
1390 'calculo de distancias (estæV)
1395 CLS:PRINT "Calculo de distancias"
1400 FOR i=0 TO n-1
1410 FOR j=0TO1:IF (i=0 AND j=0)OR (i=n-1 AND j=1) THEN1440
1420 dstv(i,j)=ABS( (dst(i+j-1)*SINang(i+j-1,j))/SIN ang(i+j-1,2))
1430 SETN:PRINT "D";CHR$(97+i);"æ";"v=(D";CHR$(96+i+j);CHR$(97+i+j);"Sen";CHR$(136+j);i+j;")/SenV";(i+j);"=";:SETF3:PRINT TAB(0);dstv(i,j)
1440 NEXT:NEXT 
1450 'calculo de coordenadas de V
1455 CLS:PRINT "Calculo de coordenadas de V"
1460 FOR i=0 TO n-1
1470 FOR j=0TO1:IF (i=0 AND j=0)OR (i=n-1 AND j=1) THEN1510
1480 z= REC (dstv(i,j),aziv(i,j):coov(i,0)=coo(i,0)+Y:coov(i,1)=coo(i,1)+X
1490 PRINT"(parciales)";TAB(0)"X";CHR$(96+i+j);"v=D";CHR$(96+i+j);"v Sen Œ";CHR$(96+i+j);"v=";Y
1500 PRINT"Y";CHR$(96+i+j);"v=D";CHR$(96+i+j);"v Cos Œ";CHR$(96+i+j);"v=";X
1502 PRINT"(absolutas)";TAB(0)"Xv=X";CHR$(96+i+j);"+x";CHR$(96+i+j);"v =";coov(i,0
1504 PRINT"Yv=Y";CHR$(96+i+j);"+y";CHR$(96+i+j);"v =";coov(i,1
1510 NEXT:NEXT:GOTO80
2997 GOSUB 2998:GOTO110
2998  digi=0:CLS:PRINT"[ REDES(2D) ]: calculo ea    ";:GOSUB 990:PRINT " Estacion total: ‘[si] ’[no]";:a$=INPUT$(1):IF a$="1" THEN digi=1
2999 IF a$="1" OR a$="2" THEN 3000 ELSE 2998
3000 SETn:GOSUB 990:PRINT " Plomada ‘[optica] ’[fisica]";:a$=INPUT$(1):IF a$="1" THEN eees=.025
3005 IF a$="2" THEN eees=.05
3006 IF a$="1" OR a$="2" THEN a$="" ELSE GOTO3000
3010 GOSUB990:PRINT" (";ap;")ap:";:INPUT"",ap
3100 GOSUB 990:PRINT" (";s;:INPUT ")s:",s:GOSUB990:PRINT" (";A;:INPUT")A:",A:GOSUB990:PRINT" (";dst;:INPUT")Distancia:",dst
3230  ev=ROUND(1/12*s,-3):ep=ROUND(150/A*(1+4*A/150),-3):CLS:PRINT"ev=";
3240  PRINT"=1/12¥";s;"=";ev;"cc","ep=150/";A;"(1+4¥";A;"/150)=";ep;"cc"
3250 ed=ROUND(  (eees/dst)*636620,-3):PRINT "ed=";eees;"/";dst;" r=";ed;"cc"
3260 IFdigi=1THENel=ROUND(ap,-3)ELSE el=ROUND(2/3*ap,-3)
3270 PRINT "el=";:IF digi=1 THENPRINT ap;")=";el;"cc"ELSE PRINT "2/3";ap;")=";el;"cc"
3280  ea=ROUND(  SQR(ev^2+el^2+ep^2+ed^2),-3)  :CLS:PRINT "ea=‚(ev’+ed’+ep’+el’)=";ea;"cc":RETURN
4000 CLS:i=0:PRINT "[ REDES(2D) ]:ŒDäæXY         ";:GOSUB 990:PRINT " ‘[escribir]     ’[ŒyD] “[coord]";:a$=INPUT$(1):ON VAL(a$)GOTO 4020,4099,4199:IF a$="0" THEN110
4010 GOTO4000
4020 IF n=0 THEN CLS:INPUT "n de ptos:",n:DIM x(n):DIM  y(n)
4030 CLS:i=i+1:SETn:PRINT "[punto][";i;"]:";:INPUT"",i:IF i=0THEN4000
4040 CLS:SETf4:    PRINT "(";x(i);")[X][";:SETn:PRINT   i;"]:";:INPUT "",x(i):SETf4: PRINT "(";y(i);")[Y][";:SETn:PRINT   i;"]:";:INPUT "",y(i):GOTO4030
4099 IF n=0THENCLS:PRINT "no dim !!!";:FOR i=1TO200:NEXT:GOTO4000
4100 CLS:INPUT "[pto]:",a:IF a=0THEN4000
4105 SETf4:LOCATE8,0:PRINT x(a);",";y(a);TAB(0);:INPUT "[pto]:",b:LOCATE8,1:PRINT x(b);",";y(b);:LOCATE0,0:STOP
4110 z=  POL (x(b)-x(a) ,y(b)-y(a) ):dst=X:azi=100-Y:IF azi<400 THEN azi=azi+400
4115 c$="":IFy(b)-y(a)<0THENc$="+200" ELSE IFx(b)-x(a)<0 THEN c$="+400"
4120 IF azi>400 THEN azi=azi-400:b$="-400"
4122 PRINT"Œ";CHR$(48+a);"æ";CHR$(48+b);"=arctg((X";CHR$(48+b);"-X";CHR$(48+a);")/(Y";CHR$(48+b);"-Y";CHR$(48+a);"))";c$;b$;TAB(0);"=";azi
4125 PRINT"D";CHR$(48+a);"æ";CHR$(48+b);"=‚((X";CHR$(48+b);"-X";CHR$(48+a);")’+(Y";CHR$(48+b);"-Y";CHR$(48+a);")’)=";TAB(0);dst
4130 CLS:SETn:PRINT "[Dst][";a;"æ";b;"]=";:SETf4:PRINT   dst;"m";TAB(0);:SETn:PRINT "[ Œ ][";a;"æ";b;"]=";:SETf4:PRINT azi:GOTO 4100
4199 IF n=0THENCLS:PRINT "no dim !!!";:FOR i=1TO200:NEXT:GOTO4000
4200 CLS:INPUT "[pto]:",a:IF a=0THEN4000
4210 LOCATE8,0:PRINT x(a);",";y(a);TAB(0);:INPUT "[pto]:",b
4220 CLS:PRINT "(";az;:SETn:PRINT  ")[Œ";a;"æ";b;"]:";:INPUT "", az:SETf4: PRINT "(";ds;:SETn:PRINT ")[D";a;"æ";b;"]:";:INPUT "",ds
4230 z= REC (ds,az):x(b)=x(a)+Y:y(b)=y(a)+X
4240 PRINT"(parciales)";TAB(0)"X";CHR$(65+i);CHR$(66+i);"=D";CHR$(65+i);CHR$(66+i);" Sen Œ";CHR$(65+i;CHR$(66+i);"=";Y:PRINT"Y";CHR$(65+i);CHR$(66+i);"=D";CHR$(65+i);CHR$(66+i);" COS Œ";CHR$(65+i);CHR$(66+i)"=";X
4245 x(b)=x(a)+X:y(b)=y(a)+Y
4250 PRINT"(absolutas)";TAB(0)"Xv=X";CHR$(65+i);"+x";CHR$(65+i);CHR$(66+i);" =";x(b:PRINT"Yv=Y";CHR$(65+i);"+y";CHR$(65+i);CHR$(66+i);" =";y(b
4260 CLS:PRINT "[X";b;"]=";:SETf4:PRINT x(b);"m"; TAB(0)"[Y";:SETn:PRINT   b;"]=";:SETf(4):PRINT  y(b);"m"  :GOTO4200
4495 GOSUB 4500:GOTO110
4500 LOCATE 14,0:PRINT "Estadimetria     ";:GOSUB 990:INPUT" <0> calcular, Generador:",g:IF g=0 THEN GOSUB 990:INPUT" Hilo Sup:",hs:GOSUB 990:INPUT " Hilo Inf:",hi:GOSUB 990:g=100*(hs-hi):PRINT " G=100(Hs-Hi)=";g
4510 GOSUB 990:INPUT " LV:",lv:GOSUB 990:PRINT " D=G Sen’LV=";g*SINlv^2:RETURN
4999 ' programa redes                  vol 17500B
5000 DATA Eje,LHdir,LHrec,Dst,Œ,Œcorr,x,y,xcorr,ycorr,X,Y
6000 'entrada de coordenadas
6005 SETN:LOCATE14,0:PRINT "Pothenot        ";:GOSUB 990
6010 INPUT " numero de estaciones:",n:CLS:DIMcoo(2,1),lh(n-1,2),ang(n-1,1),dst(2),azi(2),B(n),dstp(n+1),azip(n+1),coov(n+1,1)
6020 FOR i=0 TO 2
6030 PRINT "X";CHR$(65+i);:INPUT":",coo(i,0)
6040 PRINT "Y";CHR$(65+i);:INPUT":",coo(i,1):CLS
6050 NEXT
6060 'entrada de lecturas
6070 FOR i=0 TO n-1
6080 FOR j= 0TO2
6090 PRINT"LHp";CHR$(49+i);: IF j=1 THEN PRINT"b:"; ELSE IF i=0 AND j=0 THEN PRINT "a:"; ELSE IF i=n-1 AND j=2 THEN PRINT "c:"; ELSE  PRINT"p";CHR$(48+i+j);":";
6100 INPUT"",lh(i,j)
6110 NEXT:NEXT:CLS
6120 'calculo de angulos
6130 SETf4:FOR i=0TO n-1
6140 FOR j=0 TO1
6150 ang(i,j)=lh(i,j)-lh(i,j+1):IFang(i,j)>200THEN ang(i,j)=400-ang(i,j)
6155 b$="":c$="-":IF lh(i,j)-lh(i,j+1)<0 THEN b$="-":c$="+"
6160 PRINT CHR$(136+j);CHR$(49+i);"=";b$;"LHp";CHR$(49+i);:IF i=0 AND j=0 THENPRINT "a";c$;"LHp1b="; ELSE IF i=n-1 AND j=1 THEN PRINT "b";c$;"LHp1c="; ELSE IF j=0 THENPRINT c$;"LHp";CHR$(48+i);"b="; ELSE PRINT c$;"LHbp";CHR$(50+i);"b=";
6162 IF ang(i,j)>200 THEN ang(i,j)=ang(i,j)-400
6165 PRINT ABS(ang(i,j))
6170 NEXT:NEXT
6180 'calculo de azimutes y distancias por diferencia de coordenadas conocidas
6190 CLS:PRINT "Calculo de dist y azi entre vert"
6200 FOR i=0 TO 1
6210 z=  POL (coo(i+1,0)-coo(i,0),coo(i+1,1)-coo(i,1):dst(i)=X:azi(i)=100-Y
6215 c$="":IFcoo(i+1,1)-coo(i,1)>0THENc$="+200" ELSE IFcoo(i+1,0)-coo(i,0)>0 THEN c$="+400"
6220 b$="":IF azi(i)<0 THEN azi(i)=azi(i)+400:b$="+400"
6230 IF azi(i)>400 THEN azi(i)=azi(i)-400:b$="-400"
6240 PRINT"D";CHR$(97+i);"æ";CHR$(98+i);"=‚((X";CHR$(98+i);"-X";CHR$(97+i);")’+(Y";CHR$(98+i);"-Y";CHR$(97+i);")’)=";TAB(0);dst(i)
6250 PRINT"Œ";CHR$(97+i);"æ";CHR$(98+i);"=arctg((X";CHR$(98+i);"-X";CHR$(97+i);")/(Y";CHR$(98+i);"-Y";CHR$(97+i);"))";c$;b$;TAB(0);"=";azi(i)
6260 NEXT
6270 'calculo de los angulos ABC
6275 CLS:PRINT "Calculo de los angulos A,B,C"
6277 azi=azi(0)-200:IF azi<0 THEN azi=azi+400
6280 B=(-azi+azi(1)):IF B<400 THEN B=B+400
6285 k=SGN(B):B=ABS(B):PRINT "B=Œbc-Œac=";B
6290 a=1:b=1:FOR i=0 TOn-1
6300 a=a*SIN(ang(i,0)
6310 b=b*SIN(ang(i,1)
6320 NEXT
6330 M=dst(1)/dst(0)*a/b:PRINT "M=(Dbc";:FORi=1TOn:PRINT "Senˆ";CHR$(48+i);:NEXT:PRINT")/";TAB(0);"(Dab";:FORi=1TOn:PRINT "Sen‰";CHR$(48+i);:NEXT:PRINT ")="; M
6340 c=0:FOR i=0TOn-1
6350 FORj=0TO1
6360 c=c+ABS(ang(i,j)
6365 NEXT:NEXT
6370 N=(n+1)*200-(B+c):PRINT "N=";(n+1)*200;"+(B";:FORi=0TO1:FORj=1TOn:PRINT "+";CHR$(136+i);CHR$(48+j);:NEXT:NEXT:PRINT ")=";N
6380 A=ATN((M*SINN)/(1+M*COSN)):b$="":IF A<0 THENA=A+200:b$="+200"
6385 PRINT "A=ArcTg((M SenN)/(1+M CosN))";b$;"=   ";A
6390 C=N-A:PRINT "C=N-A=";C
6400 FOR i=0TOn
6420 IF i=0 THEN B(0)=200-(ABS(ang(0,0))+A):PRINT "B1=200-(ˆ1+A)";:GOTO 6440
6430 IF i=n THEN B(n)=200-(ABS(ang(n-1,1))+C):PRINT "B";CHR$(49+n);"=200-(‰";CHR$(48+n);"+C)";:GOTO6440
6435 B(i)=200-(ABS(ang(i-1,1))+ABS(ang(i,0)):PRINT "B";CHR$(49+i);"=200-(‰";CHR$(48+i);"+ˆ";CHR$(49+i);
6440 PRINT "=";B(i):NEXT
6450 'calculo de azimut y distancia de los P
6455 CLS:PRINT "Calculo de Œ,D"
6460 FOR i=1 TO n
6470 IF i=1 THEN dstp(1)=ABS((SINA*dst(0))/SIN ang(0,0):GOTO 6490
6480 dstp(i)=ABS((SINang(i-2,1)*dstp(i-1)/SIN ang(i-1,0)
6490 PRINT "Dp";CHR$(48+i);" =";dstp(i):NEXT
6570 dstp(0)=ABS( (dst(0)*SINB(0))/SINang(0,0):PRINT "Dap1=(Dab Sen B1)/SINˆ1=";TAB(0);dstp(0)
6580 dstp(n+1)=ABS( (dst(1)*SINB(n))/SINang(n-1,1):PRINT "Dcp";CHR$(48+n);"=(Dbc Sen B";CHR$(49+n);")/Sen‰";CHR$(48+n);"=";TAB(0);dstp(n+1)
6590 CLS:PRINT "Œba=";azi:FOR i=1 TOn
6600 IF i=1 THEN azip(i)=azi-B(i-1)*k:PRINT "Œbp1=Œbc";CHR$(ABS(k-1)+43);"B";CHR$(49+n);:GOTO 6620
6610 azip(i)=azip(i-1)-B(i-1)*k :PRINT "Œbp";CHR$(49+i);"=Œbp";CHR$(48+i);CHR$(ABS(k-1)+43);"B";CHR$(49+i);
6620 b$="":IF azip(i)>400 THEN azip(i)=azip(i)-400:b$="-400"
6625 IF azip(i)<0 THEN azip(i)=azip(i)+400:b$="+400"
6630 PRINT b$;"=";azip(i):NEXT
6632 azip(0)=azi(0)+A*k:PRINT "Œap1=Œab";CHR$(ABS(k-1)+43);"A=";azip(0)
6634 azi=azi(1)-200:IF azi<0 THENazi=azi+400
6636 b$="":azip(n+1)=azi-C*k:IF azip(n+1)<0 THEN azip(n+1)=azip(n+1)+400:b$="+400"
6638 PRINT "Œcp";CHR$(48+n);"=Œcb";CHR$(ABS((-1*k)-1)+43);"C";b$;"=";azip(n+1)
6640 'calculo de coordenadas de p
6650 CLS:PRINT "Calculo de coordenadas de V"
6660 FOR i=0 TO n+1
6665 IF i=0 THEN a=0 ELSE IF i=n+1 THEN a=2 ELSE a=1
6670 z= REC (dstp(i),azip(i):coov(i,0)=coo(a,0)+Y:coov(i,1)=coo(a,1)+X
6680 PRINT"(parciales)";TAB(0)"X";CHR$(65+i);"v=Dp";          ;CHR$(65+i);" Sen Œp";         ;CHR$(65+i);"=";Y:PRINT"Y";CHR$(65+i);"v=D";CHR$(64+i);CHR$(65+i);" Cos Œ";CHR$(64+i);CHR$(65+i)"=";X
6690 PRINT"(absolutas)";TAB(0)"Xv=X";CHR$(64+i);"+x";CHR$(64+i);CHR$(65+i);" =";coov(i,0:PRINT"Yv=Y";CHR$(64+i);"+y";CHR$(64+i);CHR$(65+i);" =";coov(i,1
6700 NEXT:GOTO80
7000 LOCATE14,0:PRINT "Hansen        ";:GOSUB 990: DIM coo(1,1),lh(1,2),ang(1,1),angg(3),dstp(1,1),azip(1,1),coop(1,1)
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
7185 c$="":IFcoo(1,1)-coo(0,1)<0THENc$"+200" ELSE IFcoo(1,0)-coo(0,0)>0 THEN c$="+400"
7190 IF azi<400 THEN azi=azi+400
7200 IF azi>400 THEN azi=azi-400
7210 PRINT"D";CHR$(65);"æ";CHR$(66);"=‚((X";CHR$(66);"-X";CHR$(65);")’+(Y";CHR$(66);"-Y";CHR$(65);")’)=";TAB(0);dst(i)
7220 PRINT"Œ";CHR$(65);"æ";CHR$(66);"=arctg((X";CHR$(66);"-X";CHR$(65);")/(Y";CHR$(66);"-Y";CHR$(65);"))";c$;b$;TAB(0);"=";azi(i) :NEXT
7230 'calculo de angulos por diferencia de lecturas
7235 CLS:PRINT "Calculo de angulos"
7240 RESTORE 7310
7250 FOR i=0TO1
7260 FOR j=0TO1
7270 READ a$
7280 ang(i,j)=lh(i,j+1)-lh(i,j):b$="":c$="-":IFlh(i,j+1)-lh(i,j)<0 THEN b$="-":c$="+"
7282 IF ang(i,j)<0 THEN ang(i,j)=400+ang(i,j)
7285 IFang(i,j)>200 THEN ang(i,j)=400-ang(i,j)
7290 PRINT CHR$(136+j);CHR$(49+i;"=";b$;"LHp";CHR$(49+i;a$;c$;"LHp";CHR$(49+i;:READa$:PRINT a$;"=";ang(i,j)
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
7420 dstp(0,0)=(dst*SIN angg(3))/SIN ang(0,0):PRINT "Dap1=(Dab Š4)/Senˆ1=";dstp(0,0)
7430 dstp(0,1)=(dst*SIN (angg(0)+angg(2)))/SIN ang(0,0):PRINT "Dbp1=(Dab(Š1+Š3)/Senˆ1=";dstp(0,1)
7440 dstp(1,1)=(dst*SIN angg(2))/SIN ang(1,1):PRINT  "Dap2=(Dab(Š4+Š2))/Sen‰2=";dstp(1,1)
7450 dstp(1,0)=(dst*SIN (angg(3)+angg(1)))/SIN ang(1,1):PRINT  "Dbp2=(Dab Š3)/Sen‰2=";dstp(1,0)
7455 azii=azi-200:IF azii<0 THEN azii=azii+400
7460 azip(0,0)=azi+angg(2)+angg(0):PRINT"Œap1=Œab+Š3+Š1=";azip(0,0)
7470 azip(0,1)=azii-angg(3):PRINT "Œbp1=Œba-Š4=";azip(0,1)
7480 azip(1,0)=azi+angg(2):PRINT "Œap2=Œab+Š3=";azip(1,0)
7490 azip(1,1)=azii-angg(3)-angg(1):PRINT "Œbp2=Œba-Š4-Š2=";azip(1,1)
7500 'calculo de coordenadas
7510 CLS:PRINT "Calculo de coordenadas de V"
7520 FOR i=0 TO 1
7530 FOR j=0TO1
7540 z= REC (dstp(i,j),azip(i,j):coop(i,0)=coo(j,0)+Y:coop(i,1)=coo(j,1)+X:PRINT dstp(i,j);azip(i,j)
7550 PRINT"(parciales)";TAB(0)"X";CHR$(64+i);"=D";CHR$(64+i);"p";CHR$(50+j);" Sen Œ";CHR$(64+i;CHR$(64+i);"=";Y:PRINT"Y";CHR$(64+i);"v=D";CHR$(64+i);"p";CHR$(50+j);" Cos Œ";CHR$(64+i);CHR$(65+i)"=";X
7560 PRINT"(absolutas)";TAB(0)"Xv=X";CHR$(64+i);"+x";CHR$(64+i);CHR$(65+i);" =";coov(i,0:PRINT"Yv=Y";CHR$(64+i);"+y";CHR$(64+i);CHR$(65+i);" =";coov(i,1
7570 NEXT:NEXT:GOTO80
8000 'entrada de lecturas
8005 LOCATE15,0:PRINT "Radiacion";:GOSUB 990
8010 INPUT " n de puntos visados:",n:CLS: n=n+1:DIM dst(n-1),azi(n-1),lh(n-1),coo(n-1,1)
8015 INPUT "(0 si „=0), Œpæref:",azi
8020 FOR i=0 TO n-1
8030 IF i=0THENPRINT "(0 si „=0), LHpæref";ELSEPRINT "LHpæ";CHR$(96+i);
8035 INPUT ":",lh(i)
8036 IFi<>0THEN PRINT "<0> calcular, Dpæ";CHR$(96+i);:INPUT ":",dst(i):IF dst(i)=0 THEN GOSUB 4500:dst(i)=g*SINlv^2
8040 CLS:NEXT
8050 INPUT"Xp:",xa:INPUT "Yp:",ya
8060 CLS:PRINT "Calculo de la radiacion":des=azi-lh(0):PRINT "„=Œpæref-LHpæref=";des:CLS
8070 FOR i=1TOn-1
8080 azi(i)=lh(i)+des:PRINT "Œpæ";CHR$(96+i);"=LHpæ";CHR$(96+i);"+„=";azi(i)
8090 IF azi(i)<0 THEN azi(i)=azi(i)+400
8100 NEXT
8110 'calculo de las coord de los ptos radiados
8115 PRINT"(relativas)   x=D SenŒ";TAB(14);"y=D CosŒ"
8120 CLS:FORi=1TOn-1
8130 z=REC(dst(i),azi(i)):coo(i,0)=xa+Y:coo(i,1)=ya+X
8135 PRINT"(relativas)   x";CHR$(96+i);"=";Y;TAB(14);:PRINT"y";CHR$(96+i);"=";X
8140 PRINT"(absolutas)  X";CHR$(96+i);"=x";CHR$(96+i);"+Xp=";coo(i,0);TAB(13);:PRINT"Y";CHR$(96+i);"=y";CHR$(96+i);"+Yp=";coo(i,1)
8150 NEXT:GOTO10
9000 'excentricas
9005 LOCATE14,0:PRINT "Excentrica";:GOSUB 990
9010 'entrada de datos
9020 INPUT" LHa'æa:",laa:INPUT "LHa'æb:",lab:INPUT "Da'æa:",d:INPUT "Da'æb:",D
9030 'calculo
9040 DD=SQR(D^2+d^2-2*D*d*COS(lab-laa)):PRINT "Daæb=";DD
9050 a=(d*636620*SIN(lab-laa))/DD:PRINT "a'ba=";a;"cc"
9060 lhab=lab+a/1E4:PRINT "LHaæb=";lhab
9100 'calculo de superficies
9105 LOCATE14,0:PRINT "Superficies   ";:GOSUB 990
9110 'entrada de puntos
9120 INPUT" n de vertices:",n:CLS
9130 DIM coo(n-1,1):FORi=0TOn-1:FORj=0TO1:PRINT CHR$(88+j);CHR$(49+i);:INPUT ":",coo(i,j)
9140 NEXT:CLS:NEXT
9150 'calculo de superficie
9160 s=0:PRINT "2S=";:FORi=0TOn-1
9165 IFi=0 THEN a=n-1 ELSE a=i-1
9167 IFi=n-1 THEN b=0 ELSE b=i+1
9170 s=s+coo(i,0)*(coo(a,1)-coo(b,1)
9175 PRINT "X";CHR$(49+i);"(Y";CHR$(49+a);"-Y";CHR$(49+b);")";:IF i<>n-1 THEN PRINT "+";
9178 IF i=5 OR i=10 THEN STOP
9180 NEXT
9190 PRINT "=";s:CLS:PRINT "Superficie=";s/2:GOTO110
