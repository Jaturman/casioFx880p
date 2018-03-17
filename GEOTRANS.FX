1 ON ERROR GOTO20000:PRINT "( ?";TAB(0);"SN error P9-10";TAB(0); :ojo$=INPUT$(2):IF ojo$="+-" THEN 9 ELSE END
2 '    GeoTrans 1.00 may97.Temon & Niko
3 i=0:ang=ROUND(ang*3600,-(rea+1))/3600
4 i=i+1: b$=MID$(DMS$(ang),i,1:IF i>11 THEN 6
5 IF b$="." THEN 7 ELSE4
6 i=LEN(DMS$(ang))
7 ang$=LEFT$(DMS$(ang),i)+MID$(STR$(FRAC(ang*3600),4,rea+1)
8 RETURN
9 ojo$="":CLS:ANGLE0:PRINT TAB(0);:IF alfa0=0THENCLEAR:DIMent(5),sal(3),sal$(3):a=6378137:e=.08181919:rea=3:red=3
10 CLS:PRINT"GeoTrans 1.00 may97";TAB(0)">ëDir,íInv,ìEli,îRnd,ïMet ?";:pro$=INPUT$(1)
40 ON VAL(pro$) GOSUB 60,70,5006,5000,5300:IF pro$="0" THEN ojo$="":CLS:PRINT "fin de GeoTrans......";:FORi=1TO300:NEXT:CLS:END
50 GOTO 10
60 if met=0 then gosub 100 else gosub600
70 if met=0 then gosub 1000 else gosub800
80 CLS:PRINT "Problema resuelto !";:FORi=1TO400:NEXT:goto10
100 CLS:PRINT "Problema Directo Pto H";TAB(0);"e=";e;"a=";a
110 GOSUB 5010
114 CLS:PRINT TAB(32);:IF ent(3)<=90 THEN alfa0=ent(3):PRINT "à0=àP1=";:GOTO120
116 IF ent(3)<=180 THEN alfa0=180-ent(3):PRINT "à0=180-àp1=";:GOTO120
118 IF ent(3)<=270 THEN alfa0=ent(3)-180:PRINT "à0=àp1-180=";:GOTO120
119 alfa0=360-ent(3):PRINT "à0=360-àp1=";
120 ang=alfa0:GOSUB2:alfa0=ang:alfa0$=ang$:PRINT alfa0$
125 CLS:PRINT "Calculando coordenadas...";:lat=ent(1):GOSUB 5100:exc=ent(4)^2*1/(2*N*Ro)*COS alfa0*SIN alfa0
130 exc=exc*180/PI
135 ang=exc:GOSUB2:exc=ang:exc$=ang$
140 x=ROUND(ent(4)*SIN(alfa0-exc/3),-red-1
150 y=ROUND(ent(4)*COS(alfa0-2/3*exc),-red-1
160 CLS:PRINT"x=s Sen(à0-ã/3)=";x;"m";TAB(0)"y=s Sen(à0-2/3ã)=";y;"m","ã=sí/(2NRo) Cosà0 Sen à0=";TAB(0)exc*PI/180;"Ê";exc$:GOSUB 5200:GOSUB5210
170 it=0:ee=0
180 CLS:PRINT "Calculando la latitud";
185 it=it+1:PRINTTAB(0) "(iteracion";it;")....";
190 Ilat=y/Ro*(1-ee^2/(8*a^2)*y^2*COS(2*latm)
200 Ilat=Ilat*180/PI
205 ang=Ilat:GOSUB2:Ilat=ang:Ilat$=ang$
210 IF ent(3)>0 AND ent(3)<90 OR (ent(3)>270AND ent(3)<360)THEN latH=ent(1)+Ilat ELSE latH=ent(1)-Ilat
215 ang=latH:GOSUB2:latH=ang:latH$=ang$
220 latm=(ent(1)+latH)/2
225 ang=latm:GOSUB2:latm=ang:latm$=ang$
230 lat=latm:GOSUB5100
240 CLS:PRINT"Óè=";:IF it=1 THEN PRINT"y/Ro=";ELSEPRINT "y/Ro(y-eí/(8aí) yíCos 2è";
245 PRINTTAB(0);Ilat*PI/180;"Ê";Ilat$,"èH=";:IF latH>ent(1) THEN PRINT "èp1-Óè=";ELSE PRINT "èp1+Óè=";
247 PRINT latH$;TAB(0)"èm=";latm$:GOSUB 5210
250 CLS:PRINT"Otra iteracion ë[si]:";:a$=INPUT$(1):IF a$="1" THEN ee=e:GOTO180
300 lat=latH:CLS:PRINT"é/Ro=xí/(2Ní) TAN èH=";TAB(0);
310 SigRo=x^2/(2*N^2)*TAN latH
320 SigRo=SigRo*180/PI
330 ang=SigRo:GOSUB2:SigRo=ang:SigRo$=ang$
340 sal(1)=latH-SigRo
350 ang=sal(1):GOSUB2:sal(1)=ang:sal$(1)=ang$
360 PRINTSigRo*PI/180;"Ê";SigRo$,"èp2=èH-é/Ro=";sal$(1);TAB(0);:STOP
390 CLS:PRINT "Calculando la longitud...";
400 lat=sal(1)
410 GOSUB5100: Ilon=ASN(SIN((x/N)*180/PI))*1/COSsal(1)
415 ang=Ilon:GOSUB2:Ilon=ang:Ilon$=ang$
420 IF ent(3)>0 AND ent(3)<180 THEN sal(2)=ent(2)+Ilon ELSEsal(2)=ent(2)-Ilon
425 ang=sal(2):GOSUB2:sal(2)=ang:sal$(2)=ang$
430 CLS:PRINT"ÓΩ=ASN(Sen(x/N)*SIN èp2)=";TAB(0);Ilon$,"x/Np2=";TAB(0);x/N;"Ê";DMS$((x/N)*180/PI):GOSUB5200
435 PRINT"Ωp2=";:IF ent(3)>0 AND ent(3)<180 THEN PRINT "Ωp1+ÓΩ=";ELSE PRINT "Ωp1-ÓΩ=";
437 PRINTsal$(2);TAB(0);:STOP
440 CLS:PRINT "Calculando azimut reciproco...";
500 Iazi=2*ATN(SIN( (ent(1)+sal(1))/2)/COS( (ent(1)-sal(1))/2)*TAN(Ilon/2))
505 ang=Iazi:GOSUB2:Iazi=ang:Iazi$=ang$
510 IF ent(1)>0 THEN sal(3)=ent(3)+Iazi+180 ELSE sal(3)=ent(3)-Iazi+180
520 IF sal(3)>360 THEN sal(3)=sal(3)-360
525 ang=sal(3):GOSUB2:sal(3)=ang:sal$(3)=ang$
528 CLS:PRINT"(èp1-èp2)/2=";DMS$((ent(1)-sal(1))/2;TAB(0)"(èp1+èp2)/2=";DMS$((ent(1)+sal(1))/2,"ÓΩ/2=";DMS$(Ilon/2
530 CLS:PRINT"Óà=2ATN(SIN((èp1+èp2)/2)  /          COS((èp1-èp2)/2) TAN(ÓΩ/2)","Óà=";Iazi$;TAB(0)"àp2=";:IF ent(1)>0 THEN PRINT "àp1+Óà˘180="; ELSE PRINT "àp2-Óà˘180=";
540 PRINT sal$(3):RETURN
600 'PROBLEMA DIRECTO CUERDA
605 cls:print"Problema Directo Cuerda"
610 INPUT"è1";F1,"L1";L1,"S";S,"à1Ê2";a1:F=F1:GOSUB900:N1=N:So=S-S^3/(24*R^3):PRINT"N1="N"p1="p,"R="R,"So=S-Sì/24Rí=";So
620 e1=e^2/(1-e^2):PRINT"e'í=eí/(1-eí)="e1
630 C=1:K1=-So/(2*N):K2=1:K3=0:GOTO 650
640 C=C+1:K1=-So*(1+e1*n^2)/(2*N):K1=ROUND(K1,-9):K2=SQR(1-K1^2)
650 IF C=1 THEN PRINT"cosÃ1Ê2=-So/2N1="K1,"senÃ1Ê2=1" ELSE PRINT"COSÃ1Ê2=-So(1+e'íní)/2N1="K1,"senÃ1Ê2=Ç(1-cosíÃ1Ê2)="K2
660 IF K1=K3 THEN GOTO690 ELSE GOTO680
680 n=SINF1*K1+COSF1*K2*COSa1:PRINT"n=senè1cosÃ1Ê2+cosè1senÃ1Ê2cosà1Ê2="n:K3=K1:GOTO640
690 IL=ATN(1/((N*COSF1/(So*K2)+COSF1*K1/K2-SINF1*COSa1)/SINa1)):L2=L1+IL
700 PRINT"ÓL=arcctg(1/senà1*(N1cosè1/SosenÃ1+cosè1ctgÃ-senè1cosà1))=",DMS$(IL)
710 PRINT"L2=L1+ÓL=";DMS$(L2),"1aprox N2=N1":F22=0
720 NF=ASN(So/N*(K2*(COSa1-SINa1*SINF1*TAN((L2-L1)/2))+e1*n*COSF1)):F2=F1+NF:F=F2:GOSUB900:N2=N
730 PRINT"Óè=ASN(So/N2*(senÃ1(COSà1-senà1senè1tg((L2-L1)/2))+e'íncosè1))=",DMS$(NF),"è2=è1+Óè="DMS$(F2),"N2="N
740 IF F2=F22 THEN GOTO750 ELSE F22=F2:GOTO720
750 GOSUB910:a21=a2:IF a1<90 OR a1>270 THEN a21=a2+180
760 GOSUB920
770 return
800 'PROBLEMA INVERSO CUERDA
805 cls:print"Problema Inverso Cuerda"
810 INPUT"è1";F1,"L1";L1,"è2";F2,"L2";L2
820 K=(SIN((F2-F1)/2))^2+COSF1*COSF2*(SIN((L2-L1)/2))^2:PRINT"sení≈/2=sení((è2-è1)/2)+cosè1+cosè2sení((L2-L1)/2)=",K
830 F=F1:GOSUB900:N1=N:F=F2:GOSUB900:N2=N:So=N1*SQR((N2/N1-1)^2+4*N2/N1*K-e^2*(2-e^2)*(N2*SINF2/N1-SINF1)^2):PRINT"N1="N1"N2="N2,"So=N1Ç((N2/N1-1)í+4N2sení(≈/2)/N1-eí(2-eí)(N2senè2/N1-senè1)í)=",So
840 F=(F1+F2)/2:GOSUB900:S=So+So^3/(24*R^2):PRINT"Nm="N"pm="p,"R=ÇNmpm="R,"S=So+Soì/24Rí="S
850 GOSUB910:a21=a2:K1=N1:K2=F1:K3=L1:N1=N2:F1=F2:L1=L2:N2=K1:F2=K2:L2=K3:GOSUB910:a12=a2:IF a12<0 THEN a12=a12+180
860 GOSUB930:ab1=a12+180:PRINT" Seleccione acimut segun dibujo 1="DMS$(a12" 2=";DMS$(ab1);:C$=INPUT$(1):CLS:IF C$="2" THEN a12=ab1
870 IF a12<90 OR a12>270 THEN a21=a21+180
880 GOSUB 920
890 return
900 N=A/SQR(1-e^2*(SINF)^2):p=A*(1-e^2)/SQR(1-e^2*(SINF)^2)^3:R=SQR(N*p):RETURN
910 a2=ATN(1/((N2*SINF2/N1-SINF1)*COSF2*e^2/(COSF1*SIN(L1-L2))+SIN(F1-F2)/(COSF1*SIN(L1-L2))+SINF2*TAN((L1-L2)/2))):RETURN
920 PRINT"à2Ê1=arcctg(eí(N2senè2/N1-senè1)cosè2/cosè1sen(L1-L2)+","+sen(è1-è2)/cosè1sen(L1-L2)+senè2tg((L1-L2)/2))="DMS$(a21):RETURN
930 PRINT"à1Ê2=arcctg(eí(N1senè1/N2-senè2)cosè1/cosè2sen(L2-L1)+","+sen(è2-è1)/cosè2sen(L2-L1)+senè1tg((L2-L1)/2))=":RETURN
1000 CLS:PRINT "Problema Inverso Pto H";TAB(0);"e=";e;"a=";a
1001 GOSUB5010:CLS:PRINT "Calculando azimutes...";
1010 Ilon=(ent(2)-ent(4):ang=Ilon:GOSUB2:Ilon=ang:Ilon$=ang$
1020 Ilat=ABS(ent(1)-ent(3)):ang=Ilat:GOSUB2:Ilat=ang:Ilat$=ang$
1030 latm=(ent(1)+ent(3))/2:ang=latm:GOSUB2:latm=ang:latm$=ang$
1040 Iazi=2*ATN(SIN( (ent(1)+ent(3))/2)/COS( (ent(1)-ent(3))/2)*TAN(Ilon/2))
1045 ang=Iazi:GOSUB2:Iazi=ang:Iazi$=ang$
1050 lat=latm:GOSUB5100
1060 azim=ATN(-Ro/N*(SIN( (ent(1)-ent(3))/2)/COS( (ent(1)+ent(3))/2)*1/TAN(Ilon/2))
1065 IF azim<90 THEN azim=180+azim
1068 ang=azim:GOSUB2:azim=ang:azim$=ang$
1070 IF ent(2)>ent(4) THEN sal(1)=azim+Iazi/2+90:sal(2)=azim-Iazi/2-90
1080 IF ent(2)<ent(4) THEN sal(1)=azim+Iazi/2-90:sal(2)=azim-Iazi/2+90
1084 FORj=1TO2:ang=sal(j):GOSUB2:sal(j)=ang:sal$(j)=ang$:NEXTj
1085 CLS:PRINT"2àm=àp1+àp2";TAB(0)"Óà˘180=àp1-àp2"
1086 CLS:IF ent(2)>ent(4) THEN PRINT"àp1=àm+Óà/2+90=";sal$(1);TAB(0)"àp2=àm+Óà/2-90=";sal$(2)ELSE PRINT"àp1=àm+Óà/2-90=";sal$(1);TAB(0)"àp2=àm+Óà/2+90=";sal$(2)
1087 PRINT"(èp1-èp2)/2=";DMS$((ent(1)-ent(3))/2) ;
1088 PRINTTAB(0)"(èp1+èp2)/2=";DMS$((ent(1)+ent(3))/2)
1089 PRINT"ÓΩ/2=";DMS$(Ilon/2);TAB(0);:STOP: GOSUB5200:GOSUB5210
1090 CLS:PRINT"Óà=2ATN( SIN((èp1+èp2)/2) /       COS((èp1-èp2)/2)   TAN(ÓΩ/2)","Óà=";Iazi$;TAB(0);:STOP:PRINT "àm=-Ro/N SIN((èp1-èp2)/2) /         COS((èp1+èp2)/2) 1/TANÓΩ/2","àm=";azim$;TAB(0);:STOP
1200 CLS:PRINT "Calculando coordenadas....";
1210 lat=ent(3):GOSUB 5100:Np2=N:Rop2=Ro
1220 x=ROUND(N*PI/180*ASN(SIN ABS(Ilon)*COSent(3)),-(red+1)
1230 Sig=ROUND(Ro*(x^2/(2*N^2)*TAN ent(3)),-(red+1)
1250 lat=latm:GOSUB5100
1260 Beta=ROUND(Ro*Ilat*PI/180*(1+e^2/8*(Ilat*PI/180)^2*COS(2*(ent(1)+ent(3))/2)),-(red+1)
1280 y=ROUND(Beta-Sig,-(red+1)
1285 CLS: PRINT "x=N ASN(SINÓΩ COSè)=";TAB(16);x;"m","y=â-é=";y;"m";TAB(0);:STOP
1290 CLS:PRINT"é=Roxí/(2Ní) TANèH=";TAB(18);Sig;"m","â=RoÓè (1+eí/8 Óèí COS2è=";TAB(16);Beta;"m","N(p2)=";Np2;TAB(0)"Ro(p2)=";Rop2,"Ro(m)=";Ro
1300 CLS:PRINT "Calculando la cuerda....";TAB(0);
1310 IF sal(1)<=90 THEN alfa0=sal(1):PRINT"à0=àp1=";:GOTO1337
1320 IF sal(1)<=180 THEN alfa0=180-sal(1):PRINT"à0=180-àp1=";:GOTO 1337
1330 IF sal(1)<=270 THEN alfa0=sal(1)-180:PRINT"à0=àp1-180=";:GOTO 1337
1335 alfa0=360-sal(1):PRINT"à0=360-àp1=";
1337 ang=alfa0:GOSUB2:alfa0=ang:alfa0$=ang$:PRINT ang$:CLS:PRINT "....";
1338 lat=latm:GOSUB5100
1340 s1=ROUND(x/SINalfa0*(1+y^2/(6*N*Ro)),-(red+1)
1350 s2=ROUND(y/COSalfa0*(1-x^2/(3*N*Ro)),-(red+1)
1370 CLS:PRINT"sx=x/SINà0 (1+yí)/(6NRo))=";TAB(18);s1;"m","sy=y/COSà0 (1-xí)/(3NRo))=";TAB(18);s2;"m"
1375 PRINT "s=";: IF s1>s2 THEN PRINT s1; ELSE PRINT s2;
1380 PRINT "m";TAB(0);:STOP:RETURN
4980 '       [geotrans 1.0may97]     '         by Temon Design       '         vol 6.55 KB
5000 CLS:PRINT"Redondeo  (";1/10^red   ;:INPUT")Distancia:",red
5002 PRINTTAB(10)"(";1/10^rea;:INPUT")Angulo:",rea:RETURN
5006 CLS:PRINT"Elipsoide (";a;:INPUT")a:",a
5008 PRINTTAB(5) "(";e;:INPUT")e:",e:RETURN
5010 RESTORE VAL(pro$)+10000:CLS:FORi=1TO 4:READa$
5020 PRINT"(";DMS$(ent(i));")";a$;TAB(0);:INPUT":",ent(i):NEXT
5030 RETURN
5100 N=ROUND(a/(1-e^2*SINlat^2)^(1/2),-(red+1)
5110 Ro=ROUND( (a*(1-e^2))/(1-e^2*SINlat^2)^(3/2),-(red+1)
5120 RETURN
5200 PRINT "N=a/(1-eíSINíè)^(1/2)";TAB(13);"=";N;"m"  :RETURN
5210 PRINT "Ro=(a(1-eí))/(1-eíSINíè)^(3/2)                =";Ro;"m"  :RETURN
5300 cls:print"Metodo ëPto Auxiliar";tab(0)"íCuerda";:a$=input$(1):if a$="1"or a$="2"then met=val(a$)-1 else  goto5300
5310 return
10001 DATA Latitud P1 (èp1),Longitud P1 (Ωp1),à p1,s
10002 DATA Latitud P1 (èp1),Longitud P1 (Ωp1),Latitud P2 (èp2),Longitud P2 (Ωp2)
20000 IF ERR=2 THEN 9
20005 CLS:PRINT "GeoTrans 1.00:       error!!!!!";:FORi=1TO300:NEXT:RESUME10
