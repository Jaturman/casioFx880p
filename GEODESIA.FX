1 ANGLE0:ON ERROR GOTO10000:GOTO9
2 'GEODesia 1.00 Jun97 Merida'             J.Turegano
3 i=0:ang=ROUND(ang*3600,-(rea+1))/3600
4  i=i+1: b$=MID$(DMS$(ang),i,1:IF i>11 THEN 6
5 IF b$="." THEN 7 ELSE4
6 i=LEN(DMS$(ang))
7 ang$=LEFT$(DMS$(ang),i)+MID$(STR$(FRAC(ang*3600),4,rea+1)
8 RETURN
9 CLS:PRINT "Elipsoide Actual:";TAB(0)"e=";e;"  a=";a;:FORi=1TO200:NEXT
10 CLS:PRINT"[ GEODESIA ]:";TAB(0)">�Eli,�RDst,�NivT,�Reduc,�Tri ?";
11 RESTORE13:GOSUB 12:FORi=1TO4:GOSUB 12:LOCATE 31,0:GOSUB 12:READ b$:GOSUB 12:PRINT b$;:GOSUB12:NEXT:GOTO11
12 a$=INKEY$:IF a$="" THEN RETURN
13 DATA /,-,�,|
14 ON VAL(a$)GOTO50,490,700,1500,2000,#9,#5,#5,#5:IF a$="0"THEN LOCATE31,0:PRINT" Salir....                      ";:FORi=1TO300:NEXT:CLS:END
15 IF a$="."THENGOSUB30:GOTO10
20 CLS:PRINT "�Eli: Elipsoide";TAB(0)"�RDst: Reduccion Distancias","�NivT: Nivelacion Trigonometrica�Reduc: Reduccion de azimutes","�Tri:Calculo Triangulos geodes.";TAB(0)".: Redondeo":GOTO10
30 CLS:PRINT"Redondeo  (";1/10^red   ;:INPUT")Distancia:",red
40 PRINTTAB(10)"(";1/10^rea;:INPUT")Angulo:",rea:RETURN
49 'ELIPSOIDE
50 CLS:PRINT"[elipsoide]";TAB(0)">�abc,�NRo,�Coor,�ea,����',�arc";:a$=INPUT$(1):ON VAL(a$) GOTO60,52,100,90,70,80:IF a$="0"THEN10
51 CLS:PRINT "u�/a�+v�/b�=1    v-v1=TAN�(u-u1)Sup(casquete)=2�R Flecha":GOTO50
52 GOSUB4990:GOTO50
60 CLS:PRINT"e=c/a        e�=2�-��    e'=c/b";TAB(0)"�=(a-b)/a    a�=b�+c�":GOTO50
70 CLS:PRINT "[Latitudes]:   TAN �=a/b TAN�";TAB(0)"TAN�'=b/a TAN�,TAN�=a�/b� TAN�'":GOTO50
80 CLS:PRINT "[Arco]=Rad Ang: par: N COS� d�  mer:Ro�(1+e�/8COS2�m)�Ro d�":GOTO50
90 CLS:PRINT"Elipsoide (";a;:INPUT")a:",a
95 PRINTTAB(5) "(";e;:INPUT")e:",e:GOTO50
100 CLS:PRINT" ";TAB(0)"Desconocido �[xyz] �[XYZ] �[��]";:a$=INPUT$(1):IF a$="0"THEN50
110 CLS:PRINT" ";TAB(0)"Conocido ";:ON VAL(a$)GOTO120,130,140:IF a$="0"THEN50
115 CLS:PRINT "u=N COS�";TAB(0)"v=N(1-e�)SIN�":GOTO100
120 PRINT"�[XYZ][��] �[��]";:a$=INPUT$(1):CLS:ON VAL (a$) GOTO 220,150:IF a$="0"THEN100
125 GOTO120
130 PRINT"�[xyz][��h] �[��h]";:a$=INPUT$(1):CLS: ON VAL(a$) GOTO 210,160:IF a$="0"THEN100
135 GOTO130
140 PRINT"�[xyz] �[XYZ]";:a$=INPUT$(1):CLS:  ON VAL(a$) GOTO 170,180:IF a$="0"THEN100
145 GOTO140
150 GOSUB270:GOSUB5000:x=ROUND(N*COSLat*COSLon,-(red+1:y=ROUND(N*COSLat*SINLon,-(red+1:z=ROUND(N*(1-e^2)*SINLat,-(red+1:PRINT"x=N COS� COS�=";x;TAB(0)"y=N COS� SIN�=";y,TAB(0)"z=N(1-e�) SIN�=";z:GOTO50
160 GOSUB270:GOSUB280:GOSUB5000:X=ROUND((N+h)*COSLat*COSLon,-(red+1:Y=ROUND((N+h)*COSLat*SINLon,-(red+1:Z=ROUND((N*(1-e^2)+h)*SINLat,-(red+1:PRINT"X=(N+h)COS� COS�=";X;TAB(0)"Y=(N+h)COS� SIN�=";Y,TAB(0)"Z=(N(1-e�)+h)SIN�=";Z:GOTO50
170 GOSUB260:Lon=ATN(y/x):ang=Lon:GOSUB2:Lon=ang:Lon$=ang$:Lat= ATN(z/((1-e^2)*SQR(x^2+y^2)):ang=Lat:GOSUB2:Lat=ang:Lat$=ang$:PRINT"�=ATN(y/x)=";Lon$,"�=ATN(z/((1-e�)�(x�+y�)))=";TAB(0);Lat$:GOTO50
180 GOSUB250:Lon=ATN(Y/X):ang=Lon:GOSUB2:Lon=ang:Lon$=ang$:it=0
190 it=it+1:CLS:PRINT"Iteracion ";it;:IFit=1THEN h=0:N=1:PRINT "       h�0";
200 Lat=ATN(Z/SQR(X^2+Y^2)*(1-e^2*N/(N+h))^-1:ang=Lat:GOSUB2:Lat=ang:Lat$=ang$:PRINTTAB(0)"�=ATN(Z/�(X�+Y�) (1-e�N/(N+h))=";TAB(0);Lat$
201 GOSUB5000:h=ROUND(SQR(X^2+Y^2)/COSLat-N,-(red+1:CLS:PRINT"h=�(X�+Y�)/(COS�-N)=";TAB(0);h
202 CLS:PRINT"Otra iteracion �[si]";:a$=INPUT$(1):IF a$="1"THEN 190 ELSE GOTO 205
205 CLS:PRINT "�=ATN(Y/X)=";Lon$:GOTO50
210 GOSUB260:GOSUB270:GOSUB280:X=ROUND(x+h*COSLat*COSLon,-(red+1:Y=ROUND(y+h*COSLat*SINLon,-(red+1:Z=ROUND(z+h*SINLat,-(red+1:PRINT"X=x+hCOS�COS�=";X;TAB(0)"Y=y+hCOS�SIN�=";Y,"Z=z+hSIN�=";Z:GOTO50
220 GOSUB250:GOSUB270:GOSUB280:x=ROUND(X-h*COSLat*COSLon,-(red+1:y=ROUND(Y-h*COSLat*SINLon,-(red+1:z=ROUND(Z-h*SINLat,-(red+1:PRINT"x=X-hCOS�COS�=";x;TAB(0)"y=Y-hCOS�SIN�=";y,"z=Z-hSIN�=";z:GOTO50
250 PRINT"(";X;:INPUT")X:",X:PRINT"(";Y;:INPUT")Y:",Y:PRINT"(";Z;:INPUT")Z:",Z:RETURN
260 PRINT"(";x;:INPUT")x:",x:PRINT"(";y;:INPUT")y:";y:PRINT"(";z;:INPUT")z:",z:RETURN
270 PRINT"(";DMS$(Lat;:INPUT")�:",Lat:PRINT"(";DMS$(Lon;:INPUT")�:",Lon:RETURN
280 PRINT"(";h;:INPUT")h:",h:RETURN
489 'REDUCCION DE DISTANCIAS
490 CLS:PRINT "[reduccion de distancias]";:FORi=1TO250:NEXT
500 CLS:PRINT"do-��D-��D'-��So-��S";TAB(0)"do�-D�-D'�-So�-S    �[D�So]";:a$=INPUT$(1):CLS:ON VAL(a$)GOTO 600,510,530,550,600,610,620,630,570:IFa$="0"THEN10
505 GOTO500
510 PRINT"(";D;:INPUT")D:",D:PRINT"(";Ih;:INPUT")�h:",Ih
520 CLS:PRINT"D'=D-�h�/2D-�h�/8D�=";:DD=D-Ih^2/(2*D)-Ih^4/(8*D^3):PRINTDD
530 PRINT"(";DD;:INPUT")D':",DD:PRINT"(";hm;:INPUT")hm:",hm:GOSUB 650:GOSUB5000
540 CLS:PRINT"So=D'-D' hm/R+D'hm�/R�=";:So=DD-DD*(hm/R)+DD*hm^2/R^2:PRINTSo
550 PRINT"(";So;:INPUT")So:",So:GOSUB 650:GOSUB5000
560 CLS:PRINT"S=So+So�/24R�=";:S=So+So^3/(24*R^2):PRINTS:GOTO500
570 PRINT"(";D;:INPUT")D:",D:PRINT"(";Ih;:INPUT")�h:",Ih:PRINT"(";ha;:INPUT")ha:",ha:PRINT"(";hb;:INPUT")hb:",hb:GOSUB 650:GOSUB5000
580 PRINT"So=�((D�-�h�)/((1+ha/R)(1+hb/R))";:So=( (D^2-Ih^2)/((1+ha/R)*(1+hb/R)))^(1/2):PRINT"      =";So:GOTO500
600 CLS:PRINT "th cos(z,mb,ia,do)":GOTO500
610 CLS:PRINT "D=�(D'�+�h�)":GOTO500
620 CLS:PRINT "D'=So/(1-hm/R+hm�/R�)":GOTO500
630 CLS:PRINT "So=S-S�/24R�":GOTO500
650 PRINT "(";DMS$(Lat;:INPUT ")�m:",Lat:RETURN
700 'NIVELACION TRIGONOMETRICA
710 CLS:PRINT "[nivelacion trig.] �H+dH=Hb-Ha  >�v.aisl,�v.reciproc,�c.ortomet";:a$=INPUT$(1):CLS:ON VAL(a$) GOTO 720,910,970:IF a$="0"THEN GOTO10
715 CLS:PRINT "Visual aislada";TAB(0)"�t=do COSZa'+ia-mb","e=D�/2R";TAB(0)"Ro=ATN(D/2R k)      r=D�/2R k","Visual reciproca";TAB(0)"�H=D(1+Hm/R)TAN((Zb-Za)/2)","Corr. ortometrica";TAB(0)"dH=-H � SIN2� d�","d�=�final-�inicial";TAB(0)"beta=0.0052":GOTO710
720 PRINT"(";do;:INPUT")do:",do
730 PRINT"(";ia;:INPUT")ia:",ia
740 PRINT"(";mb;:INPUT")mb:",mb
750 PRINT"(";z;:INPUT")Za':",z
760 It=do*COSz+ia-mb:PRINT"�t=do COSZa'+ia-mb=";It
770 GOSUB5000 
780 PRINT"(";D;:INPUT")D:",D
790 PRINT"(";za;:INPUT")Za:",za
800 PRINT"(";zb;:INPUT")Zb:",zb
810 K=1+R/D*(200-(Za+Zb)):PRINT"k=1+R/D(200-(Za+Zb))=";K
820 PRINT"(";K;:INPUT")k:",K
830 e=D^2/(2*R):PRINT"e=D�/2R";e
840 r=D^2/(2*R)*K:PRINT"r=D�/2R k";r
850 RoR=D/(2*R)*K:PRINT"Ro=ATN(D/2R k=";RoR
860 PRINT"(";K;:INPUT")k:",K
870 PRINT"(";e;:INPUT")e:",e
880 PRINT"(";r;:INPUT")r:",r
890 PRINT"(";It;:INPUT")�t:",It
900 IH=It+e-r:PRINT"�H=�t+e-r=";IH:GOTO710
910 GOSUB4990
920 PRINT"(";D;:INPUT")D:",D
930 PRINT"(";Hm;:INPUT")Hm:",Hm
940 PRINT"(";za;:INPUT")Za:",za
950 PRINT"(";zb;:INPUT")Zb:",zb
960 IH=D*(1+Hm/R)*TAN ((zb-za)/2):PRINT"�H=D(1+Hm/R)TAN((Zb-Za)/2)=";IH:GOTO710
970 PRINT"(";Lati;:INPUT")�inicial:",Lati
980 PRINT"(";Latf;:INPUT")�final:",Latf
990 PRINT"(";H;:INPUT")H:",H
1000 beta=0.0052:PRINT"(";beta;:INPUT")�:",beta
1010 dLat=Latf-Lati:PRINT"d�=�final-�inicial";dLat
1020 Latm=(lati+latf)/2:PRINT"�m=";Latm
1030 dH=-H*beta*SIN(2*Latm)*dLat:PRINT"dH=-H � SIN2� d�=";dH:GOTO710
1490 'REDUCCION DE AZIMUTES
1500 CLS:PRINT "[reduccion de azimutes]";:FORi=1TO250:NEXT:CLS: PRINT"z/Z cenital astro/geodesica";TAB(0)"�/� com.vert/hor des.r.vert.":CLS
1510 PRINT"(";DMS$(Aij;:INPUT")Aij:",Aij:PRINT"(";DMS$(zij;:INPUT")z(c.refracc):",zij:PRINT"(";DMS$(LatA;:INPUT")�(Astro):",LatA:PRINT"(";DMS$(LonA;:INPUT")�(Astro):",LonA
1520 PRINT"(";DMS$(LatGi;:INPUT")�(Geo)i:",LatGi:PRINT"(";DMS$(LonGi;:INPUT")�(Geo)i:",LonGi:PRINT"(";DMS$(LatGj;:INPUT")�(Geo)j:",LatGj:PRINT"(";DMS$(LonGj;:INPUT")�(Geo)j:",LonGj
1530 PRINT"(";hj;:INPUT")hj:",hj:PRINT"(";Dij;:INPUT")Dij:",Dij
1540 CLS:PRINT"....";
1550 ch=(LonA-LonGi)*COSLatGi:cv=LatA-LatGi
1555 ang=ch:GOSUB2:ch=ang:ch$=ang$:ang=cv:GOSUB2:cv=ang:cv$=ang$
1560 C1=-ch*TANLatGi:Zij=zij+cv*COSAij+ch*SINAij
1564 ang=C1:GOSUB2:C1=ang:C1$=ang$:ang=Zij:GOSUB2:Zij=ang:Zij$=ang$
1566 C2=-(cv*SINAij-ch*COSAij)*1/TANZij:ALaplace=Aij+C1+C2
1568 ang=C2:GOSUB2:C2=ang:C2$=ang$:ang=ALaplace:GOSUB2:ALaplace=ang:ALaplace$=ang$
1570 LatGm=(LatGi+LatGj)/2:Lat=LatGm:GOSUB5000:CLS:PRINT ".....";:C3=180/PI* (hj/(2*Ro)*e^2*SIN(2*ALaplace)*(COSLatGm)^2:AA=ALaplace+C3:ang=AA:GOSUB2:AA=ang:AA$=ang$:   C4=-180/PI*(e^2*Dij^2*(COSLatGm)^2*SIN(2*AA))/(12*N^2)
1575 ang=LatGm:GOSUB2:LatGm=ang:LatGm$=ang$:ang=C3:GOSUB2:C3=ang:C3$=ang$:ang=C4:GOSUB2:C4=ang:C4$=ang$
1580 CLS:PRINT"�=(�a-�g)COS�g=";DMS$(ch;TAB(0)"�=�a-�g=";DMS$(cv,"C1=-�TAN(�g)=";C1$,"C2=-(� SINAij-�COSAij)COTG Zij";TAB(0)"Zij=zij+�COSAij+�SINAij","C2=";C2$;TAB(0)"Zij=";Zij$,"�=Aij+C1+C2=";ALaplace$,
1590 PRINT"C3=hj/(2Rom)e�SIN2� COS�(�gm)";TAB(0)"=";C3*PI/180;"�";C3$,"�gm=";LatGm$
1600 PRINT"C4=-(e�Dij�COS�(�gm)SIN(2�))";TAB(0)"/(12Nm�)=";C4*PI/180;"�","C4=";C4$;TAB(0)" ","�'=�+C3=";AA$;TAB(0)"�E=�'+C4=";:ang=(AA+C4):GOSUB2:PRINT ang$:GOTO10
2000 'CACULO DE TRIANGULOS GEODESICOS
2010 CLS:PRINT"[calculo triangulos geodesicos] >�Lado<90Km,�Lado>90Km,A'=A-�/3";:a$=INPUT$(1):ON VAL(a$)GOTO 2020,2030:IF a$="0"GOTO10
2015 GOTO2010
2020 CLS:PRINT"�=T/R�";TAB(0)"T=(bcSINA)/2    A'~A":GOTO2000
2030 CLS:PRINT"�=T/R� (1+m�/(8R�)";TAB(0)"T=(bcSINA')/2 , m�=(a�+b�+c�)/3"
2040 CLS:PRINT"(";b;:INPUT")b:",b: PRINT"(";c;:INPUT")c:",c
2050 PRINT"(";aa;:INPUT")a:",aa: PRINT"(";DMS$(Angulo;:INPUT")A:",Angulo: PRINT"(";DMS$(Lat;:INPUT")�m:",Lat
2060 GOSUB 5000:Ang=Angulo:it=0
2070 m=ROUND(SQR((aa^2+b^2+c^2)/3),-(red+1):CLS:PRINT "m=";m
2080 CLS:it=it+1:PRINT"Iteracion";it;
2090 exc=((b*c*SINAng)/(2*R^2))*((1+m^2/(8*R^2)):excc=exc*180/PI:ang=excc:GOSUB2:excc=ang:excc$=ang$:Ang=Angulo-(exc/3*180/PI):ang=Ang:GOSUB2:Ang=ang:Ang$=ang$
2100 CLS:PRINT"�=";exc;"�";excc$; TAB(0)"A'=A-�/3=";Ang$
2110 CLS:PRINT"Otra iteracion �[Si]";:a$=INPUT$(1):IF a$="1" THEN 2080
2120 GOTO2000
4980 '          [GEODesia Jun97]     '          [v 1.00 Merida ]     '          [por J.Turegano]     '          [ vol 8.62 KB  ]
4990 CLS:PRINT "(";DMS$(Lat;:INPUT")Lat:",Lat
5000 N=ROUND(a/(1-e^2*SINLat^2)^(1/2),-(red+1
5010 Ro=ROUND( (a*(1-e^2))/(1-e^2*SINLat^2)^(3/2),-(red+1
5020 R=ROUND(SQR(N*Ro),-(red+1:CLS:PRINT "N=";N;TAB(0)"Ro=";Ro;"R=";R:RETURN
10000 IF ERR=2 THENCLS:PRINT"� Menu";:FORi=1TO220:NEXT:GOTO10
10010 CLS:LOCATE20,1:PRINT "....error";:FORi=1TO300:NEXT:RESUME10
