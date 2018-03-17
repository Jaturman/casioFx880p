5    ' COMO INTRODUCIR Pk '         EN EL REPLANTEO CLOTOIDE    - Replanteo en entrada:          [ introducir Pk + Pk(V1) ]     - Replanteo en salida:           [ introducir Pk(V3) - Pk ]    .
10 SET f4:ANGLE2:PRINT CHR$(27);:IF FRE2<3000 THEN CLS:PRINT "min FRE2=3Kb !!!";:END
20 IF control >5 THEN CLS:BEEP:  PRINT "programa defectuoso !!!!";:END
30  ON ERROR GOTO 5000
80 CLS:PRINT "> Programa OBRA. v:3.0 casio";:FOR i=1TO100:NEXT:a$=INPUT$(1)
100 CLS:PRINT "ë[clotoide]   í[circular]  _v3_";TAB(0)"ì[parabola]   î[ herr.  ]  OBRA";:LOCATE27,0:  a$=INPUT$(1):ON VAL(a$) GOTO900,4500,2000,2900,5100,5100,4000,4000,4000:IF a$="0" THEN CLS:PRINT "fin del programa OBRA.....";:FOR i=1TO200:NEXT:CLS:END
110 GOTO100
900 CLS:PRINT "[clotoide]         < F > form.";TAB(0)"ë[calculo]  í[TT]  ì[replanteo]";:a$=INPUT$(1):ON VAL(a$) GOTO1000,1110,1290:IF a$="0" THEN 100
910 IF a$="F" OR a$="f" THEN GOSUB20010
920 GOTO900
1000 CLS:PRINT "[clotoide]";TAB(0);"< EXE > incognita";:FOR i=1TO 100:NEXT
1010 CLS:uu=11:PRINT "[clotoide] ";:A=0:R=0:L=0:INPUT "A:",A:GOSUB20000:INPUT "R:",R:GOSUB20000:INPUT "L:",L:CLS:PRINT "....";
1015 IF A<>0 AND R<>0 AND L<>0THEN1010
1020 IF A=0 THEN A=SQR(L*R)
1021 IF R=0 THEN R=A^2/L
1022 IF L=0 THEN L=A^2/R
1040 tao=200*(L/(2*R))/PI:xf=L-L^5/(10*(2*R*L)^2)+L^9/(216*(2*R*L)^4)-L^13/(9360*(2*R*L)^6:yf=L^3/(3*(2*R*L))-L^7/(42*(2*R*L)^3)+L^11/(1320*(2*R*L)^5)-L^15/(75600*(2*R*L)^7
1050 sl=SQR(xf^2+yf^2):sigma=ATN(yf/xf):x0=xf-SINtao*R:ir=yf+COStao*R-R:tc=yf/SINtao:tl=xf-yf/TANtao
1060 CLS:PRINT "A=";A;"        (Aí=RL)";TAB(0);"R=";R;"m   L=";L;"m";:a$=INPUT$(1):IF a$="F" OR a$="f" THEN GOSUB 20010
1070 CLS:  PRINT "Xf=";xf;"m    Yf=";yf;"m";TAB(0)"tao=L/2R=";tao;"Ê";(PI*tao)/200;"rad","Sl="sl;"m     é=";sigma;TAB(0)"Xo=";x0;"m    ÓR=";ir;"m","Tc=";tc;"m Tl=";tl;"m";TAB(0)
1100 CLS:PRINT "Quieres calcular TT 1[si] 2[no]";:a$=INPUT$(1):ON VAL(a$) GOTO 1110,900:GOTO1100
1110 CLS: PRINT "[calculo TT]";TAB(0)"(";V;")V:";:INPUT "",V:LOCATE31,0:PRINT "                      ";:LOCATE31,0: PRINT " (";R;:INPUT ")R:",R
1120 CLS:uu=10:PRINT "[entrada](";x0e;")Xo:";:INPUT "",x0e:GOSUB20000:PRINT ire;")ÓR:";:INPUT "",ire:CLS:PRINT "[salida] (";x0s;")Xo:";:INPUT "",x0s:GOSUB20000:PRINT irs;")ÓR:";:INPUT "",irs:CLS:PRINT "....";
1150 d=ABS(irs-ire)/SINV:te=(R+ire)/TAN(V/2):ts=(R+irs)/TAN(V/2)
1160 IF ire<irs THEN tte=x0e+te+d:tts=x0s+ts-d:CLS: PRINT "d=(ÓRs-ÓRe)/senV=";d
1170 IF ire>irs THEN tte=x0e+te-d:tts=x0s+ts+d:CLS:PRINT "d=(ÓRe-ÓRs)/senV=";d
1175 IF ire=irs THEN CLS:PRINT "d=0  (clotoide simetrica)"
1180 PRINT "t=(R+ÓR)/Tg(V/2)";TAB(0)"te=";te;"m ts=";ts;"m"
1190 IF ire<irs THEN PRINT "TTe=Xoe+t+d=";tte;"m";TAB(0)"TTs=Xos+t-d=";tts;"m"
1200 IF ire>irs THEN PRINT "TTe=Xoe+t-d=";tte;"m";TAB(0)"TTs=Xos+t+d=";tts;"m"
1205 IF ire=irs THEN tte=x0e+te:tts=x0s+ts:PRINT "TTe=Xoe+t=";tte;"m";TAB(0)"TTs=Xos+t=";tts;"m"
1210 CLS:PRINT "Donde te encuentras";TAB(0)"ë[entrada]  í[salida]";:a$=INPUT$(1):IF VAL(a$)=1 THEN tt=tte ELSE tt=tts
1215 IFa$="0" THEN 900
1220 uu=6:CLS:PRINT "[V1] (";:PRINT x1;:INPUT ")X:",x1:GOSUB 20000:PRINT y1;: INPUT ")Y:",y1:CLS:PRINT "[V2] (";x2; :INPUT ")X:",x2:GOSUB 20000:PRINT y2;:INPUT ")Y:",y2:CLS:PRINT "....";
1230 z=  POL (x2-x1 ,y2-y1 ):dvc=X-tt:avc=100-Y:IF avc<400 THEN avc=avc+400
1235 IF avc>400 THEN avc=avc-400
1240 z= REC (dvc,avc):xc=x1+Y:yc=y1+X
1250 CLS:PRINT "å V1ÊC=";avc;TAB(0)"D V1ÊC=D V1ÊV2-TT=";dvc;"m","Xc=";xc;"m";TAB(0)"Yc=";yc;"m":GOTO1300
1290 CLS:PRINT "(";A;:INPUT ")A:",A:PRINT "(";L;:INPUT ")L:",L:   PRINT "(";dvc;:INPUT ")DV1ÊC:",dvc:PRINT "(";avc;:INPUT")åVÊC:",avc:PRINT "(";x1;:INPUT ")Xv1:",x1:PRINT "(";y1;:INPUT ")Yv1:",y1:z= REC (dvc,avc):xc=x1+Y:yc=y1+X:CLS:PRINT "....";:GOTO1350
1300 CLS:PRINT "Quieres replantear ë[si] í[no]";:a$=INPUT$(1)
1310 ON VAL(a$) GOTO1350,900
1320 GOTO1300
1350 CLS:PRINT "[replanteo]Ce:Pk-PkV1#Cs:PkV3-Pk(";pkr;:INPUT ")Pk:",pkr:IF pkr=0 THEN 900
1360 CLS:PRINT "....";: l=pkr-dvc:IF l<0 OR l>L THEN CLS:PRINT "PK fuera de clotoide !!!";:FOR i=1TO200:NEXT:GOTO1350
1370 L=l:R=A^2/L: tao=(200* (L/(2*R)) )/PI :xf=L-L^5/(10*(2*R*L)^2)+L^9/(216*(2*R*L)^4)-L^13/(9360*(2*R*L)^6:yf=L^3/(3*(2*R*L))-L^7/(42*(2*R*L)^3)+L^11/(1320*(2*R*L)^5)-L^15/(75600*(2*R*L)^7
1380 sl=SQR(xf^2+yf^2):sigma=ATN(yf/xf):x0=xf-SINtao*R:ir=yf+COStao*R-R:tc=yf/SINtao:tl=xf-yf/TANtao
1390 CLS:PRINT "A=";A;TAB(0);"r=";R;"m    l=";L;"m"
1400 PRINT "xf=";xf;"m    yf=";yf;"m";TAB(0)"tao=";tao;"Ê";(PI*tao)/200;"rad","sl="sl;"m     é=";sigma;TAB(0)"xo=";x0;"m    Ór=";ir;"m","tc=";tc;"m   tl=";tl;"m";TAB(0)
1410 CLS:PRINT "situacion clotoide";TAB(0)"ë[izq]  í[drcha]";:a$=INPUT$(1):CLS:PRINT "....";:IF a$="" THEN 1410
1415 IF a$="1" THEN sigma=-sigma
1420 z=REC(sl,avc+sigma):xp=xc+Y:yp=yc+X:CLS:PRINT "X(pk";pkr;")=";xp;"m";TAB(0)"Y(pk";pkr;")=";yp;"m":GOTO1350
2000 CLS:PRINT "[parabola]";TAB(0)"ë[int.rect] í[åLT] ì[Z] 4[PK]";:a$=INPUT$(1):ON VAL(a$) GOTO 2100,2300,2400,2500:IF a$="0" THEN100
2010 GOTO2000
2100 uu=10:CLS: PRINT "[int.rect](";xa;:INPUT ")Xa:",xa:GOSUB20000:PRINT "(";za;:INPUT ")Za:",za:GOSUB20000:PRINT "(";xb;:INPUT ")Xb:",xb:GOSUB20000:PRINT "(";zb;:INPUT ")Zb:",zb:GOSUB20000
2110 PRINT "(¢)(";pa$;:INPUT ")Pa:",a$:IF VAL(a$)<>0 THEN pa$=a$:pa=VALF(pa$)
2120 GOSUB20000:PRINT "(¢)(";pb$;:INPUT ")Pb:",a$:IF VAL(a$)<>0 THEN pb$=a$:pb=VALF(pb$)
2130 CLS:PRINT "[recta 1]       Z -";za;"=";TAB(9)pa$;"( X-";xa;")":CLS:PRINT "[recta 2]       Z -";zb;"=";TAB(9)pb$;"( X-";xb;")":CLS:PRINT-za+zb;"=";pa-pb;"X"; :IF pa*-xa-pb*-xb>0 THEN PRINT "+";
2140 PRINT pa*-xa-pb*-xb
2200 xv=(zb-za+pa*xa-pb*xb)/(pa-pb):zv=pa*(xv-xa)+za
2210 CLS:PRINT "X=";xv;"m";  TAB(0)"Z=";zv;"m"   :GOTO2000
2300 uu=7: CLS:PRINT "[åLT] (";pa$;:INPUT ")Pa:",a$:IF VAL(a$)<>0 THEN pa$=a$:pa=VALF(pa$)
2310 GOSUB20000:PRINT pb$;:INPUT ")Pb:",a$:IF VAL(a$)<>0 THEN pb$=a$:pb=VALF(pb$)
2320 GOSUB20000:PRINT "+)hoyo (-)lomo           (";kv;:INPUT ")Kv:",kv:CLS:PRINT "[åLT] (";xv;:INPUT ")PKv:",xv:CLS:PRINT "....";
2350 ttita=pa-pb:LL=ABS(ttita*kv):T=LL/2
2360 CLS:PRINT "å=Pa-Pb=";ttita;TAB(0)"L=åKv=";LL;"m T=";T;"m"  ,"PKt1=";xv-T;"m";   TAB(0)"PKt2=";xv+T;"m":GOTO2000
2400 uu=12:CLS:PRINT "[calculo Z]  Zrr=Zref+P(X)      (";z;:INPUT ")Zref:",z:IF z=0 THEN2000
2405 CLS:PRINT "[calculo Z](";x;:INPUT ")X:",x:GOSUB 20000:PRINT "¢)("; p$;:INPUT ")P:",a$:IF VAL(a$)<>0THENp$=a$:p=VALF(p$)
2410 zrr=z+p*x
2420 CLS:PRINT "calcular Zrc 1[si] 2[no]";:a$=INPUT$(1):ON VAL(a$) GOTO 2440,2450
2430 GOTO2420
2440 CLS:PRINT "(";kv;:INPUT ")Kv:",kv:y=x^2/(2*kv):zrc=zrr+y
2450 CLS:PRINT "Zrr=";zrr;"m";:IF VAL(a$)=1 THEN PRINT " Zrc=";zrc;"m";TAB(0)"Zrc=Zrr-y. y=Xí/2Kv=";y:GOTO2400
2460 STOP:GOTO2400
2500 uu=16:CLS:PRINT "[Pk conocido P]Pc=(Pb-Pa)/L X+Pa(";L;:INPUT ")L:",L:CLS:PRINT "[Pk conocido P](";pa$;:INPUT ")Pa:",a$:IF VAL(a$)<>0 THEN pa$=a$:pa=VALF(pa$)
2510 GOSUB20000:PRINT pb$;:INPUT ")Pb:",a$:IF VAL(a$)<>0 THEN pb$=a$:pb=VALF(pb$)
2520 GOSUB20000:PRINT pc$;:INPUT ")Pc:",a$:IF VAL(a$)<>0 THEN pc$=a$:pc=VALF(pc$)
2530 CLS:PRINT "X(P=";pc$;")=";(pc-pa)/((pb-pa)/L);"m":GOTO2000
2900 CLS:PRINT "[herramientas]        ë[int.r]  í[wDR]   ì[peralte]   î[u.obra]";:a$=INPUT$(1):ON VAL(a$) GOTO3000,3200,3400,3600,3800 :IF a$="0" THEN100
2910 GOTO2900
3000 uu=10:CLS:PRINT "[int.rec](";XA;:INPUT ")Xa:",XA:GOSUB 20000:PRINT YA;:INPUT ")Ya:",YA:GOSUB 20000:PRINT XB;:INPUT ")Xb:",XB:GOSUB 20000:PRINT YB;:INPUT ")Yb:",YB:GOSUB 20000:PRINT PA;:INPUT ")åa:",PA
3010 GOSUB20000:PRINT PB;:INPUT ")åb:",PB
3020 CLS:PRINT "[recta 1]      Y-";YA;"=";TAB(0)"1/Tg";PA;"( X-";XA;")":CLS:PRINT "[recta 2]      Y-";YB;"=";TAB(0)"1/Tg"; PB;"( X-";XB;")":CLS:PRINT-YA+YB;"=";1/TAN PA-1/TAN PB;"X";:IF (1/TAN PA)*-XA-(1/TAN PB)*-XB>0 THEN PRINT "+";
3030 PRINT (1/TAN PA)*-XA-(1/TAN PB)*-XB
3050 X=(YB-YA+(1/TAN PA)*XA-(1/TAN PB)*XB)/((1/TAN PA)-(1/TAN PB)):Y=(1/TAN PA)*(X-XA)+YA
3060 CLS:PRINT "X=";X;"m";TAB(0)"Y=";Y;"m":GOTO2900
3200 uu=6:CLS:w=0:D=0:R=0: PRINT "[wDR]";TAB(0)"< EXE > incognita";:FOR i=1 TO100:NEXT:CLS:PRINT "[wDR]";:INPUT " w:",w:GOSUB 20000:INPUT "D:",D:GOSUB 20000:INPUT "R:",R
3210 IF w=0 THEN w=(400*D)/(2*PI*R):CLS:PRINT "w=400D/2ÁR=";w
3220 IF D=0 THEN D=(2*PI*R*w)/400:CLS:PRINT "D=2ÁRw/400=";D;"m"
3230 IF R=0 THEN R=(400*D)/(2*PI*w):CLS:PRINT "R=400D/2Áw=";R;"m"
3240 GOTO2900
3400 uu=10:CLS:PRINT "[peralte]";:PRINT "(";L;:INPUT ")L:",L:GOSUB20000:PRINT "¢)(";pm$;:INPUT ")Pmax:",a$:IF VAL(a$)<>0 THEN pm$=a$:pm=VALF(pm$)
3410 CLS:PRINT "Lo=L/50Pmax=";L/(50*pm);"m";TAB(0)"[ si nos dan l: (Lo=(L-l)/2)]"
3420 CLS:PRINT L;"-----------";pm*100;"%";TAB(1)"("; aa;:INPUT  "):";aa:IF aa=0 THEN 2900
3430 LOCATE0,1:PRINT aa;"-----------";(pm*aa*100)/L;"%";:LOCATE0,0:STOP :GOTO3420
3600 CLS:PRINT "[u.obra]";TAB(0)"ë[superficie] í[volumen]";:a$=INPUT$(1):ON VAL(a$)GOTO3620,3650:IF a$="0" THEN 2900
3610 GOTO3600
3620 uu=13:CLS:PRINT "[superficie](";bs;:INPUT ")Bs:",bs:IF bs=0 THEN 3600
3630 GOSUB20000:PRINT bi;:INPUT ")Bi:",bi:GOSUB20000:PRINTh;:INPUT ")h:",h:CLS:PRINT "superficie=";((bs+bi)*2)*h;"mí":GOTO3620
3650 uu=10:CLS:PRINT "[volumen](";s1;:INPUT ")S1:",s1:IF s1=0 THEN 3600
3660 GOSUB20000:PRINT s2;:INPUT ")S2:",s2:GOSUB20000:PRINTdsep;:INPUT ")Dsep:",dsep:CLS:PRINT "volumen=";((s1+s2)*2)*dsep;"mì":GOTO3620
3800 CLS:PRINT "[int.r-circ]","[rec] Y-Yt=P(X-Xt)";TAB(0)"[circ] (X-Xo)í+(Y-Yo)í=Rí","[rec] Y=Yt+PX-PXt # N=Yt-PXt";TAB(0)"[c]Xí-2XoX+Xoí+Yí-2YoY+Yoí-Rí=0","Xí-2XoX+Xoí+(PX+N)í-2YoY+Yoí-Rí =0","Xí+Xoí-2XoX+(PíXí+Ní+2NPX)+Yoí- [2Yo(Y)]-Rí # [-2YoPX-2YoN]"
3801 PRINT "(1+Pí)Xí+(2NP-2Xo-2YoP)X+Yoí+Ní +Xoí-2YoN-Rí=0","a=1+Pí  #  b=2NP-2Xo-2YoP";TAB(0)"c=Yoí+Xoí+Ní-2YoN-Rí":GOTO2900
4000 i=0:CLS:PRINT "[editor de puntos]";TAB(0)"ë[escribir]     í[åyD] ì[coord]";:a$=INPUT$(1):ON VAL(a$)GOTO 4020,4099,4199:IF a$="0" THEN100
4010 GOTO4000
4020 IF n=0 THEN CLS:INPUT "dim:",n:DIM x(n):DIM  y(n)
4030 CLS:i=i+1:SETn:PRINT "[punto][";i;"]:";:INPUT"",i:IF i=0THEN4000
4040 CLS:SETf4:    PRINT "(";x(i);")[X][";:SETn:PRINT   i;"]:";:INPUT "",x(i):SETf4: PRINT "(";y(i);")[Y][";:SETn:PRINT   i;"]:";:INPUT "",y(i):GOTO4030
4099 IF n=0THENCLS:PRINT "no dim !!!";:FOR i=1TO200:NEXT:GOTO4000
4100 CLS:INPUT "[pto]:",a:IF a=0THEN4000
4105 SETf4:LOCATE8,0:PRINT x(a);",";y(a);TAB(0);:INPUT "[pto]:",b:LOCATE8,1:PRINT x(b);",";y(b);:LOCATE0,0:STOP
4110 z=  POL (x(b)-x(a) ,y(b)-y(a) ):dst=X:azi=100-Y:IF azi<400 THEN azi=azi+400
4120 IF azi>400 THEN azi=azi-400
4130 CLS:SETn:PRINT "[Dst][";a;"Ê";b;"]=";:SETf4:PRINT   dst;"m";TAB(0);:SETn:PRINT "[ å ][";a;"Ê";b;"]=";:SETf4:PRINT azi:GOTO 4100
4199 IF n=0THENCLS:PRINT "no dim !!!";:FOR i=1TO200:NEXT:GOTO4000
4200 CLS:INPUT "[pto]:",a:IF a=0THEN4000
4210 LOCATE8,0:PRINT x(a);",";y(a);TAB(0);:INPUT "[pto]:",b
4220 CLS:PRINT "(";az;:SETn:PRINT  ")[å";a;"Ê";b;"]:";:INPUT "", az:SETf4: PRINT "(";ds;:SETn:PRINT ")[D";a;"Ê";b;"]:";:INPUT "",ds
4230 z= REC (ds,az):x(b)=x(a)+Y:y(b)=y(a)+X:CLS:PRINT "[X";b;"]=";:SETf4:PRINT x(b);"m"; TAB(0)"[Y";:SETn:PRINT   b;"]=";:SETf(4):PRINT  y(b);"m"  :GOTO4200
4500 CLS:PRINT "[circular](no disponible)";:FOR i=1TO300:NEXT:GOTO100
5000 CLS:control=control+1:PRINT "error!!!";:FOR i=1 TO 300:NEXT:IF control<6 THEN  RESUME100 ELSE CLS:PRINT "Programa defectuoso !!!!!!";:END
5100 CLS:PRINT "[limpiar variables] ë[si] ê[no]";:a$=INPUT$(1):IF a$="1" THEN CLEAR
5110 GOTO100
6000 '     [topografia de obras]     '     v: 3.0, (agosto 1996)     '     Villar Olalla. Cuenca     '     programado,J.Turegano     '     EUPolitecnica  Merida     '     ( Casio Fx 880/850P )     '     vol:10.210 Kb  Basic.
20000 LOCATE uu,0:PRINT "                                                ";:LOCATE uu,0:RETURN
20001 CLS:PRINT "[int.rect][ ‰(-)(+)Ê  Â(-)(+)„ ]";:LOCATE31,0:RETURN
20010 CLS:PRINT "[formulario clotoide]","Aí=LR          #   Tao=L/2R";TAB(0)"SL=Ç(Xí+Yí)    #   Tg é=Y/X","Xo=X-Sen(Tao)R #ÓR=Y+Cos(Tao)R-RTc=Y/Sen(Tao)  # Tl=X-Y/Tg(Tao)":CLS:PRINT "fin del formulario....";:FOR i=1TO200:NEXT:RETURN
