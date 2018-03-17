4 IF FRE2<2000THEN CLS:PRINT "OFITEC:   min FRE2=2000B !!!";:END
5 IFcontrol>5THENCLS:BEEP:PRINT"OFITEC: Version Defectuosa !!!";:END
6 IFpaso=0THEN  IFFRE2<160+FRE0THEN PRINT"OFITEC 1.00(E97)";:FORi=1TO11:FORj=1TO40:NEXT:PRINT".";:NEXT
10 CLS:  PRINTCHR$(9);CHR$(27);:ANGLE2:SETn:contador=contador+1:ONERRORGOTO10100
15 ON salida GOTO20,30
20 salida=1:CLS:PRINT"OFITEC  ‘[mi] ’[dt] “[ci] ”[eh]";TAB(0);"[_][ ]<       •[eL] –[eT] —[eZ]";:LOCATE1,1:IFf=1THENPRINT "?";:LOCATE1,1
21 LOCATE4,1:PRINTCHR$(144+VAL(b$));:LOCATE1,1:u$=b$:b$=INPUT$(1):IFb$=CHR$(240)THENIFf=0THENf=1ELSEf=0
22 IFb$=CHR$(13)THENb$=u$
23 IFf=1THENONVAL(b$)GOTO3030,3050,3130,3110,3040,3060,3080
25 ONVAL(b$)GOTO300,470,1550,1500,400,500,700,30,650:IFb$="0"THENb$=u$:GOTO10000
27 IFb$="."THENb$=u$:CLS:PRINT"[mi] error con mira";TAB(0)"[dt] error con distanciometro","[ci] error con cinta";TAB(0)"[eh] error estadia horizontal":union=1
28 IFunion=1THENPRINT"[eL] error lineal";TAB(0)"[eT] error transversal","[eZ] error altimetrico","<9>Clear, <0>Salir, <EXE>Ultimo";TAB(0)"<MODE> formulario ä-æ Calculo":union=0:GOTO20
29 IFb$=CHR$(240)THENb$=u$:GOTO20
30 salida=2:b$=u$:CLS:PRINT"OFITEC  ‘[T ] ’[ea] “[äæ] ”RECA";TAB(0)"[_][ ]>       •[Tr] –[iD] —[iI]";:LOCATE1,1:IFf=1THENPRINT"?";:LOCATE1,1
31 LOCATE4,1:PRINTCHR$(144+VAL(c$));:LOCATE1,1:v$=c$:c$=INPUT$(1):IFc$=CHR$(240)THENIFf=0THENf=1ELSEf=0
32 IFc$=CHR$(13)THEN c$=v$
33 IFf=1THENONVAL(c$)GOTO3000,3010,30,3100,3070,3120,3150
35 ON VAL(c$)GOTO50,98,1200,2000,930,1100,1300,20,650:IFc$="0"THENc$=v$:GOTO10000
38 IFc$="."THENc$=v$:CLS:PRINT"[T ] calculo tolerancias";TAB(0)"[ea] error angular","[äæ] degäægra";TAB(0)"RECA recapitulacion","[Tr] error triangulacion";TAB(0)"[iD] error interseccion directa","[iI] error interseccion inversa ":GOTO30
39 IFc$=CHR$(240)THENc$=v$:GOTO30
40 c$=v$:GOTO20
50 CLS:PRINT"[ Tolerancias ]æcalculo";TAB(0)"‘[restituidor]";:rest$=INPUT$(1):IF rest$="1"THEN60
55 GOSUB1000:PRINT" (";N;:INPUT")N:",N:GOSUB1000:PRINT" (";eq;:INPUT")eq:",eq:tp=ROUND(.2*N/1E3,-4):ta=ROUND(1/4*eq,-4):CLS:PRINT"Tp=(0.8)0.2";N;"=";tp;"(";tp*.8;")";TAB(0)"Ta=(0.8)1/4";eq;"=";ta;"("ta*.8;")":GOTO30
60 LOCATE15,0:PRINT"Restituidor";:GOSUB1000:PRINT" (";mb;:INPUT")mb=4mk:",mb:GOSUB1000:PRINT" (";ml;:INPUT")ml:",ml:GOSUB1000:PRINT" (";dpa;:INPUT")dPa:",dpa:GOSUB1000:PRINT" (";p;:INPUT")recub.long.(%) :",p
62 GOSUB1000:PRINT" (";campo;")";:INPUT"campo(w):",campo
64 CLS:PRINT"k: ‘[plano]  ’[poco movido]";TAB(3)"“[urbano] ”[muy accidentado]";:a$=INPUT$(1):ONVAL(a$)GOTO65,66,67,68:GOTO64
65 k=1:GOTO70
66 k=1.5:GOTO70
67 k=2:GOTO 70
68 k=3:GOTO 70
69 GOTO64
70 epr=ROUND(mb*SQR2*ml/1000 ,-4):ezr=ROUND(((mb*dpa*k)/(2*(1-p/100)*TAN(campo/2)))/1000,-4)
75 CLS:PRINT"calcular error red apoyo 1[si]";:a$=INPUT$(1):IF a$<>"1"THENCLS:PRINT"ep=";mb;"‚2";ml;"=";epr;"m","ez=(";mb;"¥";dpa;"¥";k;")/";TAB(0)"(2(1-";p/100;")TAN(";campo;"/2))=";ezr:GOTO30
80 CLS:PRINT"[Tolerancia]";TAB(0)"(";N;:INPUT")N:",N:GOSUB1000:PRINT" (";eq;:INPUT")eq:",eq:tp=ROUND(.2*N/1E3,-4):ta=ROUND(1/4*eq,-4):CLS:PRINT"[T][Planimetria]  Tp=";tp;"m";TAB(0)"Rest.=";epr;" R.Apoyo=";ROUND(SQR(tp^2-epr^2),-4)
90 CLS:PRINT"[T][Altimetria]   Ta=";ta;"m";TAB(0)"Rest.=";ezr;" R.Apoyo=";ROUND(SQR(ta^2-ezr^2),-4):GOTO30
98 GOSUB99:GOTO30
99 digi=0:CLS:PRINT"[calculo ea](D=digital)";
100 GOSUB1000:PRINT" (";ap;")ap:";:IF digi=0 THEN INPUT"",ap$ELSE INPUT"",ap$
101 IF ap$<>""ANDap$<>"d"ANDap$<>"D"THENap=VAL(ap$)
105 IF ap$="d" OR ap$="D"THEN digi=1:LOCATE12,0:PRINT"nonio digital";:GOTO100
106 LOCATE12,0:PRINT "             ";:IF ap=0THENng=1ELSEng=0
110 IFng=1THENCLS:PRINT"æ ea niv geometrica";:a=3
120 a$="":IFng=0THENGOSUB1000:PRINT" (";eees;:INPUT")ee+es:",eees:h=0:IFeees=0THENGOSUB1000:PRINT" Equipo de poligonacion 1[si]";:a$=INPUT$(1):IFa$="1"THENh=1:a=12:b=30
125 IFeees<>0THENh=1:a=12:b=30
130 IFng=0ANDh=0THENCLS:PRINT "æ ea cenital";
132 IFh=0ORng<>0THENa=3
135 IFng=0ANDh=1THENCLS:PRINT "æ ea horizontal";:IFa$="1"THENPRINT" (e.p)";
140 PRINTTAB(0)"(";s;:INPUT ")s:",s:GOSUB1000:PRINT" (";A;:INPUT")A:",A:IFh=1ANDng=0ANDa$<>"1"THENGOSUB1000:PRINT" (";dst;:INPUT")Distancia:",dst
150 ni=1:IFdigi<>1ANDng=0THENGOSUB1000:PRINT" (";ni;:INPUT")n indices:",ni
155 a$="":IFng=1THEN bessel=1:GOSUB1000:PRINT" Burbuja Partida  ‘[si]:";:a$=INPUT$(1):IFVAL(a$)=1THENa=20
158 a$="":IFng=1ORh=0THENGOSUB1000:PRINT" ‘[coinc.] ’[no coinc.]";:IF ng=1THENPRINT" “[cuna]";
161 IFng=1ORh=0THENa$=INPUT$(1):IFVAL(a$)=2THENb=150:GOTO165
162 IFng=1ORh=0THENIFVAL(a$)=1THENb=60:GOTO165
163 IFng=1ORh=0THENIFVAL(a$)=3THENb=30:GOTO165
164 IFng=1ORh=0THEN158
165 IFVAL(a$)=2ORVAL(a$)=1ORVAL(a$)=3THEN180
170 IFng=1THENGOTO160
180 IFng=0 THEN GOSUB1000:PRINT" Bessel ‘[si] 2[no]:";:a$=INPUT$(1):IFVAL(a$)=1THENbessel=2:GOTO200
185 IFVAL(a$)=2THENbessel=1:GOTO200
190 IFng=0THENGOTO180
200 ev=ROUND(1/a*s,-3):ep=ROUND(b/A*(1+.04*A)*1/SQR bessel,-3):CLS:PRINT"e";:IFng=1THENPRINT"h";ELSEPRINT"v";
210 PRINT"=1/";a;"¥";s;"=";ev;"cc","ep=";b;"/";A;"(1+4¥";A;"/100)1/‚";bessel;"                  =";ep;"cc":el=0:ed=0
220 IF h<>0 AND ng<>1  THEN ed=ROUND(  (eees/dst)*636620,-3):PRINT TAB(0) "ed=";eees;"/";dst;" r=";ed;"cc";TAB(0)
230 IF ng<>1  THEN  IF digi=1 THEN el=ROUND(ap/SQRbessel,-3)ELSE el=ROUND(2/3*ap/SQRbessel*1/SQRni  ,-3)
235 IF ng<>1THEN   PRINT "el=";:IF digi=1 THENPRINT ap;"(1/‚";bessel;")=";el;"cc";TAB(0)  ELSE PRINT "2/3";ap;"(1/‚";bessel;")(1/‚";ni;")=";TAB(20);el;"cc"
240 ea=ROUND(  SQR(ev^2+el^2+ep^2+ed^2),-3)  :CLS:PRINT "ea=";ea;"cc";TAB(0)"(recæ 1/‚2 ea)  (triæ80%ea)":RETURN
300 CLS:PRINT"[ mira ]æcalculo  ";TAB(0)"(";d;:INPUT ")d:",d:GOSUB1000: PRINT " (";A;:INPUT ")A:",A:GOSUB 1000:PRINT " (";beta;:INPUT ")beta(g):",beta:GOSUB 1000:  PRINT " (";alfa;:INPUT ")alfa(g):",alfa:CLS:PRINT  ".....";:IF d=0.01 THEN a=55 ELSE a=140
310 dst=5*A+a:eD=ROUND(1/3*d/dst*100*SQR2,-5)  :eI=ROUND( (beta*1E4)/636620*TANalfa,-5):erel=ROUND(SQR(eD^2+eI^2),-5)
320 CLS:PRINT "‹d=1/3";d;"/";dst;"100‚2";TAB(20)"=";eD;"m","D=5¥";A;"+";a;"=";dst;"m";TAB(0) :PRINT "‹i=";beta*10000 ;"/r TAN";alfa;"=";eI;"m"; TAB(0)"‹=";erel;"m" :GOTO20
400 d=1:CLS:PRINT "[ eL ]";TAB(0)"(";tp;:INPUT ")eL:",tp
402 IF tp=0 THEN LOCATE9,0:PRINT "æcalculo del eL  ";
403 IF tp<>0 THENLOCATE9,0:PRINT "                ";
405 GOSUB 1000:PRINT  " (";dst;:INPUT ")D:",dst
410 IF tp<>0 AND dst=0 THENLOCATE12,0:PRINT "æcalculo dist f(eL)";:GOSUB1000
420 IF tp<>0 AND dst<>0 THENLOCATE12,0:PRINT "æcal.n.tramos f(eL)";
430 GOSUB 1000:PRINT " (";erel;:INPUT ")‹:",erel:GOSUB 1000:   PRINT " (";n;:INPUT ")n veces med dist:",n:IF tp=0 AND dst<>0 THEN GOSUB 1000:PRINT " (";nn;:INPUT ")n tramos:(1=rad):",nn
440 IF tp=0THEN el=ROUND(  erel/SQRn*dst*SQRnn,-4)  :CLS:PRINT "eL=";erel;"/‚";n;dst;"‚";nn;"=";TAB(20) el;"m":GOTO20
450 IF tp<>0 AND dst=0 THEN dst=ROUND((tp*SQRn)/(erel),-3) :CLS:PRINT "D(m)=(Tp‚n)/‹";TAB(0)"D=(";tp;"‚";n;")/";erel;"="; dst: GOTO20
460 IF tp<>0 AND dst<>0 THEN nn=ROUND(((tp*SQRn)/(erel*dst))^2,-1):CLS:PRINT "n(tramos)=(Tp‚n / ‹D)’","n=(";tp;"‚";n;"/";erel;"¥";dst;")’=";TAB(0);nn;"tramos"  :GOTO20
470 CLS:PRINT "[ distanciometro ]";TAB(0)"(";alfa;:INPUT ")ˆ(g):",alfa:GOSUB1000:PRINT " (";beta;:INPUT ")‰(g):",beta:GOSUB 1000:PRINT " (";dst;:INPUT ")D:",dst:GOSUB 1000:PRINT " (";ea;:INPUT ")eav:",ea:IF ea=0 THEN GOSUB 99:CLS:PRINT "[distanciometro]";
472 m=2: GOSUB 1000:PRINT " (";m;:INPUT ")m:",m
475 GOSUB 1000:INPUT " e.p.d: ",epd1:GOSUB 1000:PRINT " e.p.d:";epd1;"mm + ";  :INPUT "",epd2:GOSUB 1000:PRINT " e.p.d:";epd1;"mm +";epd2;"ppm";:FOR i=1TO200:NEXT
480 CLS:PRINT ".....";:   g1=ROUND((epd1+(dst*epd2)/1000)/1000 ,-4  :g2=ROUND( m*SINbeta,-4  :dg=ROUND(SQR(g1^2+g2^2),-4 :v=100-alfa:eL=ROUND(   SQR((SINv*dg)^2+(COSv*dst*ea/636620)^2),-4:erel=ROUND(    eL/dst,-8)
490 CLS:PRINT "v=100-";alfa;"=";v;"g"; TAB(0)"dv=";ea,"G1=epd=";g1;"m";   TAB(0)"G2=";m;"SIN";beta;"=";g2;"m"  ,"dG=‚(G1’+G2’)=";dg;"m" ,"eL=‚((SIN";v;dg;")’+(COS";v;TAB(0) "¥";dst;"¥";ea;"/r)’=";eL;"m":CLS:PRINT "‹=eL/";dst;"=";erel;"m"  :GOTO20
500 CLS:PRINT "[ eT ]";TAB(0)"(";tp;:INPUT ")eT:",tp:IF tp=0 THEN LOCATE12,0:PRINT "æcalculo del eT";
505 GOSUB 1000:PRINT  " (";dst;:INPUT ")D:",dst
510 IF tp<>0 AND dst=0 THENLOCATE12,0:PRINT "æcalculo dist f(eT)";:GOSUB1000
520 IF tp<>0 AND dst<>0 THENLOCATE12,0:PRINT "æcal.n.tramos f(eT)";:GOSUB1000
530 n=1:GOSUB1000:PRINT " (";ea;:INPUT ")ea:",ea:IF ea=0 THEN GOSUB 99:CLS:PRINT "[ eT ]";
532 IF tp=0 THEN GOSUB1000:PRINT " (";nn;:INPUT ")n tramos:(1=rad):",nn
535 GOSUB1000: PRINT " autoorientable 1[si] 2[no]";:a$=INPUT$(1):IF VAL(a$)=2 THEN a=2:GOTO540
536 IF VAL(a$)=1 THEN a=1:GOTO540
537 GOTO535
540 CLS:PRINT ".....";:IF tp=0AND a=2    THEN et=ROUND(ea/636620*dst*SQR2*SQR((nn*(nn+1)*(2*nn+1))/6),-4):CLS:PRINT "eT=";ea;"/r";dst;"‚2";:IF nn>1 THEN  PRINT TAB(0) "‚((n(n+1)(2n+1))/6";
542 IF tp=0AND a=1    THEN et=ROUND(ea/636620*dst*SQRnn,-4):CLS:PRINT "eT=";ea;"/r";dst;:IF nn>1 THEN  PRINT "‚";nn;
545 IF tp=0THENPRINT TAB(20)  "=";et;"m":GOTO20
550  IF tp<>0 AND dst=0 AND a=2 THENdst=(((tp*636620)/(ea*SQRa)):dst=ROUND(dst,-4):CLS:PRINT"D=(Tp r)/(ea ‚2)","D=(";tp;"r)/(";ea;"‚2)=";TAB(20);dst;"m":GOTO20
555  IF tp<>0 AND dst=0 AND a=1  THEN b=1:dst=(((tp*636620)/(ea*SQRa)):dst=ROUND(dst,-4):CLS:PRINT"D=(eT r)/ea","D=(";tp;"r)/(";ea;")=";TAB(20)dst;"m":GOTO20
560 IF tp<>0 AND dst=0  THEN PRINT ")=";dst;"m":GOTO20
580 IF tp<>0 AND dst<>0 AND a=1 THEN nn=ROUND(((tp*636620)/(dst*ea))^2,-1):CLS:PRINT "nn= ((eT r)/(D ea))’","nn=((";tp;"r)/(";dst;"¥";TAB(0);ea;"))’="; nn;"tramos":GOTO20
590 nn=0:IF tp<>0ANDdst<>0THEN c=6*((tp*636620)/(ea*dst*SQRa))^2:nn=0
600 nn=nn+1:IF nn*(nn+1)*(2*nn+1)>c THEN GOTO620
610 GOTO600
620 c=ROUND(c,-4):nn=ROUND(nn-1,-1):CLS:PRINT "6((eT¥r)/(ea¥D‚";a;"))’=";TAB(18)"n(n+1)(n+1)",c;"=n(n+1)(2n+1)";TAB(2)"n=";nn;"tramos":GOTO20
650 CLS:PRINT "OFITEC[";contador;"][mem";ROUND(((FRE2-FRE0)*100)/1857,-1);"%][error";control;"]  Limpiar variables ‘[si]";:a$=INPUT$(1):IF a$="1" THEN CLEAR :GOSUB 1000:PRINT "  Clear....";:FORi=1TO300:NEXT
660 GOTO15
700 CLS:PRINT "[ eZ ] Nivelacion:";TAB(0);"‘[trigonometrica] ’[geometrica]";:a$=INPUT$(1):ON VAL(a$) GOTO 702,900:IF a$="0" THEN 20
701 GOTO700
702 CLS:PRINT "[ eZ ]";TAB(0)"(";ta;:INPUT ")eZ:",ta:IF ta=0 THEN LOCATE12,0:PRINT "æcalculo del eZ";
705 GOSUB 1000:PRINT  " (";dst;:INPUT ")D:",dst
710 IF ta<>0 AND dst=0 THENLOCATE12,0:PRINT "æcalculo dist f(eZ)";:GOSUB1000
720 IF ta<>0 AND dst<>0 THENLOCATE12,0:PRINT "æcal.n.tramos f(eZ)";:GOSUB1000
730 nn=1:GOSUB1000:PRINT " (";ea;:INPUT")ea:",ea:IF ea=0 THEN GOSUB 99:CLS:PRINT "[ eZ ]";
731 GOSUB 1000:PRINT " (";erel;:INPUT ")‹:",erel:GOSUB 1000:PRINT " (";alfa;:INPUT ")ˆ(g):",alfa:GOSUB1000:PRINT " (";beta;:INPUT ")‰(g):",beta:GOSUB 1000:PRINT " (";m;:INPUT ")m:",m:     IFta=0 THEN GOSUB1000:PRINT " (";nn;:INPUT ")n tramos:(1=rad):",nn
736 IF ta=0 AND nn>1 THEN LOCATE28,0:PRINT ",Ez";
740 IF ta=0 AND nn<>0 THEN GOSUB 1000:PRINT " (";nnn;:INPUT ")n. med.desnivel:",nnn:GOSUB 1000:PRINT " (";n;:INPUT ")n. med.dist:",n
750 IF ta<>0 AND dst<>0 THEN GOSUB 1000:PRINT " (";nnn;:INPUT ")n. med.desnivel:",nnn:GOSUB 1000:PRINT " (";n;:INPUT ")n. med.dist:",n
755 CLS:PRINT "....";
760 a=((1+erel/SQRn)*TANalfa-TANalfa):et=dst*a: b=(TAN(alfa+ea/1E4)-TANalfa):ett=dst*b:ei=0.01:em=m*beta*1E4/636620*TAN(alfa+beta):ez=ROUND(SQR(et^2+ett^2+ei^2+em^2),-4)
770  a=ROUND(a,-8):b=ROUND(b,-8):et=ROUND(et,-6):ett=ROUND(ett,-6):em=ROUND(em,-6):erel=ROUND(erel,-8)
775 IFdst<>0THENCLS:PRINT"et'=";dst;"[(1+";erel;"/‚";n;")";TAB(0)"TAN";alfa;"-TAN";alfa;"]=";et;"m","et''=";dst;"[TAN(";alfa+ea/1E4;")";TAB(0"-TAN";alfa;"]=";ett"m"
777 IFdst<>0THENPRINT "ei= 0.01 m";TAB(0,"em=";m;beta*1E4 ;"/r";TAB(5)"TAN(";alfa;"+";beta;")=";em;"m","ez=";ez;"/‚";nnn;"m";TAB(0
780 IF ta=0 AND nn>1 THEN Ez=ROUND(  ez/SQRnnn *SQRnn,-4):CLS:PRINT "Ez=ez/‚"; nnn ;"‚";nn;"=";Ez;"m"
790 IF ta<>0 AND dst<>0 THEN nn=ROUND( ((ta*SQRnnn)/ez)^2,-1):CLS:PRINT "nn=(Ta ‚n)/eZ";TAB(0)"nn=(";ta;"‚";n;"/";ez;"=";nn;"tramos":GOTO20
800 IF ta<>0 AND dst=0THENCLS:PRINT "et=D(";a;")";TAB(0)"ett=D(";b;")","ei=0.01";TAB(0)"em=";em:dst=ROUND(SQR((ta^2-(ei^2+em^2))/(a^2+b^2)),-4):CLS:PRINT "D=";dst
820 GOTO20
900 CLS:PRINT "[ em ] æcalculo em";TAB(0)"(";ea;:INPUT ")ea:",ea:IF ea=0 THEN GOSUB 99:CLS:PRINT "[calculo em]";
905 GOSUB 1000:PRINT " (";beta;:INPUT ")‰(g):",beta:GOSUB 1000:PRINT " (";m;:INPUT ")m:",m: GOSUB 1000:PRINT " (";dn;:INPUT ")Dn:",dn:GOSUB 1000:PRINT " (";D;:INPUT ")Km nivelados:",D:CLS:PRINT ".....";
910 eem=ROUND(ea/636620*dn*1E3,-4):eeem=ROUND( m*1E3 *(beta*1E4)/636620*TANbeta,-4:em=ROUND(SQR(eem^2+eeem^2),-4:ek=ROUND( em*SQR(1E3/dn),-4):ez=ROUND( ek*SQRD/1E3,-4
920 CLS:PRINT "em'=";ea;"/r";dn;"=";eem;"mm","em''="; m;beta;"/r TAN";beta;"=";TAB(20);eeem;"mm","em=";em;"mm";TAB(0),"ek=em‚(1000/";  dn;")=";ek;"mm";TAB(0) "ez=ek‚"; D;"=";ez;"m":GOTO20
930 CLS:PRINT "[ Triangulacion ]æcalculo Ea,Eb";TAB(0)"(";ea;:INPUT ")ea:",ea:IF ea=0 THENGOSUB 99:CLS:PRINT "[ Triangulacion ]æcalculo Ea,Eb";:ea=.8*ea:GOSUB 1000:PRINT " ea æ (80%) ea =";ea;"cc"
932 GOSUB 1000:PRINT " (";L;:INPUT ")L:",L:GOSUB 1000:PRINT " (";B;:INPUT ")B:",B:GOSUB 1000:PRINT " (";erelB;:INPUT ")‹ m.bse:",erelB:GOSUB 1000: PRINT " (";nn;:INPUT ")n.encadena.:",nn
935 GOSUB 1000:PRINT " (";n;:INPUT ")n med.base:",n
940 CLS:PRINT ".....";: Ea=ROUND(  L*ea/636620*SQR((nn*(nn+1)*(2*nn+1))/6+3),-4:Eb=ROUND(  (nn+1)/2*(erelB/SQRn)*B,-4:ET=ROUND( SQR(Ea^2+Eb^2),-4
950 CLS:PRINT "Ea=";L;"¥";ea;"/r";TAB(0)"‚((n(n+1)(2n+1))/6+3)=";Ea;"m","Eb=(n+1)/2"; erelB;"¥"; B;"=";TAB(20);Eb;"m","Etotal=";ET;"m"   :GOTO30
1000 IF FRE0<200 AND min=0 THEN CLS:PRINT "OFITEC:  FRE0<200B !!!!!";:BEEP:  A$=INPUT$(1):min=1
1010 LOCATE31,0:PRINT "                               ";:LOCATE31,0:RETURN
1100 CLS:PRINT "[ Iterseccion Directa ]";TAB(0)"(";dst;:INPUT ")L:",dst:GOSUB1000:PRINT " (";ea;:INPUT ")ea:",ea:IF ea=0 THEN GOSUB99:CLS:PRINT "[ Interseccion Directa ]";
1105 GOSUB 1000:PRINT " (";alfaa;:INPUT ")ˆ:(ang.en V):",alfaa:CLS:PRINT ".....";
1110 IF alfaa>100 THEN alfaa=200-alfaa: CLS:PRINT "ˆ>100  ˆ=200-ˆ=";alfaa:CLS:PRINT ".....";
1120 e=ROUND(   (dst*ea)/(636620*SIN(alfaa/2)),-4  :CLS:PRINT "e=(";dst;"¥";ea;")/";TAB(0)"(r SIN(";alfaa;"/2))=";e;"m":GOTO30
1200 CLS:PRINT "[ äæ ]";TAB(0)"‘[deg æ gra] ’[gra æ deg]";:a$=INPUT$(1):ON VAL(a$) GOTO 1205,1220:IF a$="0" THEN 30
1205 CLS:PRINT "[deg æ gra]";TAB(0);"(";DMS$(de);")";:INPUT ":",de1:GOSUB 1000:PRINT " (";DMS$(de);")";DMS$(de1) ;:INPUT ":",de2:GOSUB 1000:PRINT " (";DMS$(de);")";DMS$(DEG(de1,de2)); :INPUT ":",de3
1210 de=DEG(de1,de2,de3):gr=ROUND(  (200*de)/180,-5  :CLS:PRINT DMS$(de);"æ";gr;"g" :GOTO30
1220 CLS:PRINT "[gra æ deg]";TAB(0);"(";gr;:INPUT "):",gr:de=(gr*180)/200:CLS:PRINT gr;"æ";DMS$( DEG(de):GOTO30                 '
1300 CLS:PRINT "[ Interseccion Inversa ]";TAB(0)"(no disponible)";:FOR i=1TO300:NEXT:GOTO30
1500 CLS:PRINT "[ estadia horizontal ]";TAB(0)"(";H;:INPUT ")H:",H:GOSUB 1000:PRINT " (";b;:INPUT ")b:",b:GOSUB 1000:PRINT " (";ds;:INPUT ")ds(cc):",ds:dH=ROUND(H^2/b*ds/636620,-4):CLS:PRINT "dH=";H;"’/";b;ds;"/r=";TAB(0);dH;"m":   GOTO20
1550 CLS:PRINT "[ cinta ]";TAB(0);"(";dst;:INPUT ")L:",dst:eL=ROUND(.00032*dst+.0078*SQRdst,-4)  :CLS:PRINT "e=0.00032";dst;"+0.0078‚";dst;TAB(20) "=";eL;"m":GOTO20
1999 'recapitulacion
2000 CLS:PRINT "OFITEC   [ RECAPITULACION ]";:FOR i=1TO300:NEXT:  IF go<>1 THEN DIM rtabla$(7,3),rtabla(7,3),ttolerancia(7),empr(7),empa(7):go=1
2010 CLS:PRINT "Fase( 1 )[RED PRINCIPAL]";TAB(0)"(";empr(1);:INPUT ")Empr:",empr(1)
2015 IF empr(1)=0 THEN LOCATE25,0:PRINT " Empr?";:GOSUB 1000:PRINT " (";longitud;:INPUT ")Longitud:",longitud:GOSUB 1000:PRINT " (";ea;:INPUT ")ea:",ea:IF ea=0 THEN GOSUB 99:CLS:PRINT "fase( 1 )[RED PRINCIPAL]  Empr?";
2017 IF empr(1)=0 THEN GOSUB 1000:PRINT " (";alfa;:INPUT ")alfa:",alfa
2020 IF empr(1)=0 THEN  empr(1)=ROUND( longitud*((.8*ea)/(636620*SIN(alfa/2))),-4) :CLS:PRINT "Empr=(L ea)/(r Sen(ˆ/2))=";TAB(20);empr(1);"m"
2050 CLS:PRINT"Fase( 1 ):red principal   Empa?";TAB(0)"(";emedbase;:INPUT ")Emedbase:",emedbase:GOSUB 1000:PRINT" (";emedang;:INPUT ")Emedang:",emedang:empa(1)=ROUND(SQR(emedbase^2+emedang^2),-4:GOSUB 1000:PRINT " Empa=";empa(1);"m":i=1:rr$=""
2055 DIMf$(6):f$(1)="TRI2":f$(2)="POL1":f$(3)="POL2":f$(4)="POL Brujula":f$(5)="RADIACION":f$(6)="INTERSECC"
2060 i=i+1:CLS:PRINT "Fase";i;"[";rtabla$(i,1);"]";TAB(0);"< 0 > salir";:j=0:rr$="":PRINT CHR$(9);
2065 LOCATE5,0:r$=INPUT$(1):IFr$<>CHR$(30) AND r$<>CHR$(31)THEN rr$=rr$+r$ELSErr$=""
2067 IF r$=CHR$(13)THEN 2080
2068 rtabla$(i,1)=rr$
2069 LOCATE8,0:PRINT "                    ";:IF r$<>CHR$(30) AND r$<>CHR$(31) ANDr$<>"0"  THEN LOCATE8,0: PRINT rtabla$(i,1);"]";:GOTO 2065
2070 IF r$="0"THEN2110
2071 IF r$=CHR$(31) THEN j=j+1
2072 IF r$=CHR$(30) THEN j=j-1
2073 IF j>6 THEN j=1
2074 IF j<1 THEN j=6
2075 rtabla$(i,1)=f$(j):LOCATE8,0:PRINT rtabla$(i,1);"]";
2079 GOTO2065
2080 GOSUB 1000:PRINT " (";rtabla(i,2);:INPUT ")error:",rtabla(i,2)
2090 GOSUB 1000:PRINT " Fig encuadrada 1[si] 0[no]";:a$=INPUT$(1):IF a$="1" OR a$="0"THEN rtabla(i,3)=VAL(a$) ELSE GOTO2090
2100 IF i<7THEN2060ELSE GOTO2110
2110 CLS:PRINT "...";:ERASEf$
2120 FOR j= 2 TOi:PRINT ".";:IF rtabla(j,3)=1THEN er=rtabla(j,2)/2 ELSE er=rtabla(j,2)
2130 empr(j)=ROUND(   SQR(er^2+empr(j-1)^2,-4):empa(j)=ROUND( SQR(empr(j)^2+empa(1)^2),-4):IF j<>2THEN ttolerancia(j)=ROUND(   SQR(2*empr(j-1)^2+rtabla(j,2)^2),-4)ELSEttolerancia(j)=ROUND(    SQR(empr(j-1)^2+rtabla(j,2)^2,-4)
2140 NEXTj
2150 FOR j=2TOi-1:CLS:PRINT "Fase:";rtabla$(j,1);
2155 LOCATE17,0: IF rtabla(j,3)=0 THEN PRINT " T= -";ELSEPRINT " T= ";ttolerancia(j);"m";:IF empa(j)>ttolerancia(j)THENPRINT " *";
2160 PRINT TAB(0)"Empr:";empr(j);"m"; "  Empa:";empa(j);"m" :NEXTj:GOTO30
3000 CLS:PRINT"[Tolerancias]";TAB(0);"Tp=0.2 N(80%)   Ta=1/4 eq(80%)","[restitucion] ep=mb ‚2 ml";TAB(0)"ml: ptoæ0.02mm  detalleæ0.01mm","[restituidor]";TAB(0)"ez=(mb dPa K)/(2(1-p)TANw/2","K:planoæ1 poco movidoæ1.5";TAB(0)"urbanoæ2 muy acciden.æ3"
3005 f=0:GOTO30
3010 CLS:PRINT "[ea]  ev=1/[] s";TAB(2)"[eahæ12  eavæ3  burb.partæ20]":LOCATE 5,0:PRINT "el=2/3 ap (1/‚2)<1/‚2>";TAB(5)"(Bessel) <2 indices>      ":LOCATE 5,0:PRINT "ep=[]/A(1+4A/100) (1/‚2)(B)[eahæ30 eavæ30cu,60coin,150noc]"
3020 CLS:PRINT "[ea] ed=(ee+es)/D r":CLS:PRINT "[ea] eah:ev,el,ep,ed";TAB(0)"eav: (tri):eh,el,ep (geo):eh,ep","[ea] Repeticion:";TAB(0)"ea=‚((2el/n)’+(2ep/n)’+ev’+ed’)","[ea] Reiteracion";TAB(0)"ea=‚(2/n (el’+ep’)+ev’+ed’)":f=0:  GOTO30
3030 CLS:PRINT "[mira] ‹=‚(‹D+‹I)";TAB(0)"‹D=1/3 d/D K‚2":GOSUB 1000:PRINT " D=5A+[]  [mira:1cmæ55 2cmæ140]":GOSUB 1000:PRINT " ‹I= r TANˆ":f=0: GOTO20
3040 CLS:PRINT "[eL]";TAB(0);"eL=‹/‚(n med dist) D‚(n tramos)","[eL] lados desiguales";TAB(0)"eL= ‚„(‹ Lado)’","[eL] villani";TAB(0)"Dtramo= 2Dtramo":f=0:GOTO20
3050 CLS:PRINT "[distanciometro]";TAB(0);"eL=‚((SINv dG)’+(COSv Dg dv/r)’":CLS:PRINT "eL=‚((SINv dG)’+(COSv Dg dv/r)’";TAB(0)"v=100-ˆ":GOSUB 1000:PRINT " dG=‚(G1’+G2’): G1=epd,G2=mSIN‰":GOSUB 1000:PRINT " Dg=Dr          dv=ea":f=0: GOTO20
3060 CLS:PRINT "[ eT ]  RADiacion";TAB(0)"eT=ea/r D [‚2]  [no autoorient]","[ eT ]  POLigonal (no auto)";TAB(0)"eT=ea/r D‚2‚((n(n+1)(2n+1)/6)","[ eT ]  POLigonal (auto)";TAB(0)"eT=D/r ea‚n    n:n de tramos"
3065 PRINT "[eT] lados desiguales";TAB(0)"eT=1/r‚(„((Dist.Fin)’(ea1’-ea2’","ea1=ea(D=ant), ea2=ea(D=prox)","[eT] villani";TAB(0)"Dtramo=2Dtramo, ‹=(D‚2)/(‚2 ‚2)":f=0: GOTO20
3070 CLS:PRINT "[ Triangulacion ] ea (80% ea)";TAB(0)"Ea=Lea/r ‚((n(n+1)(2n+1))/6 +3)","[ Triangulacion ] Et=‚(Ea’+Eb’) Eb=(n+1)/2 ‹ B","[ Trilateracion ] ea=el/L r";TAB(0)   "Ea(trilateracion)=Ea(triang.)":f=0:GOTO30
3080 CLS:PRINT "[ eZ ] Nivelacion Trigonometrica","e't=D((1+‹)TANˆ-TANˆ)";TAB(0)"e''t=D(TAN(ˆ+eav)-TAN ˆ)","ei=0.01";TAB(0)"em=m ‰/r TAN(ˆ+‰)"
3090 CLS:PRINT "[ em ] Nivelacion Geometrica";TAB(0)"e'm=ea/r Dn, e''m=m ‰/r TAN ‰":f=0:GOTO20
3100 CLS:PRINT "[ Recapitulacion ]","Empr=‚((e prop. fig)/[2])’+";TAB(5)"Empr(ptos encuadran fig)’","Empa=‚((Empr de la fig)’+";TAB(5)"(Empa de red principal)’","T=‚((Empr pto salida)’+";TAB(0)"(Empr pto lleg.)’+(e prop fig)’","Empr:RGN=0.15m  NAP=0.3mm";
3105 PRINT TAB(0):f=0:GOTO30
3110 CLS:PRINT "[Estadia horizontal] dH=H’/b ds H:dist. a la estadia","b: longitud de la estadia";TAB(0)"ds:error med. ang. central =ap":f=0:GOTO20
3120 CLS:PRINT "[ Interseccion directa ]";TAB(0)"e=(L ea)/(r SIN(ˆ/2)":f=0:GOTO30
3130 CLS:PRINT "[ cinta ]";TAB(0)"e=0.00032 L + 0.0078 ‚L":f=0:GOTO20
3150 CLS:PRINT "[iI]            e=( (ea‚2/r2S)";TAB(0)"‚(LadoMayor’+LadoIntermedio’) )","[iI]  A-B-C";TAB(7)"ïP/","[iI] Lados?      ab=AB/(PA PB)   bc=BC/(PB PC)   ac=AC/(PA PC)","[iI] Superficie? p=(ab+bc+ac)/2 S=‚(p(p-ab)(p-bc)(p-ac)":f=0:GOTO30
10000 CLS:PRINT"OFITEC  fin del programa.....";:FOR i=1TO200:NEXT:CLS:END
10001 '    [       OFITEC      ]  '        v: 1.00 (enero 1997 )  '        OFIcina TECnica topo.  '        J.Turegano.M (MERIDA)  '        EUPolitecnica  Merida  '        [ Casio Fx 880/850P ]  '        vol:18318B BasicPB100
10100 IF ERR=2 THEN RESUME10120
10110 CLS:control=control+1:PRINT "OFITEC                error!!!!";:FOR i=1TO300:NEXT:IF control<6 THEN RESUME10120ELSECLS:PRINT "OFITEC: Version Defectuosa !!!!!";:END
10120 ONsalida GOTO20,30
