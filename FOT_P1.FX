1 END: CLEAR
2 CLS:PRINT "       FOTOGRAMETRIA II:        �FIN  �MM CC  �TRANS.3D(mm cc)";:J$=INPUT$(1):JS=VAL(J$)
4 ON JS GOSUB 10,2000
6 IF J$="0" THEN CLEAR :SETN:CLS:END
8 GOTO 2
10 ERASE A,W,AT,L,N,AO,NN,T,X,V,P
20 CLS:INPUT"  AJUSTE POR MINIMOS CUADRADOS  NUMERO DE ECUACIONES:",M
30 INPUT"NUMERO DE INCOGNITAS:",N
40 DIMA(M,N),AT(N,M),L(M),AO(N,M),W(M,M),N(N,N),NN(N,N),T(N),X(N),V(M),P(N)
50 PRINT"introducir matriz A";:PRINT
60 FORJ=1TON
70 FORI=1TOM
80 PRINT CHR$(48+I);CHR$(96+J);:INPUT":",A(I,J)
90 NEXTI
100 PRINT"TODO BIEN [S/n]?";:PRINT:A$=INPUT$(1)
110 IF A$="N" OR A$="n" THEN 70
120 NEXTJ
130 PRINT"Introducir matriz L";:PRINT
140 FORI=1TOM
150 PRINT"K";CHR$(48+I);:INPUT":",L(I)
160 NEXTI
170 PRINT"TODO BIEN [S/n]?";:PRINT:A$=INPUT$(1)
180 IF A$="N" OR A$="n" THEN 140
200 FORI=1TOM:W(I,I)=1:NEXTI
210 PRINT"Introducir pesos [s/N]?";:PRINT:A$=INPUT$(1)
240 IF NOT(A$="S"ORA$="s") THEN 294
250 FORI=1TOM
260 PRINT"w(";I;:INPUT "):",W(I,I)
270 NEXTI
280 PRINT"TODO BIEN [S/n],";:PRINT:A$=INPUT$(1)
290 IF A$="N" OR A$="n" THEN 250
294 PRINT"LISTADOS O PROCESO [L/P]? ";:PRINT:PROC$=INPUT$(1)
296 IF PROC$="p" OR PROC$="P" THEN PROC=1 ELSE PROC=0
300 PRINT"Calculando A'....";:PRINT
310 FORI=1TOM
320 FORJ=1TON
330 AT(J,I)=A(I,J)
340 NEXTJ
350 NEXTI
360 PRINT"Calculando A'*W....";:PRINT
370 X=0
380 FORL=1TON
390 FORI=1TOM
400 FORJ=1TOM
410 X=X+AT(L,J)*W(J,I)
420 NEXTJ
430 AO(L,I)=X:X=0
440 NEXTI
450 NEXTL
455 IF PROC=1 THEN 540
460 PRINT"Listar matriz A'*W [S/n]?";:PRINT:A$=INPUT$(1)
470 IF A$="N" OR A$="n" THEN 540
480 FORI=1TON
490 FORL=1TOM
500 PRINT"A'*W(";I;",";L;")=";AO(I,L)
510 NEXTL
520 NEXTI
540 PRINT"Calculando N=(A'*W)*A....";:PRINT
550 X=0
560 FORL=1TON
570 FORI=1TON
580 FORJ=1TOM
590 X=X+AO(L,J)*A(J,I)
600 NEXTJ
610 N(L,I)=X:X=0
620 NEXTI
630 NEXTL
635 IF PROC=1 THEN 690
640 PRINT"Listar matriz N [S/n]";:PRINT:A$=INPUT$(1)
650 IF A$="N" OR A$="n" THEN 690
660 FORI=1TON
670 FORL=1TON
680 PRINTCHR$(96+I);CHR$(96+L);"=";N(I,L):NEXT :NEXT
690 PRINT"Invirtiendo N....";:PRINT
700 FORI=1TON
710 NN(I,I)=1
720 NEXTI
730 FORJ=1TON
740 FOR I=J TO N
750 IF N(I,J)<>0 THEN 780
760 NEXTI
770 PRINT"No tiene inversa";:PRINT:END
780 FORKK=1TON
790 X=N(J,KK)
800 N(J,KK)=N(I,KK)
810 X=N(I,KK)
820 XB=NN(J,KK)
830 NN(I,KK)=XB
840 NEXTKK
850 Y=1/N(J,J)
860 FORKK=1TON
870 N(J,KK)=N(J,KK)*Y
880 NN(J,KK)=NN(J,KK)*Y
890 NEXTKK
900 FORL=1TON
910 IF L=J THEN 970
920 Y=-N(L,J)
930 FORKK=1TON
940 N(L,KK)=N(L,KK)+N(J,KK)*Y
950 NN(L,KK)=NN(L,KK)+NN(J,KK)*Y
960 NEXTKK
970 NEXTL
980 NEXTJ
982 IF PROC=1 THEN 1050
985 PRINT"Listar matriz Inversa  [S/n]?";:PRINT:A$=INPUT$(1):IF A$="N" OR A$="n" THEN 1050
990 PRINT"Listado de inversa de N";:PRINT
1000 FORI=1TON
1010 FORJ=1TON
1020 PRINTCHR$(96+I);CHR$(96+J);"=";NN(I,J)
1030 NEXTJ
1040 NEXTI
1050 PRINT"Calculando T=(A'*W)*L....";:PRINT
1060 X=0
1070 FORI=1TON
1080 FORJ=1TOM
1090 X=X+AO(I,J)*L(J)
1100 NEXTJ
1110 T(I)=X
1120 X=0
1130 NEXTI
1132 IF PROC=1 THEN 1180
1135 PRINT"Listar matriz T=(A'*W)*L [S/n]?";:PRINT:A$=INPUT$(1):IF A$="N" OR A$="n" OR PROC$="SI" THEN 1180
1140 PRINT"Listado de T....";:PRINT
1150 FORI=1TON
1160 PRINT"T";I;"=";T(I)
1170 NEXTI
1180 PRINT"Calculando matriz X....";:PRINT
1190 X=0
1200 FORI=1TON
1210 FORJ=1TON
1220 X=X+NN(I,J)*T(J)
1230 NEXTJ
1240 X(I)=X
1250 X=0
1260 NEXTI
1270 BEEP1:PRINT"Listado de matriz X";:PRINT
1280 FORI=1TON
1290 PRINT"X(";I;")=";X(I)
1300 NEXTI
1310 PRINT"Residuos....";:PRINT
1320 X=0:EMC=0
1330 FORI=1TOM
1340 FORJ=1TON
1350 X=X+A(I,J)*X(J)
1360 NEXTJ
1370 V(I)=X-L(I):X=0:EMC=EMC+(V(I))^2*W(I,I)
1380 NEXTI
1400 FORI=1TOM
1410 PRINT"v(";I;")=";V(I)
1420 NEXTI
1425 IF M-N=0 THEN PRINT " No.ECUACIONES = No.INCOGNITAS     NO HAY COMPROBACION !!!  ":RETURN
1430 EMC=SQR(EMC/(M-N))
1440 PRINT"emc=";EMC
1450 FORI=1TON
1460 P(I)=1.96*EMC*NN(I,I)
1470 NEXTI
1480 PRINT"Precision observaciones (95%)";:PRINT
1490 FORI=1TON
1500 PRINT"s(";I;")=";P(I)
1510 NEXTI
1520 RETURN
2000 CLS:MODE6:PRINT" *** TRANSFORMACION 3D *** ";:FOR I=1TO150:NEXT I
2010 CLS:PRINT "INTRODUCCION DE DATOS";TAB(0);"NUMERO DE PUNTOS: ";:NP$=INPUT$(1):NP=VAL(NP$)
2020 IF NP<3 THEN CLS:PRINT "SE NECESITAN AL MENOS 3 PUNTOS":GOTO 2010
2030 DIM NPT(NP),CTX(NP),CTY(NP),CTZ(NP),CMX(NP),CMY(NP),CMZ(NP)
2040 J=1:CLS:PRINT "COORDENADAS TERRENO";:FOR I=1TO150:NEXT I:CLS
2050 FOR I=1 TO NP
2060 CLS:PRINT "N�DE PUNTO:";J;:INPUT J:NPT(I)=J:J=J+1
2070 PRINT "X";NPT(I);:INPUT ":",X:CTX(I)=X
2080 PRINT "Y";NPT(I);:INPUT ":",Y:CTY(I)=Y
2090 PRINT "Z";NPT(I);:INPUT ":",Z:CTZ(I)=Z
2095 PRINT X;Y;TAB(0);Z;"    CORRECTO [S/n]";:S$=INPUT$(1):IF S$="N" OR S$="n" THEN J=J-1:GOTO 2060
2100 NEXT I
2105 CLS:PRINT "COORDENADAS MODELO";:FOR I=1TO150:NEXT I
2110 FOR I=1 TO NP
2130 CLS:PRINT "x";NPT(I);:INPUT ":",X:CMX(I)=X
2140 PRINT "y";NPT(I);:INPUT ":",Y:CMY(I)=Y
2150 PRINT "z";NPT(I);:INPUT ":",Z:CMZ(I)=Z
2155 PRINT X;Y;Z;TAB(0);"CORRECTO [S/n]";:S$=INPUT$(1):IF S$="N" OR S$="n" THEN I=I-1:GOTO 2130
2160 NEXT I
2170 PRINT "OMEGA FHI KAPPA APROX.[s/N]";:S$=INPUT$(1):IF S$="s" OR S$="S" THEN 2190
2180 W=0:F=0:K=0:GOTO 2200
2190 INPUT "OMEGA";W,"FHI";F,"KAPPA";K:PRINT "CORRECTO [S/n]";:S$=INPUT$(1):IF S$="N" OR S$="n" THEN 2190
2200 DM=0:dm=0:APRO=0:CLS:PRINT "CALCULO DE DISTANCIAS";:FOR I=1TO150:NEXT I:CLS
2210 FOR I=1 TO NP
2220 FOR J=I+1 TO NP
2230 D=SQR((CTX(I)-CTX(J))^2+(CTY(I)-CTY(J))^2+(CTZ(I)-CTZ(J))^2):DM=DM+D
2240 d=SQR((CMX(I)-CMX(J))^2+(CMY(I)-CMY(J))^2+(CMZ(I)-CMZ(J))^2):dm=dm+d
2260 PRINT "D[";NPT(I);"-";NPT(J);"]";TAB(16);"d[";NPT(I);"-";NPT(J);"]";TAB(0);ROUND(D,-4);TAB(16);ROUND(d,-4)
2270 NEXT J
2280 NEXT I
2290 FAC=FACTNP/(FACT2*FACT(NP-2)):DM=DM/FAC:dm=dm/FAC:LAMBDA=DM/dm
2300 PRINT "D.Media Terr.";TAB(16);"D.Media Mod.";TAB(0);ROUND(DM,-4);TAB(16);ROUND(dm,-4)
2310 PRINT CHR$(145+APRO);"Coef.Escala=";LAMBDA:TXM=0:TYM=0:TZM=0
2320 FOR I=1 TO NP
2330 TX=CTX(I)-LAMBDA*CMX(I):TY=CTY(I)-LAMBDA*CMY(I):TZ=CTZ(I)-LAMBDA*CMZ(I):TXM=TXM+TX:TYM=TYM+TY:TZM=TZM+TZ
2340 NEXT I
2350 TX=TXM/NP:TY=TYM/NP:TZ=TZM/NP:PRINT CHR$(145+APRO);"Tx=";TX,CHR$(145+APRO);"Ty=";TY,CHR$(145+APRO);"Tz=";TZ
2400 REM COORDENADAS TERRENO PRIMERA APROX
2410 M=3*NP:N=7:DIM R(M,N),A(M,N),CL(M),CAP(M)
2420 J=1
2430 GOSUB 5000
2500 J=1:SETF3
2510 FOR I=1TO NP
2520 PRINT "X";I;"=";CAP(J):J=J+1:PRINT "Y";I;"=";CAP(J):J=J+1:PRINT "Z";I;"=";CAP(J):J=J+1
2540 NEXT I
2560 J=1:SETN
2570 FOR I=1 TO NP
2580 CL(J)=CTX(I)-CAP(J):J=J+1:CL(J)=CTY(I)-CAP(J):J=J+1:CL(J)=CTZ(I)-CAP(J):J=J+1
2590 NEXT I
2600 CLM=0
2610 FOR I=1 TO M
2620 PRINT "L";I;"=";CL(I):CLM=CLM+ABS(CL(I))
2630 NEXT I
2640 CLM=CLM/M:IF CLM<0.0001 THEN 5100
3580 CLS:PRINT "MATRIZ AMPLIADA A.... ";
3590 I=1
3600 FOR P=1 TO NP:J=1
3620 R(I,J)=COSK*COSF*CMX(P)-SINK*COSF*CMY(P)+SINF*CMZ(P):J=J+1
3630 R(I,J)=0:J=J+1
3640 R(I,J)=LAMBDA*((-COSK*SINF)*CMX(P)+SINK*SINF*CMY(P)+COSF*CMZ(P)):J=J+1
3650 R(I,J)=LAMBDA*((-SINK*COSF)*CMX(P)-COSK*COSF*CMY(P)):J=J+1
3660 R(I,J)=1:J=J+1
3670 R(I,J)=0:J=J+1
3680 R(I,J)=0:J=J+1
3700 I=I+1:J=1
3710 R(I,J)=CMX(P)*(SINK*COSW+SINF*COSK*SINW)+CMY(P)*(COSK*COSW-SINK*SINF*SINW)-COSF*SINW*CMZ(P):J=J+1
3720 R(I,J)=LAMBDA*((CMX(P)*(-SINK*SINW+SINF*COSK*COSW)+CMY(P)*(-COSK*SINW-SINK*SINF*COSW)-COSF*COSW*CMZ(P)):J=J+1
3730 R(I,J)=LAMBDA*(CMX(P)*(COSF*COSK*SINW)+CMY(P)*(-SINK*COSF*SINW)+SINF*SINW*CMZ(P)):J=J+1
3740 R(I,J)=LAMBDA*(CMX(P)*(COSK*COSW-SINF*SINK*SINW)+CMY(P)*(-SINK*COSW-COSK*SINF*SINW)):J=J+1
3750 R(I,J)=0:J=J+1
3760 R(I,J)=1:J=J+1
3780 R(I,J)=0:J=J+1
3790 I=I+1:J=1
3800 R(I,J)=CMX(P)*(SINK*SINW-SINF*COSK*COSW)+CMY(P)*(COSK*SINW+SINK*SINF*COSW)+COSF*COSW*CMZ(P):J=J+1
3810 R(I,J)=LAMBDA*(CMX(P)*(SINK*COSW+SINF*COSK*SINW)+CMY(P)*(COSK*COSW-SINK*SINF*SINW)-COSF*SINW*CMZ(P):J=J+1
3820 R(I,J)=LAMBDA*(CMX(P)*(-COSF*COSK*COSW)+CMY(P)*(SINK*COSF*COSW)-SINF*COSW*CMZ(P)):J=J+1
3830 R(I,J)=LAMBDA*(CMX(P)*(COSK*SINW+SINF*SINK*COSW)+CMY(P)*(-SINK*SINW+COSK*SINF*COSW)):J=J+1
3840 R(I,J)=0:J=J+1
3850 R(I,J)=0:J=J+1
3860 R(I,J)=1:J=J+1
3870 I=I+1
3880 NEXT P
3890 CLS:REM "PRESENTACION DE RESULTADOS DE LA MATRIZ A"
3895 PRINT"Listar matriz A ampliada [S/n]?";:PRINT:A$=INPUT$(1):IF A$="N" OR A$="n" THEN 4000
3900 FOR I=1 TO 3*NP
3910 FOR J=1 TO 7
3920 PRINT "r";I;J;"=";R(I,J)
3930 NEXT J
3940 NEXT I
4000 PRINT "PROCESO ITERATIVO S/n?";:A$=INPUT$(1)
4010 IF A$="N" OR A$="n" THEN END ELSE 4510
4020 GOTO 4000
4510 PRINT "....";:ERASE A,W,AT,L,N,AO,NN,T,X,V,P
4540 DIMA(M,N),AT(N,M),L(M),AO(N,M),W(M,M),N(N,N),NN(N,N),T(N),X(N),V(M),P(N)
4550 FORI=1TOM
4560 FORJ=1TON
4570 A(I,J)=R(I,J)
4580 NEXTJ
4590 NEXTI
4600 FORI=1TOM
4610 L(I)=CL(I)
4620 NEXTI
4630 CLS:GOSUB 200
4650 W=W+(X(2)*400/(2*PI)):F=F+(X(3)*400/(2*PI)):K=K+(X(4)*(400/(2*PI)
4660 LAMBDA=LAMBDA+X(1):TX=TX+X(5):TY=TY+X(6):TZ=TZ+X(7)
4680 APRO=APRO+1:PRINT"VALORES DE APROX.";CHR$(144+APRO);":"
4690 PRINT "LAMBDA=";LAMBDA,"OMEGA[g]=";W,"FHI[g]=";F,"KAPPA[g]=";K,"Tx=";TX,"Ty=";TY,"Tz=";TZ
4710 GOTO 2420
5000 A11=COSF*COSK:A12=-COSF*SINK:A13=SINF:A21=COSW*SINK+SINW*SINF*COSK:A22=COSW*COSK-SINW*SINF*SINK:A23=-SINW*COSF:A31=SINW*SINK-COSW*SINF*COSK:A32=SINW*COSK+COSW*SINF*SINK:A33=COSW*COSF
5005 PRINT "MATRIZ DE ROTACION [R]";TAB(0);"a11=";A11,"a12=";A12,"a13=";A13,"a21=";A21,"a22=";A22,"a23=";A23,"a31=";A31,"a32=";A32,"a33=";A33
5010 J=1
5020 FOR I=1 TO NP
5030 CAP(J)=LAMBDA*(A11*CMX(I)+A12*CMY(I)+A13*CMZ(I))+TX:J=J+1:CAP(J)=LAMBDA*(A21*CMX(I)+A22*CMY(I)+A23*CMZ(I))+TY:J=J+1:CAP(J)=LAMBDA*(A31*CMX(I)+A32*CMY(I)+A33*CMZ(I))+TZ:J=J+1
5040 NEXT I
5050 RETURN
5100 CLS:PRINT "VALORES FINALES:";TAB(0);"LAMBDA=";LAMBDA,"OMEGA=";W,"FHI=";F,"KAPPA=";K,"Tx=";TX,"Ty=";TY,"Tz=";TZ

