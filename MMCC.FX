1 ON ERROR GOTO5000
5 CLS:SETf0:a=0:M$=STR$(M:N$=STR$(N:M$=MID$(M$,2,LEN(M$)-1:N$=MID$(N$,2,LEN(N$)-1:PRINT"[ MMCC ] A(";M$;",";N$;"):L(";M$;")";TAB(0)">In,Out,Calc,List,S,0 ?";:SETn:IF sound=1THENLOCATE30,0:PRINT"!";
7 RESTORE8:GOSUB 100:FORi=1TO4:GOSUB 100:LOCATE 31,0:GOSUB 100:READ a$:GOSUB 100:PRINT a$;:GOSUB100:NEXT:GOTO7
8 DATA /,-,ο,|
9 IFA$="0"THENLOCATE0,1:PRINT"Quit...                ";:FORi=1TO300:NEXT:CLS:PRINT "mm.cc. por J.Turegano";TAB(0);:END
10 IFA$=CHR$(244)THEN20
12 IFA$="L"ORA$="l"THEN1500
14 IFA$=CHR$(250)THEN300
16 IFA$=CHR$(245)THEN700
17 IFA$="S"OR A$="s" THEN 18 ELSE GOTO5
18 sound=ABS(sound-1):LOCATE0,1:PRINT "sound ";:IF sound=0 THENPRINT "off              ";ELSE PRINT "on               ";
19 FORi=1TO500:NEXT:GOTO5
20 IF N=0 AND M=0 THEN CLS:INPUT"n Ecuations :",M,"n Incognits :",N
40 DIM A(M,N),AT(N,M),L(M),AO(N,M),W(M,M),N(N,N),NN(N,N),NNN(N,N),T(N),X(N),V(M),P(N)
50 CLS:control=55:PRINT "input [ A ]   ( δ :+)";:FOR J=1 TO N:FOR I=1 TO M
55 PRINT TAB(0)"(";A(I,J);")A";I;CHR$(64+J);:INPUT ":",A(I,J):NEXT :NEXT
60 control=65: PRINT "input [ L ]";:FOR I=1 TO M
65 PRINTTAB(0) "(";L(I);")K";I;:INPUT ":",L(I):NEXT I
70 FOR J=1 TO M:W(J,J)=1:NEXT J
80 PRINT "weight [y/N]?";:PRINT :W$=INPUT$(1):IF W$="Y" OR W$="y" THENCLS:PRINT "Input [W]";:FOR J=1 TO M:PRINT TAB(0)"w(";J;:INPUT "):",W(J,J):NEXT J:GOTO 5
90 GOTO5
100 A$=INKEY$:IF A$="" THEN RETURN ELSE GOTO9
300 IF M=0 THEN LOCATE0,1:PRINT "matrix empty !!!           ";:FORi=1TO500:NEXT:GOTO5
305 LOCATE0,1:PRINT "Calc...                ";:EMC=0:ERASE AT,N,AO,NN,NNN,T,X,V,P:DIM AT(N,M),AO(N,M),N(N,N),NN(N,N),NNN(N,N),T(N),X(N),V(M),P(N):CLS:PRINT "Computing [ A' ]";:LOCATE M,1:PRINT ".";:LOCATE 0,1
310 FOR I=1 TO M:PRINT ".";:FOR J=1 TO N:AT(J,I)=A(I,J):NEXT J:NEXT I
320 CLS:PRINT "Computing A'*W";:LOCATEN,1:PRINT ".";:LOCATE 0,1
330 X=0:FOR L=1 TO N:PRINT ".";:FOR I=1 TO M:FOR J=1 TO M:X=X+AT(L,J)*W(J,I):NEXT J:AO(L,I)=X:X=0:NEXT I:NEXT L
340 CLS:PRINT "Computing [ N ]";:LOCATEN,1:PRINT ".";:LOCATE 0,1
350 X=0:FOR L=1 TO N:PRINT ".";:FOR I=1 TO N:FOR J=1 TO M:X=X+AO(L,J)*A(J,I):NEXT J:N(L,I)=X:NNN(L,I)=N(L,I):X=0:NEXT I:NEXT L
360 CLS:PRINT "Computing [N]";:LOCATEN,1:PRINT ".";:LOCATE 0,1
370 FOR I=1 TO N:NN(I,I)=1:NEXT I
380 FOR J=1 TO N:PRINT ".";
390 FOR I=J TO N
400 IF N(I,J)<>0 THEN 430
410 NEXT I
420 LOCATE0,1:PRINT "Inverse no found  !!!";:IF sound<>1 THEN STOP:GOTO5
425 GOTO596
430 FOR K=1 TO N:X=N(J,K):N(J,K)=N(I,K):X=N(I,K):XB=NN(J,K):NN(I,K)=XB:NEXT K
440 Y=1/N(J,J)
450 FOR K=1 TO N:N(J,K)=N(J,K)*Y:NN(J,K)=NN(J,K)*Y:NEXT K
460 FOR L=1 TO N
470 IF L=J THEN 500
480 Y=-N(L,J)
490 FOR K=1 TO N:N(L,K)=N(L,K)+N(J,K)*Y:NN(L,K)=NN(L,K)+NN(J,K)*Y:NEXT K
500 NEXT L:NEXT J
510 CLS:PRINT "Computing [ T ]";:LOCATEN,1:PRINT ".";:LOCATE0,1
520 X=0:FOR I=1 TO N:PRINT ".";:FOR J=1 TO M:X=X+AO(I,J)*L(J):NEXT J:T(I)=X:X=0:NEXT I
530 CLS:PRINT "Computing [ X ]";:LOCATEN,1:PRINT ".";:LOCATE0,1
540 X=0:FOR I=1 TO N:PRINT ".";:FOR J=1 TO N:X=X+NN(I,J)*T(J):NEXT J:X(I)=X:X=0:NEXT I
550 CLS:PRINT "Computing [ V ]";:LOCATEM,1:PRINT ".";:LOCATE0,1
560 X=0:EMC=0:FOR I=1 TO M:PRINT ".";:FOR J=1 TO N:X=X+A(I,J)*X(J):NEXT J:V(I)=X-L(I):X=0:EMC=EMC+V(I)^2*W(I,I):NEXT I
570 EMC=SQR(EMC/(M-N))
580 CLS:PRINT"Computing Precision";:LOCATEN,1:PRINT ".";:LOCATE0,1
590 FOR I=1 TO N:PRINT ".";:P(I)=1.96*EMC*NN(I,I):NEXT I
595 CLS:PRINT "End of calc";:IFsound<>1 THEN STOP:GOTO5
596 IF sound=1 THENBEEP0:BEEP1:FOR i=1TO 500:GOSUB597:NEXT:GOTO596
597 a$=INKEY$:IF a$<>"" THEN 5
598 RETURN
600 CLS:PRINT"end of calc":GOTO 5
700 LOCATE0,1:PRINT "clear data (y/N)  ?    ";:A$=INPUT$(1):IF A$="Y"OR A$="y"THEN PRINT "....";:CLEAR :FORi=1TO500:NEXT:GOTO5
710 GOTO5
1500 IF EMC=0 THEN LOCATE0,1:PRINT "calc empty !!!         ";:FORi=1TO500:NEXT:GOTO5
1505 CLS:PRINT"[A] [L] [V] Emc Pre.";TAB(0)"[A’W] [N] [N] [T] [X]";:A$=INPUT$(1):ON VAL(A$) GOTO 1590,1600,1560,1570,1580,1510,1520,1530,1540:IF A$="0"THEN1550ELSE GOTO5
1510 CLS:FOR I=1 TO N:FOR L=1 TO M:PRINT "A'*W(";I;",";L;")=";AO(I,L):NEXT :NEXT :GOTO1500
1520 CLS:FOR I=1 TO N:FOR L=1 TO N:PRINT "N";CHR$(96+I);CHR$(96+L);"=";NNN(I,L):NEXT :NEXT:GOTO1500
1530 CLS: FOR I=1 TO N:FOR J=1 TO N:PRINT "N";CHR$(96+I);CHR$(96+J);"=";NN(I,J):NEXT J:NEXT:GOTO1500
1540 CLS: FOR I=1 TO N:PRINT "T(";I;")=";T(I):NEXT:GOTO1500
1550 CLS: FOR I=1 TO N:PRINT "X(";I;")=";X(I):NEXT:GOTO1500
1560 CLS: FOR I=1 TO M:PRINT "v(";I;")=";V(I):NEXT:GOTO 1500
1570 CLS:PRINT "emc=";EMC:GOTO1500
1580 CLS: FOR I=1 TO N:PRINT "s(";I;")=";P(I):NEXT:GOTO1500
1590 CLS: FOR I=1 TO N:FOR L=1 TO M:PRINT "A(";L;",";I;")=";A(L,I):NEXT :NEXT :GOTO1500
1600 CLS:FOR I=1 TO M:PRINT "L(";I;")=";L(I):NEXT:GOTO1500
4999 '       [minimos cuadrados]             mod. marzo 1997 Fx.             por JTM.Cuenca,ITT.
5000 IF ERR=2 THEN5020ELSE IF ERR=1 THEN5010ELSE RESUME5050
5010 CLS:PRINT "Out of memory !!!";:CLEAR :END
5020 IF I-1<1 THEN I=M:J=J-1:RESUME5040
5030 I=I-1:RESUME5040
5040 GOTOcontrol
5050 CLS:PRINT "Error !!!";:FORi=1TO500:NEXT:GOTO5
