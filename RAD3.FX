8000 'entrada de lecturas
8010 INPUT "n de puntos a radiar:",n:n=n+1:DIM dst(n-1),azi(n-1),lh(n-1),coo(n-1,1)
8020 FOR i=0 TO n-1
8030 PRINT "LHaæ";:IF i=0THENPRINT "ref";ELSEPRINT CHR$(97+i);
8035 INPUT ":",lh(i)
8036 IFi<>0THEN PRINT "Daæ";CHR$(97+i);:INPUT ":",dst(i)
8040 NEXT
8050 INPUT"Xa:",xa:INPUT "Ya:",ya:INPUT "Œaæref:",azi
8060 des=azi-lh(0)
8070 FOR i=0TOn-1
8080 azi(i)=lh(i)+des
8090 IF azi(i)<0 THEN azi(i)=azi(i)+400
8100 NEXT
8110 'calculo de las coord de los ptos radiados
8120 FORi=0TOn-1
8130 z=REC(dst(i),azi(i)):coo(i,0)=xa+Y:coo(i,1)=ya+X
8140 PRINT"X";CHR$(98+i);"=";coo(i,0):PRINT"Y";CHR$(98+i);"=";coo(i,1)
8150 NEXT
