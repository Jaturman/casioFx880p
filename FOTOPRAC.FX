150 dim TURE(10,2),TURE$(10)
160 cls:print"1[XYZ] 2[D] 3[3D]";:on val(a$) goto 5010,320,10:goto200

320 CLS:PRINT "[ UTM ] XYZÊD";:GOSUB 900:GOSUB3200:GOTO300




900 'CONTROL "ENTRO2P"
910 INPUT" PTO: ",np$:LOCATE0,0:GOSUB 1000:np1=np:np1$=TURE$(np)
930 GOSUB990:PRINT" PTO: ";np$;" Ê ";:INPUT"",np$:LOCATE0,0:GOSUB 1000:np2=np:np2$=TURE$(np)
940 RETURN






3199 'CALCULO Y SALIDA XY=>D
3200 CLS:PRINT "XYÊD";:FORi=1TO500:NEXT
3210 D=sqr((TURE(np1,0)-TURE(np2,0))^2+(TURE(np1,1)-TURE(np2,1))^2+(TURE(np1,2)-TURE(np2,2))^2)
3220 CLS:SETn:PRINT "[Dutm](";np1$;"Ê";np2$;")=";:SETf3:PRINT   D;"m":RETURN

5010 'introduccion de punto
5020 CLS:INPUT "PTO: ",np$:GOSUB1000
5130 PRINT "PTO:";TURE$(np);
5140 INPUT" X: ",TURE(np,0):INPUT "Y: ",TURE(np,1):input"Z: ",TURE(np,2)




10'entrada de parametros aproximados
20CLS:INPUT"Ω: ",L:input"Ö: ",O:input"è :",F:input"k: ",K
30input"Tx",TX:input"Ty",TY:input"Tz",TZ

40'entrada de coordenadas
50for i=0to 3
60print"XYZ";i;:input":",np$
70gosub 800
80X(i)=TURE(np,0
90Y(i)=TURE(np,1
100Z(i)=TURE(np,2
110next

120for i=1to 4
130print"xyz";i;:input":",np$
140gosub 800
150x(i)=TURE(np,0
160y(i)=TURE(np,1
170z(i)=TURE(np,2
180next


190'calculo de coeficientes
200for i=1 to3
210A(i,1)=x(i)+y(i)*K-z(i)*O
220A(i,2)=0
230A(i,3)=L*(-x(i)*F+z(i))
240A(i,4)=L*(-x(i)*K-y(i))
250A(i,5)=1
260A(i,6)=0
270A(i,7)=0

280A(i+1,1)=-x(i)*K+y(i)+z(i)*O
290A(i+1,2)=-L*(x(i)*F-y(i)*O+z(i))
300A(i+1,3)=L*(x(i)*O)
310A(i+1,4)=L(x(i)+y(i)*K
320A(i+1,5)=0
330A(i+1,6)=1
340A(i+1,7)=0

350A(i+2,1)=x(i)*F-y(i)*O+z(i)
360A(i+2,2)=L*(-x(i)*K+y(i)*O
370A(i+2,3)=L*(-x(i)+y(i)*K-z(i)*F
380A(i+2,4)=L*(-x(i)*O-y(i)*F
390A(i+2,5)=0
400A(i+2,6)=0
410A(i+2,7)=1
420next







430' calculo de coordenadas aprox

440for i=1to4
450Xo(i)=L*(x(i)+y(i)*K-z(i)*F)+TX
460Yo(i)=L*(-x(i)*K+y(i)+z(i)*O)+TY
470Zo(i)=L*(x(i)*F-y(i)*O+z(i))+TZ

480L(i)=X(i)-Xo(i)
490L(i+1)=Y(i)-Yo(i)
500L(i+2)=Z(i)-Zo(i)
510next

600 cls:print"TRANSFIRIENDO....";:OPEN"COM0:6" AS #1
610 PRINT#1,"Ω=";L,"Ö=";O,"è=";F,"k=";K
620PRINT#1,TAB(0);"Tx=";TX,"Ty";TY,"Tz";TZ
630 PRINT#1,"MATRIZ [A]":for i=1to12:for j=1to7
640PRINT#1,A(i,j);" ";:next;PRINT#1,TAB(0);:NEXT
650 for i=1to12:PRINT#1,X(i);"-";Xo(i);"=";L(i);tab(0)
660 PRINT#1,Y(i);"-";Yo(i);"=";L(i+1);tab(0)
670 PRINT#1,Z(i);"-";Zo(i);"=";L(i+2);tab(0):next
680 close




1000 'CONTROL "ASOCIO"
1010 FORi=1TO actual:IF np$=TURE$(i) THEN np=i:RETURN
1020 NEXT
1030 actual=actual+1:np=actual:TURE$(actual)=np$:RETURN
