9100 'calculo de superficies
9110 'entrada de puntos
9120 INPUT"n de vertices:",n
9130 DIM coo(n-1,1):FORi=0TOn-1:FORj=0TO1:PRINT CHR$(125+j);CHR$(85+i);:INPUT ":",coo(i,j)
9140 NEXT:NEXT
9150 'calculo de superficie
9160 sx=0:FORi=1TOn-2
9170 a=coo(i,0)*(coo(i-1,1)-coo(i+1,1)
9180 NEXT
9190 PRINT "Superficie=";a/2
