9000 'excentricas
9010 'entrada de datos
9020 CLS:INPUT"LHa'æa:",laa:INPUT "LHa'æb:",lab:INPUT "Da'æa:",d:INPUT "Da'æb:",D
9030 'calculo
9040 DD=SQR(D^2+d^2-2*D*d*COS(lab-laa)):PRINT "Daæb=";DD
9050 a=(d*636620*SIN(lab-laa))/DD:PRINT "a'ba=";a;"cc"
9060 lhab=lab+a/1E4:PRINT "LHaæb=";lhab
