9000 'excentricas
9010 'entrada de datos
9020 CLS:INPUT"LHa'�a:",laa:INPUT "LHa'�b:",lab:INPUT "Da'�a:",d:INPUT "Da'�b:",D
9030 'calculo
9040 DD=SQR(D^2+d^2-2*D*d*COS(lab-laa)):PRINT "Da�b=";DD
9050 a=(d*636620*SIN(lab-laa))/DD:PRINT "a'ba=";a;"cc"
9060 lhab=lab+a/1E4:PRINT "LHa�b=";lhab
