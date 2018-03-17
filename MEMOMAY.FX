[  FOTOGRAMETRIA II  ]:     ITT  feb '98; Merida. J.Tm                                                                                                                                                                         >
Indice|+>->.>;                  [1]   |+(tema1)[Rest. Analogica][11]  |-Ecuacion Py Px
[2]   |+(tema2)[Transformacion] [21]  |-2D Afin                 [22]  |-Proyectiva              [23]  |-3D Conforme
[3]   |+(tema3)[Rest. Analitica][31]  |-Orientacion Interna
[32]  |-Orientacion externa     [321] |.Coolineacion            [322] |.Coplaneidad             [3221]|;metodo general          [3222]|;por pares consecutivos  [3223]|;por pares independientes[323] |.Calculo Coord. Modelo  >
+ 1                      [tema1]       [ RESTITUCION ANALOGICA ]
11 ec. paralaje
dPy=dby1-dby2+(Y/h)dbz1-((Y-b)/h)dbz2+h(1+Yí/hí)dw1-h(1+Yí/hí)dw2+(XY/h)dŸ1-((X-b)Y/h)dŸ2+(X)dx1-(X-b)dx2
dPx=dbx1-dbx2+(X/h)dbz1-((X-b)/h)dbz2+(YX/h)dw1-((Y-b)X/h)dw2-h(1+Xí/hí)dŸ1+h(1+Xí/hí)dŸ2-(Y)dx1+(Y-b)dx2
+ 2                      [tema2]              [ TRANSFORMACION ]
[R] Matriz de rotacion
a11=cosècosK
a12=-cosèsenK
a13=senè
a21=cosÖsenK+senÖsenècosK
a22=cosÖcosK-senÖsenèsenK
a23=-senÖcosè
a31=senÖsenK-cosÖsenècosK
a32=senÖcosK+cosÖsenèsenK
a33=cosÖcosè
[R] valores pequenos 
senx=x cosx=1 infinit.1er orden
(1) (-Ók)  Óè
 Ók  (1) (-ÓÖ)
(-Óè)  ÓÖ  (1)
- 21: (2D) Afin
X=Ωxx'cosà-                           Ωy'(cosàsenâ+senàcosâ)+Tx
Y=Ωxx'senà+                           Ωy'(cosàcosâ-senàsenâ)+Ty
- 22: PROYECTIVA
X=Xo+(Z-Zo)(a11x'+a12y'+a13c)             /(a31x'+a32y'+a33c)
Y=Yo+(Z-Zo)(a21x'+a22y'+a23c)             /(a31x'+a32y'+a33c)
X=(a1x'+b1y'+c1)/(a3x'+b3y'+1)
Y=(a2x'+b2y'+c2)/(a3x'+b3y'+1)
coeficientes
a1=(Xoa31+(Z-Zo)a11)/(a33c)
b1=(Xoa32+(Z-Zo)a12)/(a33c)
c1=(Xoa33+(Z-Zo)a13)/(a33c)
a3=(a31)/(a33c)
b3=(a32)/(a33c)
- 23: (3D) CONFORME
X=Ω(a11x+a12y+a13z)+Tx
Y=Ω(a21x+a22y+a23z)+Ty
Z=Ω(a31x+a32y+a33z)+Tz
linealizacion Taylor
Xp=(Xp)o+r11dΩ+r12dÖ+r13dè+r14dK          r15dTx+r16dTy+r17dTz
Yp=(Yp)o+r21dΩ+r22dÖ+r23dè+r24dK          r25dTx+r26dTy+r27dTz
Zp=(Zp)o+r31dΩ+r32dÖ+r33dè+r34dK          r35dTx+r36dTy+r37dTz
coeficientes
r11=xa11+ya12+za13
r12=0
r13=Ω[-x(senècosK)+y(senèsenK)+zcosè]
r14=Ω[xa12-ya11]
r15=1
r16=0
r17=0
r21=xa21+ya22+za23
r22=-Ω(xa31+ya32+za33)
r23=Ω[x(senÖcosècosK)+y(-senÖcosèsenK)+z(senÖcosè)]
r24=Ω[xa22-ya21]
r25=0
r26=1
r27=0
r31=xa31+ya32+za33
r32=Ω(xa21+ya22+za23)
r33=Ω[x(-cosÖcosècosK)+y(cosÖcosèsenK)+z(-cosÖsenè)]
r34=Ω[xa32-ya31]
r35=0
r36=0
r37=1
+ 3                      [tema3]       [ RESTITUCION ANALITICA ]
- 31: ORIENTACION INTERNA
xf=axk-byk+Tx    (tr. 2D afin)
yf=cxk+dyk+Ty
 xt=xf+xw
 yt=yf+yw
x'=xt+Ór(xt/Ç(xtí+ytí))
y'=yt+Ór(yt/Ç(xtí+ytí))
correcciones  Ór=Co+Cr+Ce
 Co=a1r+a2rì+a3rï+a5ró
 Cr=refraccion
 Ce=esfericidad
- 32: ORIENTACION EXTERNA          un paso: coolineacion           dos pasos: cooplan. > 3D
. 321: COOLINEACION
Fx=x'q+cr=x'(m31(X-Xo)+m32(Y-Yo)+m33(Z-Zo))+c(m11(X-Xo)+m12(Y-Yo)+m13(Z-Zo))=0
Fy=y'q+cs=y'(m31(X-Xo)+m32(Y-Yo)+m33(Z-Zo))+c(m21(X-Xo)+m22(Y-Yo)+m23(Z-Zo))=0
linealizacion
Fx=(xo-xc)+dx'+A11dX+A12dY+A13dZ-A11dXo-A12dYo-A13dZo+A14dw+A15dŸ+A16dx
Fy=(yo-yc)+dy'+A21dX+A22dY+A23dZ-A21dXo-A22dYo-A23dZo+A24dw+A15dŸ+A26dx
coeficientes
A11=(x'm31+cm11)1/q
A12=(x'm32+cm12)1/q
A13=(x'm33+cm13)1/q
A14=x'(-(Y-Yo)m33+(Z-Zo)m32)+c(-(Y-Yo)m13+(Z-Zo)m12)1/q
A15=x'[(ÓXcosŸ+ÓY(senwsenŸ)+ÓZ(-coswsenŸ)]+c[ÓX(-senŸcosk)+ÓY(senwcosŸcosk)+ÓZ(-coswcosŸcosk)][1/q]
A16=c(ÓXm21+ÓYm22+ÓZm23)1/q
A21=(y'm31+cm21)1/q
A22=(y'm32+cm22)1/q
A23=(y'm33+cm23)1/q
A24=[y'(-ÓYm33+ÓZm32)+c(-ÓYm23+ÓZm22)]1/q
A25=y'[(ÓXcosŸ+ÓY(senwsenŸ)+ÓZ(-coswsenŸ)]+c[ÓX(senŸsenk)+ÓY(-senwcosŸsenk)+ÓZ(coswcosŸcsenk)][1/q]
A26=c(ÓXm11+ÓYm12+ÓZm13)1/q
mm.cc
[V]=[A][X]-[L]
[A]=-[Nû][C]
[X]=[dP]
[L]=[Nû][(F)o]
. 322: COPLANEIDAD
; 3221: caso general
Ó=(Y'Z"-Z'Y")+                      ây(Z'X"-X'Z")+âz(X'Y"-Y'X")
Ó=[x' y' c][R]¢[B][R][x" y" c]¢
donde [B]:   0   bz (-by)
           (-bz)  0   bx
             by (-bx)  0
coeficientes
B11=X"Z'-X'X"
B12=X'Y"-X"Y'
B13=-(Z"Z'+Y'Y")+ây(X"Y')+âz(X"Z')
B14=(Z"A11-Y"A12)+ây(X"A12-Z"A13)+âz(Y"A13-X"A11)
  A11=x'(senw1cosŸ1cosx1)+y'(-senw1cosŸ1senx1)+c(senw1senŸ1)
  A12=x'(-cosw1cosŸ1cosx1)+y'(cosw1cosŸ1senx1)+c(-cosw1senŸ1)
  A13=x'(-senŸ1cosx1)+y'(senŸ1senx1)+c(cosŸ1)
B15=Z"A14-Y"A15)+ây(X"A15-Z"A16)+âz(Y"A16-X"A14)
  A14=ai22x'-ai21y'
  A15=ai32x'-ai31y'
  A16=ai12x'-ai11y'
B16=(Z"Z'+Y'Y")+ây(-Y"X')+âz(-Z"X')
B17=-Z'A21+Y'A22)+ây(Z'A23-X'A22)+âz(-Y'A23+X'A21)
  A21=x"(senw2cosŸ2cosx2)+y"(-senw2cosŸ2senx2)+c(senw2senŸ2)
  A22=x"(-cosw2cosŸ2cosx2)+y"(cosw2cosŸ2senx2)+c(-cosw2senŸ2)
  A23=x"(-senŸ2cosx2)+y"(senŸ2senx2)+c(cosŸ2)
B18=Y'A25-Z'A24)+ây(Z'A26-X'A25)+âz(X'A24-Y'A26)
  A24=ad22x"-ad21y"
  A25=ad32x"-ad31y"
  A26=ad12x"-ad11y"
; 3222: empalme de fotos consec
w1=Ÿ1=x1=0
Ó=(y'Z"-z'Y")+ây(z'X"-x'Z")+âz(x'Y"-y'X")
Ó=[x' y' c][B][R][x" y" c]¢
donde [B]:    0   bz (-by)
            (-bz)  0   bx
              by (-bx)  0
coeficientes
B11=X"Z'-X'Z"
B12=X'Y"-X"Y'
B16=(Z"Z'+Y'Y")+ây(-Y"X')+âz(-Z"X')
B17=-Z'A21+Y'A22)+ây(Z'A23-X'A22)+âz(-Y'A23+X'A21)
  A21=x"(senw2cosŸ2cosx2)+y"(-senw2cosŸ2senx2)+c(senw2senŸ2)
  A22=x"(-cosw2cosŸ2cosx2)+y"(cosw2cosŸ2senx2)+c(-cosw2senŸ2)
  A23=x"(-senw2cosx2)+y'(senŸ2senx2)+c(cosŸ2)
B18=Y'A25-Z'A24)+ây(Z'A26-X'A25)+âz(X'A24-Y'A26)
  A24=ad22x"-ad21y"
  A25=ad32x"-ad31y"
  A26=ad12x"-ad11y"
; 3223: pares de fotos independ
â1=â2=w1=0
Ó=(y'z"-z'y")
Ó=[x' y' c][R]¢[B][R][x" y" c]¢
donde         0   0   0
              0   0  (1)
              0 (-1)  0
coeficientes
B11=Y"A11
  A11=x'(-cosŸ1cosx1)+y'(cosŸ1senx1)+c(-senŸ1)
B12=Z"A12-Y"A13
  A12=x'(cosw1cosx1)+y'(-cosw1senx1)
  A13=x'(cosw1senŸ1senx1)+y'(cosw1senŸ1cosx1)
B13=(Z"Z'+Y'Y")
B14=(-Z'A21+Y'A22)
  A21=x"(senw2cosŸ2cosx2)+y"(-senw2cosŸ2senx2)+c(senw2senŸ2)
  A22=x"(-cosw2cosŸ2cosx2)+y"(cosw2cosŸ2senx2)+c(-cosw2senŸ2)
B15=(Y'A23-Z'A24)
  A23=ad22"-ad21y"
  A24=ad32x"-ad31y"
. 323: CALCULO DE COORD. MODELO
ÖèΩ
,
;
CEREZO 924 223609                          259361 145
