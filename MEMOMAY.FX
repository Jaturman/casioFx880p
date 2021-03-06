[  FOTOGRAMETRIA II  ]:     ITT  feb '98; Merida. J.Tm                                                                                                                                                                         >
Indice|+>->.>;                  [1]   |+(tema1)[Rest. Analogica][11]  |-Ecuacion Py Px
[2]   |+(tema2)[Transformacion] [21]  |-2D Afin                 [22]  |-Proyectiva              [23]  |-3D Conforme
[3]   |+(tema3)[Rest. Analitica][31]  |-Orientacion Interna
[32]  |-Orientacion externa     [321] |.Coolineacion            [322] |.Coplaneidad             [3221]|;metodo general          [3222]|;por pares consecutivos  [3223]|;por pares independientes[323] |.Calculo Coord. Modelo  >
+ 1                      [tema1]       [ RESTITUCION ANALOGICA ]
11 ec. paralaje
dPy=dby1-dby2+(Y/h)dbz1-((Y-b)/h)dbz2+h(1+Y�/h�)dw1-h(1+Y�/h�)dw2+(XY/h)d�1-((X-b)Y/h)d�2+(X)dx1-(X-b)dx2
dPx=dbx1-dbx2+(X/h)dbz1-((X-b)/h)dbz2+(YX/h)dw1-((Y-b)X/h)dw2-h(1+X�/h�)d�1+h(1+X�/h�)d�2-(Y)dx1+(Y-b)dx2
+ 2                      [tema2]              [ TRANSFORMACION ]
[R] Matriz de rotacion
a11=cos�cosK
a12=-cos�senK
a13=sen�
a21=cos�senK+sen�sen�cosK
a22=cos�cosK-sen�sen�senK
a23=-sen�cos�
a31=sen�senK-cos�sen�cosK
a32=sen�cosK+cos�sen�senK
a33=cos�cos�
[R] valores pequenos 
senx=x cosx=1 infinit.1er orden
(1) (-�k)  �
 �k  (1) (-�)
(-�)  �  (1)
- 21: (2D) Afin
X=�xx'cos�-                           �y'(cos�sen�+sen�cos�)+Tx
Y=�xx'sen�+                           �y'(cos�cos�-sen�sen�)+Ty
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
X=�(a11x+a12y+a13z)+Tx
Y=�(a21x+a22y+a23z)+Ty
Z=�(a31x+a32y+a33z)+Tz
linealizacion Taylor
Xp=(Xp)o+r11d�+r12d�+r13d�+r14dK          r15dTx+r16dTy+r17dTz
Yp=(Yp)o+r21d�+r22d�+r23d�+r24dK          r25dTx+r26dTy+r27dTz
Zp=(Zp)o+r31d�+r32d�+r33d�+r34dK          r35dTx+r36dTy+r37dTz
coeficientes
r11=xa11+ya12+za13
r12=0
r13=�[-x(sen�cosK)+y(sen�senK)+zcos�]
r14=�[xa12-ya11]
r15=1
r16=0
r17=0
r21=xa21+ya22+za23
r22=-�(xa31+ya32+za33)
r23=�[x(sen�cos�cosK)+y(-sen�cos�senK)+z(sen�cos�)]
r24=�[xa22-ya21]
r25=0
r26=1
r27=0
r31=xa31+ya32+za33
r32=�(xa21+ya22+za23)
r33=�[x(-cos�cos�cosK)+y(cos�cos�senK)+z(-cos�sen�)]
r34=�[xa32-ya31]
r35=0
r36=0
r37=1
+ 3                      [tema3]       [ RESTITUCION ANALITICA ]
- 31: ORIENTACION INTERNA
xf=axk-byk+Tx    (tr. 2D afin)
yf=cxk+dyk+Ty
 xt=xf+xw
 yt=yf+yw
x'=xt+�r(xt/�(xt�+yt�))
y'=yt+�r(yt/�(xt�+yt�))
correcciones  �r=Co+Cr+Ce
 Co=a1r+a2r�+a3r�+a5r�
 Cr=refraccion
 Ce=esfericidad
- 32: ORIENTACION EXTERNA          un paso: coolineacion           dos pasos: cooplan. > 3D
. 321: COOLINEACION
Fx=x'q+cr=x'(m31(X-Xo)+m32(Y-Yo)+m33(Z-Zo))+c(m11(X-Xo)+m12(Y-Yo)+m13(Z-Zo))=0
Fy=y'q+cs=y'(m31(X-Xo)+m32(Y-Yo)+m33(Z-Zo))+c(m21(X-Xo)+m22(Y-Yo)+m23(Z-Zo))=0
linealizacion
Fx=(xo-xc)+dx'+A11dX+A12dY+A13dZ-A11dXo-A12dYo-A13dZo+A14dw+A15d�+A16dx
Fy=(yo-yc)+dy'+A21dX+A22dY+A23dZ-A21dXo-A22dYo-A23dZo+A24dw+A15d�+A26dx
coeficientes
A11=(x'm31+cm11)1/q
A12=(x'm32+cm12)1/q
A13=(x'm33+cm13)1/q
A14=x'(-(Y-Yo)m33+(Z-Zo)m32)+c(-(Y-Yo)m13+(Z-Zo)m12)1/q
A15=x'[(�Xcos�+�Y(senwsen�)+�Z(-coswsen�)]+c[�X(-sen�cosk)+�Y(senwcos�cosk)+�Z(-coswcos�cosk)][1/q]
A16=c(�Xm21+�Ym22+�Zm23)1/q
A21=(y'm31+cm21)1/q
A22=(y'm32+cm22)1/q
A23=(y'm33+cm23)1/q
A24=[y'(-�Ym33+�Zm32)+c(-�Ym23+�Zm22)]1/q
A25=y'[(�Xcos�+�Y(senwsen�)+�Z(-coswsen�)]+c[�X(sen�senk)+�Y(-senwcos�senk)+�Z(coswcos�csenk)][1/q]
A26=c(�Xm11+�Ym12+�Zm13)1/q
mm.cc
[V]=[A][X]-[L]
[A]=-[N�][C]
[X]=[dP]
[L]=[N�][(F)o]
. 322: COPLANEIDAD
; 3221: caso general
�=(Y'Z"-Z'Y")+                      �y(Z'X"-X'Z")+�z(X'Y"-Y'X")
�=[x' y' c][R]�[B][R][x" y" c]�
donde [B]:   0   bz (-by)
           (-bz)  0   bx
             by (-bx)  0
coeficientes
B11=X"Z'-X'X"
B12=X'Y"-X"Y'
B13=-(Z"Z'+Y'Y")+�y(X"Y')+�z(X"Z')
B14=(Z"A11-Y"A12)+�y(X"A12-Z"A13)+�z(Y"A13-X"A11)
  A11=x'(senw1cos�1cosx1)+y'(-senw1cos�1senx1)+c(senw1sen�1)
  A12=x'(-cosw1cos�1cosx1)+y'(cosw1cos�1senx1)+c(-cosw1sen�1)
  A13=x'(-sen�1cosx1)+y'(sen�1senx1)+c(cos�1)
B15=Z"A14-Y"A15)+�y(X"A15-Z"A16)+�z(Y"A16-X"A14)
  A14=ai22x'-ai21y'
  A15=ai32x'-ai31y'
  A16=ai12x'-ai11y'
B16=(Z"Z'+Y'Y")+�y(-Y"X')+�z(-Z"X')
B17=-Z'A21+Y'A22)+�y(Z'A23-X'A22)+�z(-Y'A23+X'A21)
  A21=x"(senw2cos�2cosx2)+y"(-senw2cos�2senx2)+c(senw2sen�2)
  A22=x"(-cosw2cos�2cosx2)+y"(cosw2cos�2senx2)+c(-cosw2sen�2)
  A23=x"(-sen�2cosx2)+y"(sen�2senx2)+c(cos�2)
B18=Y'A25-Z'A24)+�y(Z'A26-X'A25)+�z(X'A24-Y'A26)
  A24=ad22x"-ad21y"
  A25=ad32x"-ad31y"
  A26=ad12x"-ad11y"
; 3222: empalme de fotos consec
w1=�1=x1=0
�=(y'Z"-z'Y")+�y(z'X"-x'Z")+�z(x'Y"-y'X")
�=[x' y' c][B][R][x" y" c]�
donde [B]:    0   bz (-by)
            (-bz)  0   bx
              by (-bx)  0
coeficientes
B11=X"Z'-X'Z"
B12=X'Y"-X"Y'
B16=(Z"Z'+Y'Y")+�y(-Y"X')+�z(-Z"X')
B17=-Z'A21+Y'A22)+�y(Z'A23-X'A22)+�z(-Y'A23+X'A21)
  A21=x"(senw2cos�2cosx2)+y"(-senw2cos�2senx2)+c(senw2sen�2)
  A22=x"(-cosw2cos�2cosx2)+y"(cosw2cos�2senx2)+c(-cosw2sen�2)
  A23=x"(-senw2cosx2)+y'(sen�2senx2)+c(cos�2)
B18=Y'A25-Z'A24)+�y(Z'A26-X'A25)+�z(X'A24-Y'A26)
  A24=ad22x"-ad21y"
  A25=ad32x"-ad31y"
  A26=ad12x"-ad11y"
; 3223: pares de fotos independ
�1=�2=w1=0
�=(y'z"-z'y")
�=[x' y' c][R]�[B][R][x" y" c]�
donde         0   0   0
              0   0  (1)
              0 (-1)  0
coeficientes
B11=Y"A11
  A11=x'(-cos�1cosx1)+y'(cos�1senx1)+c(-sen�1)
B12=Z"A12-Y"A13
  A12=x'(cosw1cosx1)+y'(-cosw1senx1)
  A13=x'(cosw1sen�1senx1)+y'(cosw1sen�1cosx1)
B13=(Z"Z'+Y'Y")
B14=(-Z'A21+Y'A22)
  A21=x"(senw2cos�2cosx2)+y"(-senw2cos�2senx2)+c(senw2sen�2)
  A22=x"(-cosw2cos�2cosx2)+y"(cosw2cos�2senx2)+c(-cosw2sen�2)
B15=(Y'A23-Z'A24)
  A23=ad22"-ad21y"
  A24=ad32x"-ad31y"
. 323: CALCULO DE COORD. MODELO
���
,
;
CEREZO 924 223609                          259361 145
