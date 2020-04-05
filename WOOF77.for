      PROGRAM WOOF77

C REFERENCE:
C
C Lampton, Michael L. (1972). Program WOOF: A Numerical
C Evaluator of Loudspeaker Systems. IEEE Transactions on
C Audio and Electroacoustics, Vol. AU-20, No. 5, December
C 1972, pages 364-366.
C
C ABSTRACT FROM ABOVE JOURNAL PAPER:
C
C A Fortran language computer program is described that evaluates
C the absolute low-frequency response and electrical impedance
C functions of frequency for direct radiator electrodynamic
C loudspeakers mounted in vented, unvented, or infinite baffle
C enclosures. The program is intended to be used interactively,
C with the engineer serving to suggest modifications of a design
C and the program providing the modified response plots. The
C program listing, typical input data, and an example of the
C program’s output are shown.
C
C NOTES:
C
C The program WOOF77 is a modified version of program WOOF. The
C main changes relate to WOOF's conversion to use Fortran 77.
C The use of Holleriths has been eliminated and hopefully improves
C clarity for the user. The plotting section has also been modified
C to provide greater frequency resolution (1 Hz rather than 2 Hz).
C The impedance plot now covers a range from 0 to 120 ohms, rather
C than 0 to 150 ohms. The program now outputs the absolute sound
C pressure level output in dB, based on the computed driver
C efficiency. After the frequency reponse plots, the SPL and
C impedance responses are listed for the user to study if desired.

      IMPLICIT NONE

C DECLARATION OF VARIABLES.

      CHARACTER MATRIX(99,29),MK(3)
      REAL A1,V,FAR,S1,BL,R,Q0
      REAL A,B,C,D,W,FR,R2,PI
      REAL M,MWOOF,M2,M2OPT,ASYMP,FH,REM,S2,S,Q,R1,X1,RRAD
      REAL RESP(99),ZEL(99)
      REAL SPLDB,IMPOHM
      INTEGER I,IFREQ,LOSS,ILEVEL,ISPL,IZ,IY
      INTEGER JY(4),JZ(4),J(11)
      COMPLEX F,Z1

      DATA MK(1),MK(2),MK(3)/' ','x','o'/
      DATA JY/100,90,80,70/
      DATA JZ/120,80,40,0/
      DATA J/0,10,20,30,40,50,60,70,80,90,100/

C COMMENCE GETTING THE INPUT DATA FROM THE USER AT
C THE COMMAND LINE. INPUT DATA IS IN FREE FORMAT.

      READ *,A1,V,FAR,S1,BL,R,Q0,M2

C ECHO THE INPUT DATA.

      PRINT 99001
      PRINT 99002,A1,V,FAR,S1,BL,R,Q0,M2

C CONCLUDES INPUT AND OUTPUT ECHO CHECK.
C COMMENCE EVALUATING CONSTANT PARAMETERS.

      PI = 4.0*ATAN(1.0)
      MWOOF = S1/(2.0*PI*FAR)**2
      S2 = 143000.0*A1**2/V
      M2OPT = MWOOF*S2/S1
      REM = BL**2/R
      FH = 0.159*SQRT(S2/M2)
      ASYMP = 0.0552*REM*(A1/MWOOF)**2
      X1 = SQRT(S1*MWOOF)
      R1 = X1/Q0
      Q = X1/(R1+REM)
      S = S2/S1
      M = M2/MWOOF
      A = Q**(-2.0)-2.0-2.0*S-2.0*S/M
      B = 1.0 + 2.0*S+S*S+4.0*S/M+2.0*S*S/M+S*S/M/M-2.0*S/Q/Q/M
      C = S*S/Q/Q/M/M-2.0*S/M-2.0*S*S/M-2.0*S*S/M/M
      D = S*S/M/M

      PRINT 99003,MWOOF,S2,REM,FH,M2OPT,ASYMP,Q,S,M,A,B,C,D

C COMMENCE EVALUATING RESPONSE AND IMPEDANCE.

      DO I = 1,99
        FR = REAL(I)
        W = 2.0*PI*FR
        RRAD = 0.022*(FR*A1)**2
        R2 = RRAD
        F = CMPLX(R2,W*M2)/CMPLX(R2,W*M2-S2/W)
        Z1 = CMPLX(R1+RRAD,W*MWOOF-S1/W) - CMPLX(0.0,1.0)*F*S2/W
        RESP(I) = RRAD*REM*CABS(F)**2/CABS(Z1+REM)**2
        ZEL(I) = R*CABS(1.0+REM/Z1)
      ENDDO

C CLEAR AND FILL THE DISPLAY MATRIX.

      DO IFREQ = 1,99
        DO ILEVEL = 1,29
          MATRIX(IFREQ,ILEVEL) = MK(1)
        ENDDO
        ISPL = NINT(112.0+10.0*ALOG10(RESP(IFREQ))) - JY(4)
        IF ( ISPL.GE.1 .AND. ISPL.LE.29 ) MATRIX(IFREQ,ISPL) = MK(2)
        IZ = NINT(0.25*ZEL(IFREQ))
        IF ( IZ.GE.1 .AND. IZ.LE.29 ) MATRIX(IFREQ,IZ) = MK(3)
      ENDDO

C COMPLETE MATRIX IS NOW STORED. NEXT, PRINT IT.

      PRINT 99004,MK(2),MK(3)
      PRINT 99008
      PRINT 99010,JY(1),JZ(1)
      DO LOSS = 1,29
        IY = 30 - LOSS
        IF ( LOSS.EQ.10 ) THEN
          PRINT 99011,JY(2),(MATRIX(IFREQ,IY),IFREQ=1,99),JZ(2)
        ELSEIF ( LOSS.NE.20 ) THEN
          PRINT 99005,(MATRIX(IFREQ,IY),IFREQ=1,99)
        ELSE
          PRINT 99011,JY(3),(MATRIX(IFREQ,IY),IFREQ=1,99),JZ(3)
        ENDIF
      ENDDO

      PRINT 99010,JY(4),JZ(4)
      PRINT 99009
      PRINT 99006,J
      PRINT 99007

      PRINT 99012,'FREQUENCY_HZ','RESPONSE_DB','IMPEDANCE_OHMS'
      DO IFREQ = 1,99
        FR = REAL(IFREQ)
        SPLDB = 112.0 + 10.0*ALOG10(RESP(IFREQ))
        IMPOHM = ZEL(IFREQ)
        PRINT 99013,FR,SPLDB,IMPOHM
      ENDDO

99001 FORMAT (/,
     & 12X,'--------------------------',/,
     & 12X,'WOOFER PERFORMANCE PLOTTER',/,
     & 12X,'--------------------------',/)
99002 FORMAT (' SYSTEM INPUT DATA ARE:'//
     & ' EFFECTIVE PISTON AREA      =',F8.4,' SQUARE METERS',/,
     & ' ADIABATIC ENCLOSURE VOLUME =',F8.4,' CUBIC METERS',/,
     & ' WOOFER FREE AIR RESONANCE  =',F8.1,' HERTZ',/,
     & ' SUSPENSION STIFFNESS       =',F8.1,' NEWTON/METER',/,
     & ' B L PRODUCT                =',F8.1,' WEBER/METER',/,
     & ' VOICE COIL RESISTANCE      =',F8.1,' OHMS',/,
     & ' SUSPENSION Q FACTOR        =',F8.2,/,
     & ' REFLEX VENT AIR MASS       =',F8.3,' KILOGRAMS',//)
99003 FORMAT (' FROM THESE DATA, CONSTANT PARAMETERS ARE:',//,
     & ' EFFECTIVE WOOFER MASS   =',F8.3,' KILOGRAMS',/,
     & ' ENCLOSURE AIR STIFFNESS =',F8.1,' NEWTON/METER',/,
     & ' ELECTRODYNAMIC DRAG     =',F8.2,' NEWTON SEC/METER',/,
     & ' HELMHOLTZ FREQUENCY     =',F8.1,' HERTZ',/,
     & ' SUGGESTED VENT AIR MASS =',F8.3,' KILOGRAMS',/,
     & ' ASYMPTOTIC EFFICIENCY   =',F8.3,' PERCENT',/,
     & '    Q=',F8.3,'    S=',F8.3,'    M=',F8.3,/,
     & '    A=',F8.3,'    B=',F8.3,'    C=',F8.3,'    D=',F8.3,//)
99004 FORMAT (4X,'ABSOLUTE RESPONSE IN DB SPL (',1A,') ',
     &        'AND IMPEDANCE IN OHMS (',1A,')'/)
99005 FORMAT (4X,':',99A1,':')
99006 FORMAT (11(I5,5X))
99007 FORMAT (50X,'FREQUENCY (HERTZ)'//)
99008 FORMAT (4X,'|',10('         |'))
99009 FORMAT (4X,'|',10('123456789|'))
99010 FORMAT (I4,'+',99('-'),'+',I3)
99011 FORMAT (I4,'+',99A1,'+',I3)
99012 FORMAT (3A16)
99013 FORMAT (3F16.2)

      END

C The following line contains a set of parameters that
C can be used as input to the WOOF77 program. Simply
C cut and paste into the command line after WOOF77 is
C executed.
C
C 0.032 0.12943 30.0 800.0 8.7982 8.00 3.00 0.031844
C
C Example of the output prepared by WOOF:
C
C            --------------------------
C            WOOFER PERFORMANCE PLOTTER
C            --------------------------
C
C SYSTEM INPUT DATA ARE:
C
C EFFECTIVE PISTON AREA      =  0.0320 SQUARE METERS
C ADIABATIC ENCLOSURE VOLUME =  0.1294 CUBIC METERS
C WOOFER FREE AIR RESONANCE  =    30.0 HERTZ
C SUSPENSION STIFFNESS       =   800.0 NEWTON/METER
C B L PRODUCT                =     8.8 WEBER/METER
C VOICE COIL RESISTANCE      =     8.0 OHMS
C SUSPENSION Q FACTOR        =    3.00
C REFLEX VENT AIR MASS       =   0.032 KILOGRAMS
C
C
C FROM THESE DATA, CONSTANT PARAMETERS ARE:
C
C EFFECTIVE WOOFER MASS   =   0.023 KILOGRAMS
C ENCLOSURE AIR STIFFNESS =  1131.4 NEWTON/METER
C ELECTRODYNAMIC DRAG     =    9.68 NEWTON SEC/METER
C HELMHOLTZ FREQUENCY     =    30.0 HERTZ
C SUGGESTED VENT AIR MASS =   0.032 KILOGRAMS
C ASYMPTOTIC EFFICIENCY   =   1.079 PERCENT
C    Q=   0.383    S=   1.414    M=   1.414
C    A=   0.001    B=  -0.001    C=   0.000    D=   1.000
C
C
C    ABSOLUTE RESPONSE IN DB SPL (x) AND IMPEDANCE IN OHMS (o)
C
C    |         |         |         |         |         |         |         |         |         |         |
C 100+---------------------------------------------------------------------------------------------------+120
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                                                                                   :
C    :                                    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
C    :                                xxxx                                                               :
C  90+                              xx                                                                   + 80
C    :                            xx                                                                     :
C    :                           x                                                                       :
C    :                          x                                                                        :
C    :                         x                                                                         :
C    :                o       x                          oo                                              :
C    :                       x                          o  o                                             :
C    :                      x                               o                                            :
C    :               o o                               o     o                                           :
C    :                     x                          o       o                                          :
C  80+                    x                                    o                                         + 40
C    :                                               o          o                                        :
C    :              o   ox                          o            oo                                      :
C    :                                             o               oo                                    :
C    :             o    xo                        o                  ooo                                 :
C    :            o    x  o                     oo                      ooooo                            :
C    :          oo         o                  oo                             ooooooooooo                 :
C    :       ooo      x     oo            oooo                                          ooooooooooooooooo:
C    :ooooooo                 oooooooooooo                                                               :
C    :                                                                                                   :
C  70+---------------------------------------------------------------------------------------------------+  0
C    |123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
C    0        10        20        30        40        50        60        70        80        90       100
C                                                  FREQUENCY (HERTZ)
C
C
C    FREQUENCY_HZ     RESPONSE_DB  IMPEDANCE_OHMS
C            1.00          -25.80            8.03
C            2.00           -1.72            8.12
C            3.00           12.37            8.28
C            4.00           22.37            8.51
C            5.00           30.12            8.82
C            6.00           36.45            9.22
C            7.00           41.81            9.75
C            8.00           46.45           10.44
C            9.00           50.54           11.32
C             :               :               :
C             :               :               :
