! The routines in this file have been adapted from drog2dsp_hybrid.F90.
module pyLPT_drog

    IMPLICIT NONE

CONTAINS

    SUBROUTINE pyLPT_Drog_TimeStep
        call LPT_Drog_TimeStep
    END SUBROUTINE

    SUBROUTINE pyLPT_Drog_MAK_NEINFO
        call LPT_Drog_MAK_NEINFO
    END SUBROUTINE pyLPT_Drog_MAK_NEINFO

    SUBROUTINE pyLPT_Drog_Initialize
        call LPT_Drog_Initialize
    END SUBROUTINE

    SUBROUTINE pyLPT_Drog_Find_Element(XSTART,YSTART,ZSTART,           &
            &                          JJ,LL,ICHECK)

        IMPLICIT NONE

        INTEGER,INTENT(OUT)   :: ICHECK
        INTEGER,INTENT(INOUT) :: JJ
        INTEGER,INTENT(INOUT) :: LL
        !f2py INTEGER,INTENT(IN,OUT) :: JJ
        !f2py INTEGER,INTENT(IN,OUT) :: LL

        REAL(8),INTENT(IN) :: XSTART
        REAL(8),INTENT(IN) :: YSTART
        REAL(8),INTENT(IN) :: ZSTART

        CALL LPT_Drog_Find_Element(XSTART,YSTART,ZSTART,           &
            &                      JJ,LL,ICHECK)
    END SUBROUTINE

    SUBROUTINE pyLPT_Drog_MAKE_STAB(NMEL,ELEMS,NMND,X,Y)

        IMPLICIT NONE

        INTEGER,INTENT(IN) :: NMEL
        INTEGER,INTENT(IN) :: NMND
        INTEGER,INTENT(IN) :: ELEMS(NMEL,3)

        REAL(8),INTENT(IN) :: X(NMND)
        REAL(8),INTENT(IN) :: Y(NMND)

        call LPT_Drog_MAKE_STAB(NMEL,ELEMS,NMND,X,Y)
    END SUBROUTINE pyLPT_Drog_MAKE_STAB

    SUBROUTINE pyLPT_Drog_BELEL(J,XP,YP,NFLAG)

        IMPLICIT NONE

        INTEGER,INTENT(IN)  :: J
        INTEGER,INTENT(OUT) :: NFLAG
        REAL(8),INTENT(IN)  :: XP
        REAL(8),INTENT(IN)  :: YP

        call LPT_Drog_BELEL(J,XP,YP,NFLAG)
    END SUBROUTINE pyLPT_Drog_BELEL

    SUBROUTINE pyLPT_Drog_Find_Vertical_Element(Layer,J,X,Y,Z)
        IMPLICIT NONE

        ! Argument variables.

        INTEGER,INTENT(IN)    :: J
        INTEGER,INTENT(OUT)   :: Layer
        REAL(8),INTENT(IN)    :: X
        REAL(8),INTENT(IN)    :: Y
        REAL(8),INTENT(INOUT) :: Z
        !f2py REAL(8),INTENT(IN,OUT) :: Z

        CALL LPT_Drog_Find_Vertical_Element(Layer,J,X,Y,Z)
    END SUBROUTINE pyLPT_Drog_Find_Vertical_Element

    SUBROUTINE pyLPT_Drog_VELS(J,L,X,Y,Z,UF,VF,WF,DIAM)

        IMPLICIT NONE

        INTEGER,INTENT(IN)     :: J
        INTEGER,INTENT(IN)     :: L

        REAL(8), INTENT(IN)    :: DIAM
        REAL(8), INTENT(INOUT) :: UF
        REAL(8), INTENT(INOUT) :: VF
        REAL(8), INTENT(INOUT) :: WF
        REAL(8), INTENT(IN)    :: X
        REAL(8), INTENT(IN)    :: Y
        REAL(8), INTENT(IN)    :: Z
        !f2py REAL(8), INTENT(IN,OUT) :: UF
        !f2py REAL(8), INTENT(IN,OUT) :: VF
        !f2py REAL(8), INTENT(IN,OUT) :: WF

        call LPT_Drog_VELS(J,L,X,Y,Z,UF,VF,WF,DIAM)
    END SUBROUTINE pyLPT_Drog_VELS

    !function pyLPT_Drog_Interpolate(J,X,Y,Field) result(ans)

    !    USE LPT_Data_Module, ONLY: NumVerts

    !    IMPLICIT NONE

    !    INTEGER,INTENT(IN) :: J
    !    REAL(8),INTENT(IN) :: X
    !    REAL(8),INTENT(IN) :: Y
    !    REAL(8),INTENT(IN) :: Field(NumVerts)
    !    REAL(8) :: ans
    !    REAL(8), EXTERNAL :: LPT_Drog_Interpolate

    !    !f2py integer, intent(aux) :: NumVerts
    !    !f2py REAL(8), intent(in)  :: Field(numverts)
    !    !REAL(8), intent(out) :: ans

    !    ans = LPT_Drog_Interpolate(J,X,Y,Field)

    !END FUNCTION pyLPT_Drog_Interpolate

    SUBROUTINE pyLPT_Drog_TRACK(JEL,J,LEL,L,XSTART,YSTART,ZSTART,      &
            &                   USTART,VSTART,WSTART,T1,T2,DT1,IPN,DIAM)
        IMPLICIT NONE

        ! Argument variables.

        INTEGER           :: IPN
        INTEGER           :: J
        INTEGER           :: JEL
        INTEGER           :: L
        INTEGER           :: LEL

        REAL(8)           :: DIAM
        REAL(8)           :: DT1
        REAL(8)           :: T1
        REAL(8)           :: T2
        REAL(8)           :: USTART
        REAL(8)           :: VSTART
        REAL(8)           :: WSTART
        REAL(8)           :: XSTART
        REAL(8)           :: YSTART
        REAL(8)           :: ZSTART

        CALL LPT_Drog_TRACK(JEL,J,LEL,L,XSTART,YSTART,ZSTART,      &
            &               USTART,VSTART,WSTART,T1,T2,DT1,IPN,DIAM)
    END SUBROUTINE pyLPT_Drog_TRACK

    SUBROUTINE pyLPT_Drog_RKQC(JINOUT,LINOUT,XX,YY,ZZ,U,V,W,           &
            &                  T,DTTRY,DTNEXT,KODE,ICURBS,DIAM)

        IMPLICIT NONE

        ! Argument variables.

        INTEGER           :: ICURBS
        INTEGER           :: JINOUT
        INTEGER           :: KODE
        INTEGER           :: LINOUT

        REAL(8)           :: DIAM
        REAL(8)           :: DTNEXT
        REAL(8)           :: DTTRY
        REAL(8)           :: T
        REAL(8)           :: U
        REAL(8)           :: V
        REAL(8)           :: W
        REAL(8)           :: XX
        REAL(8)           :: YY
        REAL(8)           :: ZZ

        CALL LPT_Drog_RKQC(JINOUT,LINOUT,XX,YY,ZZ,U,V,W,           &
            &              T,DTTRY,DTNEXT,KODE,ICURBS,DIAM)
    END SUBROUTINE pyLPT_Drog_RKQC

    SUBROUTINE pyLPT_Drog_RK4(J,JOUT,L,LOUT,XX,YY,ZZ,U,V,W,            &
            &                 T,DT,XOUT,YOUT,ZOUT,KODE,DIAM)

        IMPLICIT NONE

        ! Argument variables.

        INTEGER :: J
        INTEGER :: JOUT
        INTEGER :: KODE
        INTEGER :: L
        INTEGER :: LOUT

        REAL(8) :: DIAM
        REAL(8) :: DT
        REAL(8) :: T
        REAL(8) :: U
        REAL(8) :: V
        REAL(8) :: W
        REAL(8) :: XOUT
        REAL(8) :: XX
        REAL(8) :: YOUT
        REAL(8) :: YY
        REAL(8) :: ZOUT
        REAL(8) :: ZZ

        CALL LPT_Drog_RK4(J,JOUT,L,LOUT,XX,YY,ZZ,U,V,W,            &
            &             T,DT,XOUT,YOUT,ZOUT,KODE,DIAM)
    END SUBROUTINE pyLPT_Drog_RK4

    SUBROUTINE pyLPT_Drog_FNDELE(J,JJ,XX,YY,INDD)

        IMPLICIT NONE

        ! Argument variables.

        INTEGER,INTENT(OUT)   :: INDD
        INTEGER,INTENT(IN)    :: J
        INTEGER,INTENT(INOUT) :: JJ
        !f2py INTEGER,INTENT(IN,OUT) :: JJ

        REAL(8),INTENT(IN)    :: XX
        REAL(8),INTENT(IN)    :: YY

        CALL LPT_Drog_FNDELE(J,JJ,XX,YY,INDD)
    END SUBROUTINE pyLPT_Drog_FNDELE

    SUBROUTINE pyLPT_Drog_FNDELEls(INDD,JJ,XT,YT)

        USE LPT_Data_LATTICE_TABLE

        IMPLICIT NONE

        ! Argument variables.

        INTEGER,INTENT(INOUT) :: INDD
        INTEGER,INTENT(INOUT) :: JJ
        !f2py INTEGER,INTENT(IN,OUT) :: INDD
        !f2py INTEGER,INTENT(IN,OUT) :: JJ

        REAL(8),INTENT(IN)    :: XT
        REAL(8),INTENT(IN)    :: YT

        CALL LPT_Drog_FNDELEls(INDD,JJ,XT,YT)
    END SUBROUTINE pyLPT_Drog_FNDELEls

    SUBROUTINE pyLPT_Drog_BOUNTRK(JINOUT,LINOUT,XX,YY,ZZ,U,V,W,        &
            &                     T,T2,KODE,ICURBS,DIAM)

        IMPLICIT NONE

        ! Argument variables.

        INTEGER :: ICURBS
        INTEGER :: JINOUT
        INTEGER :: KODE
        INTEGER :: LINOUT

        REAL(8) :: DIAM
        REAL(8) :: T
        REAL(8) :: T2
        REAL(8) :: U
        REAL(8) :: V
        REAL(8) :: W
        REAL(8) :: XX
        REAL(8) :: YY
        REAL(8) :: ZZ

        CALL LPT_Drog_BOUNTRK(JINOUT,LINOUT,XX,YY,ZZ,U,V,W,        &
            &                 T,T2,KODE,ICURBS,DIAM)
    END SUBROUTINE pyLPT_Drog_BOUNTRK

end module pyLPT_drog
