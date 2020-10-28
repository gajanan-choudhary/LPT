module pyLPT_oil
    IMPLICIT NONE

CONTAINS

    SUBROUTINE pyLPT_Oil_Buoyancy(DiameterEffective,VelocityTerminal)

        IMPLICIT NONE

        REAL(8),INTENT(IN)  :: DiameterEffective
        REAL(8),INTENT(OUT) :: VelocityTerminal

        CALL LPT_Oil_Buoyancy(DiameterEffective,VelocityTerminal)
    END SUBROUTINE

    SUBROUTINE pyLPT_Oil_Source(XNEW,YNEW,ZNEW,DNEW,NNEW,SourceType)

        IMPLICIT NONE

        INTEGER,INTENT(IN)  :: NNEW
        INTEGER,INTENT(IN)  :: SourceType

        REAL(8),INTENT(IN)  :: DNEW
        REAL(8),INTENT(IN)  :: XNEW
        REAL(8),INTENT(IN)  :: YNEW
        REAL(8),INTENT(IN)  :: ZNEW

        CALL LPT_Oil_Source(XNEW,YNEW,ZNEW,DNEW,NNEW,SourceType)
    END SUBROUTINE

end module
