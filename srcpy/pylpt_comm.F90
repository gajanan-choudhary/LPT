MODULE pyLPT_Comm_Module
    USE LPT_Comm_Module
    IMPLICIT NONE

    !f2py INTEGER             :: COMM_LPT_ALL
    !f2py INTEGER             :: MyRank
    !f2py INTEGER             :: NumRanks
    !f2py INTEGER             :: NumTrackerCores
    !f2py INTEGER,ALLOCATABLE :: TrackerRanks(:)

    !f2py LOGICAL             :: AmTrackerCore
    !f2py LOGICAL             :: UseReaderCore
    !f2py LOGICAL             :: UseWriterCore

#ifdef MPI
    !f2py CHARACTER(LEN=1)     :: DummyC = "0"
    !f2py INTEGER,DIMENSION(1) :: DummyI = (/0/)
    !f2py REAL(8),DIMENSION(1) :: DummyR = (/0.D0/)
    !f2py CHARACTER(LEN=100)   :: MessageC
    !f2py INTEGER,ALLOCATABLE  :: MessageI(:)
    !f2py REAL(8),ALLOCATABLE  :: MessageR(:)
#endif

CONTAINS


    SUBROUTINE pyLPT_Comm_Init
        IMPLICIT NONE
        CALL LPT_Comm_Init
    END SUBROUTINE

    SUBROUTINE pyLPT_Comm_Final
        IMPLICIT NONE
        CALL LPT_Comm_Final
    END SUBROUTINE





#ifdef MPI



    SUBROUTINE pyLPT_Comm_To_Writer(VariableChar,VariableInteger,      &
            &               VariableReal,VariableType,ErrorMessage)

        IMPLICIT NONE

        CHARACTER(*),INTENT(IN)    :: ErrorMessage
        CHARACTER(*),INTENT(IN)    :: VariableType
        CHARACTER(*),INTENT(INOUT) :: VariableChar
        INTEGER,INTENT(INOUT)      :: VariableInteger(:)
        REAL(8),INTENT(INOUT)      :: VariableReal(:)
        !f2py CHARACTER(*),intent(in, out) :: VariableChar
        !f2py INTEGER,intent(in,out)       :: VariableInteger(:)
        !f2py REAL(8),intent(in,out)       :: VariableReal(:)

        CALL LPT_Comm_To_Writer(VariableChar,VariableInteger,      &
            &       VariableReal,VariableType,ErrorMessage)
    END SUBROUTINE


    SUBROUTINE pyLPT_Comm_From_Writer(VariableCharacter,               &
            &             VariableInteger,VariableReal,VariableType,        &
            &             ErrorMessage)

        IMPLICIT NONE

        CHARACTER(*),INTENT(IN)    :: ErrorMessage
        CHARACTER(*),INTENT(IN)    :: VariableType
        CHARACTER(*),INTENT(INOUT) :: VariableCharacter
        INTEGER,INTENT(INOUT)      :: VariableInteger(:)
        REAL(8),INTENT(INOUT)      :: VariableReal(:)
        !f2py CHARACTER(*),intent(in,out) :: VariableCharacter
        !f2py INTEGER,intent(in,out)      :: VariableInteger(:)
        !f2py REAL(8),intent(in,out)      :: VariableReal(:)

        CALL LPT_Comm_From_Writer(VariableCharacter,               &
            &        VariableInteger,VariableReal,VariableType,        &
            &        ErrorMessage)
    END SUBROUTINE


    SUBROUTINE pyLPT_Comm_Distribute(VariableChar,VariableInteger,     &
            &             VariableReal,VariableType,ErrorMessage)

        IMPLICIT NONE

        CHARACTER(*),INTENT(IN)    :: ErrorMessage
        CHARACTER(*),INTENT(IN)    :: VariableType
        CHARACTER(*),INTENT(INOUT) :: VariableChar
        INTEGER,INTENT(INOUT)      :: VariableInteger(:)
        REAL(8),INTENT(INOUT)      :: VariableReal(:)
        !f2py CHARACTER(*),intent(in,out) :: VariableChar
        !f2py INTEGER,intent(in,out)      :: VariableInteger(:)
        !f2py REAL(8),intent(in,out)      :: VariableReal(:)
        CALL LPT_Comm_Distribute(VariableChar,VariableInteger,     &
            &       VariableReal,VariableType,ErrorMessage)
    END SUBROUTINE
    
    
    SUBROUTINE pyLPT_Comm_Collect(VariableChar,VariableInteger,        &
            &             VariableReal,VariableType,ArraySize,ErrorMessage)

        IMPLICIT NONE
        CHARACTER(*),INTENT(IN)    :: ErrorMessage
        CHARACTER(*),INTENT(IN)    :: VariableType
        INTEGER,INTENT(IN)         :: ArraySize
        CHARACTER(*),INTENT(INOUT) :: VariableChar
        INTEGER,INTENT(INOUT)      :: VariableInteger(:)
        REAL(8),INTENT(INOUT)      :: VariableReal(:)
        !f2py CHARACTER(*),intent(in,out) :: VariableChar
        !f2py INTEGER,intent(in,out)      :: VariableInteger(:)
        !f2py REAL(8),intent(in,out)      :: VariableReal(:)

        CALL LPT_Comm_Collect(VariableChar,VariableInteger,        &
            &       VariableReal,VariableType,ArraySize,ErrorMessage)
    END SUBROUTINE

!ending: #ifdef MPI
#endif

END MODULE
