MODULE pyLPT_Read_Module
    USE LPT_Read_Module

    IMPLICIT NONE

    !f2py CHARACTER(LEN=500)  :: InputC
    !f2py INTEGER,ALLOCATABLE :: InputI(:)
    !f2py REAL(8),ALLOCATABLE :: InputR(:)

    !f2py REAL(8),ALLOCATABLE,TARGET :: SnapUCurr(:,:,:)
    !f2py REAL(8),ALLOCATABLE,TARGET :: SnapUWind(:,:,:)
    !f2py REAL(8),ALLOCATABLE,TARGET :: SnapVCurr(:,:,:)
    !f2py REAL(8),ALLOCATABLE,TARGET :: SnapVWind(:,:,:)
    !f2py REAL(8),ALLOCATABLE,TARGET :: SnapWCurr(:,:,:)
    !f2py REAL(8)                    :: Times(2)

CONTAINS



    SUBROUTINE pyLPT_Read

        IMPLICIT NONE

        call LPT_Read
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_Parameters

        IMPLICIT NONE

        call LPT_Read_Parameters
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_Initial_Conditions

        IMPLICIT NONE

        call LPT_Read_Initial_Conditions
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_From_Particle_File(Snap)

        IMPLICIT NONE

        INTEGER,INTENT(IN)  :: Snap

        call LPT_Read_From_Particle_File(Snap)
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_Unstructured_Mesh

        IMPLICIT NONE

        call LPT_Read_Unstructured_Mesh
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_HYCOM_Mesh

        IMPLICIT NONE

        call LPT_Read_HYCOM_Mesh
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_Update_Velocities(Snap)

        IMPLICIT NONE

        INTEGER,INTENT(IN)       :: Snap

        call LPT_Read_Update_Velocities(Snap)
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_Velocity_Field(Field,TimeSnap)

        IMPLICIT NONE

        CHARACTER(LEN=1),INTENT(IN)  :: Field
        REAL(8),INTENT(OUT)          :: TimeSnap
        call LPT_Read_Velocity_Field(Field,TimeSnap)
    END SUBROUTINE



    SUBROUTINE pyLPT_Read_From_File(UnitNumber,VariableTypes,          &
            &               ErrorMessage)

        IMPLICIT NONE

        CHARACTER(*),INTENT(IN) :: ErrorMessage
        CHARACTER(*),INTENT(IN) :: VariableTypes(:)
        INTEGER,INTENT(IN)      :: UnitNumber
        !f2py CHARACTER(*),INTENT(IN) :: ErrorMessage
        !f2py CHARACTER(*),INTENT(IN) :: VariableTypes(:)

        call LPT_Read_From_File(UnitNumber,VariableTypes,          &
            &               ErrorMessage)
        END SUBROUTINE



    SUBROUTINE pyLPT_Read_Capitalize_Word(Word)

        IMPLICIT NONE

        CHARACTER(100),INTENT(INOUT) :: Word
        !f2py CHARACTER(100),INTENT(IN,OUT) :: Word

        call LPT_Read_Capitalize_Word(Word)
    END SUBROUTINE



END MODULE


