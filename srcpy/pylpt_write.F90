MODULE pyLPT_Write_Module
    USE LPT_Write_Module, ONLY : LPT_Write_Particle_Snap

    IMPLICIT NONE

CONTAINS

    SUBROUTINE pyLPT_Write_Particle_Snap
        CALL LPT_Write_Particle_Snap
    END SUBROUTINE pyLPT_Write_Particle_Snap

END MODULE

