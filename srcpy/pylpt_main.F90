MODULE pyLPT_Main

    IMPLICIT NONE

CONTAINS

    subroutine lpt_main
        USE LPT_Comm_Module, ONLY: LPT_Comm_Init, LPT_Comm_Final
        USE LPT_Read_Module, ONLY: LPT_Read

        IMPLICIT NONE

        ! Initialize the parallel environment.
        CALL LPT_Comm_Init

        ! Read the input files.
        CALL LPT_Read

        ! Do some initial computations.
        CALL LPT_Drog_Initialize

        ! More like time-trippin', amirite?
        CALL LPT_Drog_TimeStep

        ! Finalize the parallel environment
        CALL LPT_Comm_Final

        ! The program ends in LPT_Comm_Final.
        ! No other instructions should be added here.
    end subroutine

END MODULE

