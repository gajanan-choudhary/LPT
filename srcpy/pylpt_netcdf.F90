#ifdef NETCDF
MODULE pyLPT_NetCDF_Module
    USE LPT_NetCDF_Module

    IMPLICIT NONE

#if NETCDF == 4
    !f2py INTEGER :: FillValueI !!!= 0
#endif
    !f2py INTEGER :: NC_Counter
    !f2py INTEGER :: NC_DIM_Part
    !f2py INTEGER :: NC_DIM_Time
    !f2py INTEGER :: NC_ID
    !f2py INTEGER :: NC_ID_Curr
    !f2py INTEGER :: NC_ID_Output
    !f2py INTEGER :: NC_ID_Particle
    !f2py INTEGER :: NC_ID_Wind
    !f2py INTEGER :: NC_Snap
    !f2py INTEGER :: NC_SnapCurr
    !f2py INTEGER :: NC_SnapWind
    !f2py INTEGER :: NC_Status
    !f2py INTEGER :: NC_VAR
#if NETCDF == 4
    !f2py INTEGER :: NC_VAR_NumParticles
#endif
    !f2py INTEGER :: NC_VAR_Time
    !f2py INTEGER :: NC_VAR_D
    !f2py INTEGER :: NC_VAR_X
    !f2py INTEGER :: NC_VAR_Y
    !f2py INTEGER :: NC_VAR_Z

    !f2py REAL(8)             :: FillValueR !!!= -99999.D0
    !f2py REAL(8),ALLOCATABLE :: NC_Temp(:)

CONTAINS

    SUBROUTINE pyLPT_NetCDF_Check(NC_Status)

        IMPLICIT NONE

        INTEGER,INTENT(IN) :: NC_Status

        CALL LPT_NetCDF_Check(NC_Status)

    END SUBROUTINE



END MODULE
#endif
