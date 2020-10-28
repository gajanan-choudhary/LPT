MODULE pyLPT_Data_Module
    USE LPT_Data_Module

    IMPLICIT NONE

    !f2py REAL(8),PARAMETER   :: EarthRadius = 6.3675D6
    !f2py REAL(8),PARAMETER   :: Gravity = 9.805D0

    !f2py CHARACTER(LEN=100)  :: HeaderParticles

    !f2py INTEGER,ALLOCATABLE :: BoundarySegmentElement(:)
    !f2py INTEGER,ALLOCATABLE :: BoundarySegmentFollowing(:)
    !f2py INTEGER,ALLOCATABLE :: BoundarySegmentPreceding(:)
    !f2py INTEGER,ALLOCATABLE :: BoundarySegmentVerts(:,:)
    !f2py INTEGER,ALLOCATABLE :: MapOpenMP(:)
    !f2py INTEGER,ALLOCATABLE :: MeshConnEE(:,:)
    !f2py INTEGER,ALLOCATABLE :: MeshConnEV(:,:)
    !f2py INTEGER,ALLOCATABLE :: MeshConnVE(:,:)
    !f2py INTEGER             :: NumBoundarySegments
    !f2py INTEGER             :: NumElems
    !f2py INTEGER             :: NumLayers
    !f2py INTEGER             :: NumParticlesGlobal
    !f2py INTEGER             :: NumParticlesLocal
    !f2py INTEGER             :: NumVerts !!!= 0
    !f2py INTEGER             :: OutputTimeStep
    !f2py INTEGER,ALLOCATABLE :: PartBoundary(:)
    !f2py INTEGER,ALLOCATABLE :: PartBoundaryCode(:)
    !f2py INTEGER,ALLOCATABLE :: PartElemJ(:)
    !f2py INTEGER,ALLOCATABLE :: PartElemL(:)
    !f2py INTEGER,ALLOCATABLE :: PartDomainLocal(:)
    !f2py INTEGER,ALLOCATABLE :: PartNumberLocal(:)

    !f2py REAL(8)             :: Deg2Rad
    !f2py REAL(8),ALLOCATABLE :: ElementArea_AR(:)
    !f2py REAL(8),ALLOCATABLE :: ElementArea_T(:,:)
    !f2py REAL(8),ALLOCATABLE :: MeshDepth(:)
    !f2py REAL(8),ALLOCATABLE :: MeshLat(:)
    !f2py REAL(8),ALLOCATABLE :: MeshLon(:)
    !f2py REAL(8),ALLOCATABLE :: PartDepthLocal(:)
    !f2py REAL(8),ALLOCATABLE :: PartDiameterLocal(:)
    !f2py REAL(8),ALLOCATABLE :: PartLatLocal(:)
    !f2py REAL(8),ALLOCATABLE :: PartLonLocal(:)
    !f2py REAL(8)             :: Pi
    !f2py REAL(8)             :: RequiredAccuracy !!!= 1.D0
    !f2py REAL(8),ALLOCATABLE :: ShapeFunc_A(:,:)
    !f2py REAL(8),ALLOCATABLE :: ShapeFunc_A0(:,:)
    !f2py REAL(8),ALLOCATABLE :: ShapeFunc_B(:,:)
    !f2py REAL(8),ALLOCATABLE :: Sigma(:)
    !f2py REAL(8)             :: Time1
    !f2py REAL(8)             :: Time2
    !f2py REAL(8),ALLOCATABLE :: TimeStepSizes(:)
    !f2py REAL(8),ALLOCATABLE :: VelU(:,:)
    !f2py REAL(8),ALLOCATABLE :: VelV(:,:)
    !f2py REAL(8),ALLOCATABLE :: VelW(:,:)

    !f2py ! ESSENTIAL INPUT
    !f2py ! If these parameters are not specified in the input file,
    !f2py ! then the program will print a fatal error message and stop.
    !f2py ! Thus there is no need to set default values for these parameters,
    !f2py ! because their values must be specified by the user.

    !f2py INTEGER :: NumTrackingSnaps !!!= -99
    !f2py REAL(8) :: SimulationLength !!!= -99999999.D0
    !f2py REAL(8) :: StartingTime !!!= -99.D0
    !f2py REAL(8) :: MinimumTimeStep !!!= 0.01D0
    !f2py INTEGER :: LatticeSearchBins !!!= 1000

    !f2py CHARACTER(LEN=100) :: CurrentFile !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: CurrentFileOrigin !!!= "ADCIRC"
    !f2py CHARACTER(LEN=100) :: CurrentFileFormat !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: CurrentDimensions !!!= "NULL"

    !f2py CHARACTER(LEN=100) :: WindFile !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: WindFileFormat !!!= "NULL"

    !f2py CHARACTER(LEN=100) :: ParticleInputMethod !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: ParticleInputCoordinates !!!= "Polar"
    !f2py CHARACTER(LEN=100) :: ParticleFile !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: ParticleFileFormat !!!= "NULL"
    !f2py REAL(8) :: ParticleSourceX
    !f2py REAL(8) :: ParticleSourceY
    !f2py REAL(8) :: ParticleSourceZ

    !f2py CHARACTER(LEN=100) :: MeshFile !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: MeshFileOrigin !!!= "ADCIRC"
    !f2py CHARACTER(LEN=100) :: MeshCoordinates !!!= "Polar"


    !f2py CHARACTER(LEN=100) :: CombinationMethod !!!= "Original"
    !f2py REAL(8) :: PercentageCurrent !!!= 1.D0
    !f2py REAL(8) :: PercentageWind !!!= 0.D0
    !f2py REAL(8) :: AngleWind !!!= 0.07D0

    !f2py CHARACTER(LEN=100) :: DiffusionMethod !!!= "NULL"
    !f2py REAL(8) :: Cx  !!!= 12.D0
    !f2py REAL(8) :: Cy  !!!= 12.D0
    !f2py REAL(8) :: Evx !!!= 10.D0
    !f2py REAL(8) :: Evy !!!= 10.D0

    !f2py REAL(8) :: DensityWater !!!= 998.2071D0 ! km/m3 at 20C
    !f2py REAL(8) :: DynamicViscosityWater !!!= 0.001002D0 ! Pa-s at 20C

    !f2py REAL(8) :: DensityOil !!!= 858.D0 ! kg/m3 from Socolofsky et al. (2011)
    !f2py REAL(8) :: DiameterEffective !!!= 0.00005D0 ! m 
    !f2py REAL(8) :: InterfacialTension !!!= 0.023D0 ! N/m from Belore et al. (2011)

    !f2py CHARACTER(LEN=100) :: BuoyancyMethod !!!= "NULL"

    !f2py CHARACTER(LEN=100) :: OutputFileName !!!= "NULL"
    !f2py CHARACTER(LEN=100) :: OutputFileFormat !!!= "ASCII"

END MODULE



MODULE pyLPT_Data_Lattice_Table
    USE LPT_Data_Lattice_Table
    IMPLICIT NONE

    !f2py INTEGER,ALLOCATABLE :: NE_PIECE(:,:)
    !f2py INTEGER,ALLOCATABLE :: NE_PIECE_INDEX(:,:)
    !f2py INTEGER,ALLOCATABLE :: NE_PIECE_LIST(:)

    !f2py REAL(8)             :: DX(1:2)
    !f2py REAL(8)             :: XMIN(1:2)

END MODULE pyLPT_Data_Lattice_Table

