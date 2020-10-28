MODULE pyLPT_KDTREE_Module
    USE LPT_KDTREE_Module

    IMPLICIT NONE

#ifdef KDTREE
    !f2py REAL(8),ALLOCATABLE :: ElemCenter(:,:)
    !f2py REAL(8),ALLOCATABLE :: ElemRadii(:)

    !NO f2py possible yet - TYPE(KDTREE2),POINTER            :: KD_Tree
    !NO f2py possible yet - TYPE(KDTREE2_RESULT),ALLOCATABLE :: KD_Result(:)

CONTAINS

    SUBROUTINE pyLPT_KDTREE_Initialize
        call LPT_KDTREE_Initialize
    END SUBROUTINE

    SUBROUTINE pyLPT_KDTREE_Search(Lon,Lat,Element,FoundFlag)

        IMPLICIT NONE

        INTEGER,INTENT(OUT) :: Element
        INTEGER,INTENT(OUT) :: FoundFlag
        REAL(8),INTENT(IN) :: Lat
        REAL(8),INTENT(IN) :: Lon
        call LPT_KDTREE_Search(Lon,Lat,Element,FoundFlag)
    END SUBROUTINE

#endif
END MODULE
