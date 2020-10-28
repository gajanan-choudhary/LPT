module pyLPT_print_module

    IMPLICIT NONE

CONTAINS

    SUBROUTINE pyLPT_Print(MessageCore,MessageType,MessageBody)

        IMPLICIT NONE

        CHARACTER(*),INTENT(IN) :: MessageBody
        CHARACTER(*),INTENT(IN) :: MessageType

        INTEGER,INTENT(IN) :: MessageCore

        CALL LPT_Print(MessageCore,MessageType,MessageBody)

    END SUBROUTINE



    SUBROUTINE pyLPT_Progress(Now,Total)

        IMPLICIT NONE

        INTEGER,INTENT(IN) :: Now
        INTEGER,INTENT(IN) :: Total

        CALL LPT_Progress(Now,Total)

    END SUBROUTINE


end module
