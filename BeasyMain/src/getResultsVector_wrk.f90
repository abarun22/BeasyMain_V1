!
!----------------------------------------------------------------------------------------------    
    subroutine getResultsVector(resultsData)
!----------------------------------------------------------------------------------------------    
!       
        use, intrinsic :: iso_fortran_env;                
        use DataStructure_Module
!        implicit none;        
    
!   Declarations
    
        type(DDDMS_DataContainer)                :: resultsData;
        integer(INT32)                            :: i;
        integer(INT64)                            :: j;
        integer(INT32)                            :: k;
        integer(INT32)                            :: nzones;
        
        nzones=size(resultsData%ZonesData);
        
        do i = 1,nzones            
            iszXB=size(resultsData%ZonesData(i)%XB(:))
            iszresv=size(resultsData%ZonesData(i)%resVec)
            resultsData%ZonesData(i)%resVec(1:iszXB)=resultsData%ZonesData(i)%XB(:)
            resultsData%ZonesData(i)%resVec(iszXB+1:iszresv)=resultsData%ZonesData(i)%ULI(:)
        enddo
    end subroutine