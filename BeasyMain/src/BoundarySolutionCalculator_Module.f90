module BoundarySolutionCalculator_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public :: DDDMS_BoundarySolutionCalculator;
    public :: DDDMS_SMBoundarySolutionCalculator; 
    
    !╔═══════════════════════════════════════════╗
    !║DDDMS_BOUNDARYSOLUTIONCALCULATOR DEFINITION║
    !╚═══════════════════════════════════════════╝
    !┌───────────────────────────────────────────────────────────────────────────┐
    !│ABSTRACT DDDMS_BOUNDARYSOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION│
    !└───────────────────────────────────────────────────────────────────────────┘
    type, abstract :: DDDMS_BoundarySolutionCalculator
        
    contains 
        procedure(CalculateABTRACT), deferred :: calculate;
        
    end type DDDMS_BoundarySolutionCalculator

    !┌─────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_BOUNDARYSOLUTIONCALCULATOR ABSTRACT USER-DEFINED DATA TYPE PROCEDURE DEFINITION│
    !└─────────────────────────────────────────────────────────────────────────────────────┘
    abstract interface
        subroutine CalculateABTRACT(self, inputData, zonesBounSol, inputParamsData)
            import;
            class(DDDMS_BoundarySolutionCalculator) :: self;
            class(DDDMS_DataContainer)              :: inputData;
            integer(INT32), allocatable             :: zonesBounSol(:);            
            class(DDDMS_InputParams)                :: inputParamsData;
        end subroutine CalculateABTRACT
    end interface

    !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    !│CONCRETE DDDMS_SMBOUNDARYSOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION FOR SHARED MEMORY ARCHITECTURE│
    !└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    type, extends(DDDMS_BoundarySolutionCalculator) :: DDDMS_SMBoundarySolutionCalculator
        
    contains 
        procedure :: calculate => calculateOnSM;
        
    end type DDDMS_SMBoundarySolutionCalculator

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SMBoundarySolutionCalculator
        module procedure NewDDDMS_SMBoundarySolutionCalculator; ! ADD CONSTRUCTOR TO DDDMS_SMBoundarySolutionCalculator GENERIC INTERFACE
    end interface DDDMS_SMBoundarySolutionCalculator

contains

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMBoundarySolutionCalculator USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SMBoundarySolutionCalculator) function NewDDDMS_SMBoundarySolutionCalculator(self)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMBoundarySolutionCalculator) :: self;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
    end function NewDDDMS_SMBoundarySolutionCalculator

    !┌──────────────────────────────────┐
    !│CALCULATE PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────┘
    subroutine calculateOnSM(self, inputData, zonesBounSol, inputParamsData)
    
        use, intrinsic :: iso_fortran_env;
        use omp_lib;        
        implicit none;
    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMBoundarySolutionCalculator) :: self;
        class(DDDMS_DataContainer)                :: inputData;
        class(DDDMS_InputParams)                  :: inputParamsData;
        integer(INT32), allocatable               :: zonesBounSol(:);                                        
        integer(INT32)                            :: i, j, k;
        integer(INT32)                            :: currZone;
        integer(INT32)                            :: status;                
!        integer(INT64)                            :: IndepInterfNodesCount;
        integer(INT32)                            :: IndepInterfNodesCount;
!        integer(INT64)                            :: countStep;        
        integer(INT32)                            :: countStep;        
        real(REAL64)                              :: ALPHA;
        real(REAL64)                              :: BETA;
        real(REAL64), allocatable                 :: UL(:);        
        real(REAL64), allocatable                 :: TempVEC_(:);             !==>VARIABLE TO DELETE?                      
! APB   
        integer(INT32)                            :: inz,intnod,inp,mnod,intfnod,Bcnd;
        integer(INT64)                            :: idim;
        logical                                   :: infnodstat=.false.        
! APB       
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
!-------------------------------
! Initialisation
!-------------------------------        
        if (inputParamsData%analysType .eq. 1) then     ! Problem dimension
            idim=inputParamsData%analysType
        elseif (inputParamsData%analysType .eq. 2) then
            idim=3
        endif

        !┌───────────────────────────────────────────────────────────────────────┐
        !│LOOP OVER ZONES IN THE MODEL FOR WHICH A BOUNDARY SOLUTION WAS REQUIRED│
        !└───────────────────────────────────────────────────────────────────────┘        
    
    !$OMP PARALLEL PRIVATE(i, currZone, j, k, status, IndepInterfNodesCount, countStep, Bcnd, intfnod, ALPHA, BETA, UL, TempVEC_)
    !$OMP DO                
        do i = 1, size( zonesBounSol )
            currZone              = zonesBounSol(i);
            IndepInterfNodesCount = count( inputData%IndependenInterfaceNodesArray(:, currZone) /= 0 ); !NUMBER OF INDEPENDENT NODES LYING ON THE INTERFACE FOR THE CURRENT ZONE
            
!            write(*,*)'i,currZone: ',i,currZone            
            
            !┌───────────────────────────────────────────────────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY ARRAY "UL" TO COLLECT INTERFACE SOLUTION CONCERNING THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────────────────────────┘
!            allocate( UL( 3*IndepInterfNodesCount ), STAT = status );
            allocate( UL( idim*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of UL array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                uL(:) = 0.0;
            end if  
            
            !┌───────────────────────────────────────────────────────────────────────────────┐
            !│COLLECT INTERFACE SOLUTION FOR THE CURRENT ZONE FROM VECTOR U_INTERFACESOLUTION│
            !└───────────────────────────────────────────────────────────────────────────────┘
            
            countStep = 1;
            do j = 1, size( inputData%IndependenInterfaceNodesArray(:, currZone), 1)                 
                if( inputData%IndependenInterfaceNodesArray(j, currZone) /= 0 ) then                                 
                    if (idim .eq. 1) then
                        UL(countStep) = inputData%U_InterfaceSolution(j);                    
                    else
                        UL((idim*countStep - 2):(idim*countStep)) = inputData%U_InterfaceSolution( (idim*j - 2):(idim*j) );
                    endif
                    write(10,*)(3*j - 2),(3*j)                    
                    countStep = countStep + 1;                    
                end if                
            end do            
                    
            inputData%ZonesData(currZone)%ULI(:)=UL(:);            
            
            
            !┌──────────────────────────────────────────────────────────────┐
            !│(1)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: CB <= (CB - DL*UL)│
            !└──────────────────────────────────────────────────────────────┘
            ALPHA = -1.0; 
            BETA  = +1.0;
            
            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%DL, 1 ),  & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%DL, 2 ),  & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%DL,             & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%DL, 1 ),  & !LEADING DIMENSION OF ARRAY A.
                       UL,                                             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%CB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.            
                        
            ! Rewrite interface results for the nodal directions where physical continuity BC's are applied
            ! Interface traction results(CB) are used instead of displacements
            countStep=0
            infnodstat=.false.
            do j = 1, size( inputData%IndependenInterfaceNodesArray(:, currZone), 1)
                intfnod=inputData%IndependenInterfaceNodesArray(j, currZone)
                infnodstat=inputParamsData%isNodMin(inputData,intfnod);
                if( intfnod /= 0) then
                    countStep=countStep+1                    
                    if (infnodstat) then    ! check if the node considered equals the lower node ID of the node pair in interface definition file                                                                  
                            do k=1,size(inputData%ZonesData( currZone )%CoordsNodesInZone(:,1))     ! Get zonal BC type index corresponding to global node ID
                                if (intfnod.eq.int(inputData%ZonesData( currZone )%CoordsNodesInZone(k,1))) then
                                    Bcnd=k                                    
                                    exit
                                endif
                            enddo                            
                          
                            ! check if the node is assigned physical continuity BC's
                            if( inputData%ZonesData( currZone )%BCsType(Bcnd, 1) == 39 ) then   
                                if (idim .eq. 1) then
                                    inputData%ZonesData(currZone)%ULI(countStep)=inputData%ZonesData(currZone)%CB(countStep)
                                else
                                    inputData%ZonesData(currZone)%ULI(idim*countStep - 2)=inputData%ZonesData(currZone)%CB(idim*countStep - 2)
                                endif
                            endif
                            if (idim .ne. 1) then
                                if( inputData%ZonesData(currZone)%BCsType(Bcnd, 2) == 40 ) then
                                    inputData%ZonesData(currZone)%ULI(idim*countStep - 1)=inputData%ZonesData(currZone)%CB(idim*countStep - 1)
                                endif                        
                                if( inputData%ZonesData(currZone)%BCsType(Bcnd, 3) == 41 ) then
                                    inputData%ZonesData(currZone)%ULI(idim*countStep)=inputData%ZonesData(currZone)%CB(idim*countStep)         
                                endif
                            endif
                    endif
                end if
            enddo           
            
            write(10,*)'Interface solution array ULI for zone:',i
            write(10,*)inputData%ZonesData(currZone)%ULI(:)
            
            !┌──────────────────────────────────────────────────────────────────┐
            !│(2)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= B0L*(CB - DL*UL)│
            !└──────────────────────────────────────────────────────────────────┘
            ALPHA = +1.0; 
            BETA  = +0.0;
            
            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%B0L, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%B0L, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%B0L,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%B0L, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%CB,             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.
            
            
            !┌─────────────────────────────────────────────────────────────┐
            !│(3)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= XB - A0L*UL│
            !└─────────────────────────────────────────────────────────────┘
            ALPHA = -1.0; 
            BETA  = +1.0;
            
            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%A0L, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%A0L, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%A0L,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%A0L, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       UL,                                             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.
            
            !┌─────────────────────────────────────────────────────────────┐
            !│(4)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= XB + B00*YB│
            !└─────────────────────────────────────────────────────────────┘
            ALPHA = +1.0; 
            BETA  = +1.0;
            
            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%B00, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%B00, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%B00,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%B00, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%Y,              & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       inputData%ZonesData( currZone )%XB,             & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.
            
            
            !┌───────────────────────────────────────────────────────────────┐
            !│(5)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: XB <= (A00)^(-1)*XB│
            !└───────────────────────────────────────────────────────────────┘
            !┌─────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TEMPVEC_│
            !└─────────────────────────────────────────┘
            allocate( TempVEC_( size( inputData%ZonesData( currZone )%XB, 1 ) ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array TempVEC_ failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                TempVEC_(:) = 0.0;
            end if
            
            ALPHA = +1.0; 
            BETA  = +0.0;
            
            call DGEMV('N',                                            & !MATRICES A NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       size( inputData%ZonesData( currZone )%A00, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( inputData%ZonesData( currZone )%A00, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                          & !SPECIFIES THE SCALAR ALPHA.
                       inputData%ZonesData( currZone )%A00,            & !MATRIX A, SIZE (LDA, N).
                       size( inputData%ZonesData( currZone )%A00, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       inputData%ZonesData( currZone )%XB,             & !VECTOR X.
                       1,                                              & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                           & !SPECIFIES THE SCALAR BETA. 
                       TempVEC_,                                       & !VECTOR Y.
                       1 );                                              !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.
            
            !┌─────────────────────────────────────────────────────┐
            !│FILL THE SUB-MATRIX XB USING NEW VALUES (A00)^(-1)*XB│
            !└─────────────────────────────────────────────────────┘
            inputData%ZonesData( currZone )%XB = TempVEC_;         
            
            
            !┌───────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└───────────────────────────────────────────┘
            if( allocated( TempVEC_ ) ) then
                deallocate( TempVEC_ );
                if(status /= 0) then
                    print*, "Deallocation of array TempVEC_ failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
          
            !┌─────────────────────────────────────────────────────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY ARRAY "UL" TO COLLECT INTERFACE SOLUTION CONCERNING THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────────────────────────────────┘
            if( allocated( UL ) ) then
                deallocate( UL );
                if(status /= 0) then
                    print*, "Deallocation of array UL failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
        end do  !END OF LOOP STATEMENT OVER ZONES IN THE MODEL FOR WHICH A BOUNDARY SOLUTION WAS REQUIRED          
        
    !$OMP ENDDO
    !$OMP END PARALLEL    
        
    end subroutine calculateOnSM    
    
end module BoundarySolutionCalculator_Module
    