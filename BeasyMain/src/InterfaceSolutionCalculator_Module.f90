module InterfaceSolutionCalculator_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public :: DDDMS_InterfaceSolutionCalculator;
    public :: DDDMS_SMInterfaceSolutionCalculator;

    !╔════════════════════════════════════════════╗
    !║DDDMS_INTERFACESOLUTIONCALCULATOR DEFINITION║
    !╚════════════════════════════════════════════╝
    !┌────────────────────────────────────────────────────────────────────────────┐
    !│ABSTRACT DDDMS_INTERFACESOLUTIONCALCULATOR USER-DEFINED DATA TYPE DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────┘
    type, abstract :: DDDMS_InterfaceSolutionCalculator
        
    contains 
!        procedure(CalculateABTRACT), deferred :: calculate;
        procedure(CalculateABTRACT), deferred :: Dense_InterfaceSolution;
        procedure(CalculateABTRACT), deferred :: Sparse_InterfaceSolution        
    end type DDDMS_InterfaceSolutionCalculator
    
    !┌─────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_LOCALSCHURCOMPLEMENTCALCULATOR ABSTRACT USER-DEFINED DATA TYPE PROCEDURE DEFINITION│
    !└─────────────────────────────────────────────────────────────────────────────────────────┘
    abstract interface
        subroutine CalculateABTRACT(self, inputData, inputParamsData)
            import;
            class(DDDMS_InterfaceSolutionCalculator) :: self;
            class(DDDMS_DataContainer)               :: inputData;
            class(DDDMS_InputParams)                 :: inputParamsData;
        end subroutine CalculateABTRACT
    end interface

    !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    !│CONCRETE DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE DEFINITION FOR SHARED MEMORY ARCHITECTURE│
    !└────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    type, extends(DDDMS_InterfaceSolutionCalculator) :: DDDMS_SMInterfaceSolutionCalculator
        
    contains 
!        procedure       :: calculate => calculateOnSM;
        procedure       :: Dense_InterfaceSolution => calculateDense;
        procedure       :: Sparse_InterfaceSolution => calculateSparse;
        
    end type DDDMS_SMInterfaceSolutionCalculator

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SMInterfaceSolutionCalculator
        module procedure NewDDDMS_SMInterfaceSolutionCalculator; ! ADD CONSTRUCTOR TO DDDMS_SMInterfaceSolutionCalculator GENERIC INTERFACE
    end interface DDDMS_SMInterfaceSolutionCalculator

contains

    !┌────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SMInterfaceSolutionCalculator) function NewDDDMS_SMInterfaceSolutionCalculator(self)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMInterfaceSolutionCalculator) :: self;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
    end function NewDDDMS_SMInterfaceSolutionCalculator
    
    !┌────────────────────────────────────┐
    !│"CALCULATE" PROCEDURE IMPLEMENTATION│
    !└────────────────────────────────────┘
    subroutine calculateDense(self, inputData, inputParamsData)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMInterfaceSolutionCalculator) :: self;
        class(DDDMS_DataContainer)                 :: inputData;
        class(DDDMS_InputParams)                   :: inputParamsData;
        integer(INT64), allocatable                :: pivot(:);
        integer(INT32)                             :: info;
        integer(INT32)                             :: status;
! APB
        integer(INT64)                             :: idim;
! APB
         
!-------------------------------
! Initialisation
!-------------------------------        
        if (inputParamsData%analysType .eq. 1) then     ! Problem dimension
            idim=inputParamsData%analysType
        elseif (inputParamsData%analysType .eq. 2) then
            idim=3
        endif

        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘       
        !┌─────────────────────────────────────────────────────────────────┐
        !│ALLOCATE THE INTEGER VECTOR "PIVOT" CONTAINING THE PIVOT INDICES │
        !└─────────────────────────────────────────────────────────────────┘
        allocate( pivot( size( inputData%SGlob, 1 ) ), STAT = status );
        if(status /= 0) then
            print*, "Allocation of array pivot failed!.";
            print*, "Error code: ", status;
            pause;
            stop;
        else
            pivot = 0.0;
        end if

        write(10,*)'SGlob:'
        write(10,*)inputData%SGlob
        
        write(10,*)'gGlob:'
        write(10,*)inputData%gGlob
        
        !┌───────────────────────────────────────────────────────────────────────────────────────────┐
        !│FIND THE SOLUTION OF THE EQUATIONS (SGLOBAL)A*X = (GGLOBAL)B USING THE LAPACK ROUTINE SGESV│
        !└───────────────────────────────────────────────────────────────────────────────────────────┘
       	call DGESV(size( inputData%SGlob, 1 ), & !IS THE ORDER N OF MATRIX A AND THE NUMBER OF ROWS OF MATRIX B.
                   1,                          & !IS THE NUMBER OF RIGHT-HAND SIDES; THAT IS, THE NUMBER OF COLUMNS OF MATRIX B.
                   inputData%SGlob,            & !S THE GENERAL MATRIX A TO BE FACTORED.
                   size( inputData%SGlob, 1 ), & !IS THE LEADING DIMENSION OF THE ARRAY SPECIFIED FOR A.
                   pivot,                      & 
                   inputData%gGlob,            & !IS THE GENERAL MATRIX B, CONTAINING THE NRHS RIGHT-HAND SIDES OF THE SYSTEM. 
                   size( inputData%gGlob, 1 ), & !IS THE LEADING DIMENSION OF THE ARRAY SPECIFIED FOR B.
                   info);

        if( info > 0 ) then
            write(*,*)'The diagonal element of the triangular factor of inputData%SGlob,';
            write(*,*)'U(',info,',',info,') is zero, so that';
            write(*,*)'inputData%SGlob is singular; the solution could not be computed.';
            pause;
            stop;
        end if
        
        !┌────────────────────────────────────────────────────────────┐
        !│ALLOCATE AND INITIALISE VECTOR INPUTDATA%U_INTERFACESOLUTION│
        !└────────────────────────────────────────────────────────────┘
!        allocate( inputData%U_InterfaceSolution( 3*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        allocate( inputData%U_InterfaceSolution( idim*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );
        if(status /= 0) then
            print*, "Failed allocation of U_InterfaceSolution!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%U_InterfaceSolution = 0.0; 
        end if               
        
        !┌─────────────────────────────────────────────────────────────────┐
        !│FILL THE VECTOR GGLOB USING THE SOLUTION OF THE EQUATIONS A*X = B│
        !└─────────────────────────────────────────────────────────────────┘
        inputData%U_InterfaceSolution = inputData%gGlob;
        
        write(10,*)'Size of IndependenInterfaceNodesArray:'
        write(10,*)size(inputData%IndependenInterfaceNodesArray, 1)
        
        write(10,*)'Interface solution array:'
        write(10,*)inputData%U_InterfaceSolution
        
        !┌────────────────────────────────────────┐
        !│DEALLOCATE TEMPORARY SUPPORT ARRAY PIVOT│
        !└────────────────────────────────────────┘
        if( allocated( pivot ) ) then
            deallocate( pivot, STAT = status );
            if(status /= 0) then
                print*, "Deallocation of array pivot failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            end if
        end if
        
        !────────────────────────────────────────────────────           
        !====================TO DEBUD========================
!DEC$ IF DEFINED(_DEBUG)
        call Utility_OutputDataVector( "Output_FORTRAN_Interface_Solution_Vector.out",  inputData%U_InterfaceSolution );
!DEC$ ENDIF
        !====================TO DEBUD========================
        !─────────────────────────────────────────────────────
       
    end subroutine calculateDense    
!
!****************************************************************************************
    subroutine calculateSparse(self, inputData, inputParamsData)
!****************************************************************************************
!   Objective: Usage of pardiso sparse routines to obtain the interface solution 
!              For system of equations  SGlob*UL=gGLOB
!****************************************************************************************
        implicit none;
        
!   Declarations
        class(DDDMS_SMInterfaceSolutionCalculator) :: self;
        class(DDDMS_DataContainer)                 :: inputData;
        class(DDDMS_InputParams)                   :: inputParamsData;
        integer(INT32)                             :: inz,i,j,k;
        integer(INT32)                             :: status;
        integer(INT64)                             :: idim,pt(64),szA(2),nrow,ncol,iparm(64),mtype;        
        integer(INT64),  allocatable               :: iia(:),ia(:),ja(:),perm(:);        
        integer(INT64)                             :: maxfct,mnum,phase,nrhs,msglvl,error,idum(1)
        real(REAL64),    allocatable               :: a(:);
        real(REAL64)                               :: ddum(1)
!
!-------------------------------
! Initialisation
!-------------------------------        
!
        inz=0
        maxfct=1
        mnum=1
        mtype=11
        nrhs=1
        msglvl=1
        error=0
        pt(1:64)=0
        
        if (inputParamsData%analysType .eq. 1) then     ! Problem dimension
            idim=inputParamsData%analysType
        elseif (inputParamsData%analysType .eq. 2) then
            idim=3
        endif
!----------------------------------------------------        
! Conversion of array SGlob to CSR format
!----------------------------------------------------
!
        szA=shape(inputData%SGlob)
        nrow=szA(1)
        ncol=szA(2)
        do i = 1,nrow
            do j = 1,ncol
                if (inputData%SGlob(i,j).ne.0) then
                    inz=inz+1
                endif
            enddo
        enddo
        
        allocate(a(1:inz), STAT = status )
        if(status /= 0) then
            print*, "Failed allocation of vector a!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if
        
        allocate(ja(1:inz), STAT = status )
        if(status /= 0) then
            print*, "Failed allocation of vector ja!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if
        
        allocate(iia(1:inz), STAT = status )
        if(status /= 0) then
            print*, "Failed allocation of vector iia!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if
            
        allocate(ia(1:(nrow+1)), STAT = status )
        if(status /= 0) then
            print*, "Failed allocation of vector ia!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if
        
        allocate(perm(1:nrow), STAT = status )
        if(status /= 0) then
            print*, "Failed allocation of vector perm!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            perm(1:nrow)=0
        end if
        
        allocate( inputData%U_InterfaceSolution(1:nrow) , STAT = status );
        if(status /= 0) then
            print*, "Failed allocation of U_InterfaceSolution!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%U_InterfaceSolution = 0.0; 
        end if               

! Form 'a' vector and the indices of SGlob for the non-zero terms
        inz=0
        do i = 1,nrow
            do j = 1,ncol
                if (inputData%SGlob(i,j).ne.0) then
                    inz=inz+1
                    a(inz)=inputData%SGlob(i,j)
                    iia(inz)=i
                    ja(inz)=j
                endif
            enddo
        enddo

! Form 'ia' vector with pointers to first non zero term in the column of each row
        do i = 1,nrow
            do j = 1,ncol
                if (inputData%SGlob(i,j).ne.0) then
                    exit
                endif                
            enddo
            do k = 1,inz
                if (i.eq.iia(k).and.j.eq.ja(k)) then
                    ia(i)=k
                    exit
                endif
            enddo
        enddo
        ia(nrow+1)=inz+1
        
! Initialize pardiso solver
        iparm(1) = 0 ! no solver default
        iparm(2) = 2 ! fill-in reordering from METIS
        iparm(3) = 1 ! numbers of processors
        iparm(4) = 0 ! no iterative-direct algorithm
        iparm(5) = 0 ! no user fill-in reducing permutation
        iparm(6) = 0 ! =0 solution on the first n components of x
        iparm(7) = 0 ! not in use
        iparm(8) = 9 ! numbers of iterative refinement steps
        iparm(9) = 0 ! not in use
        iparm(10) = 13 ! handle non-symmetric matrices
        iparm(11) = 1 ! use nonsymmetric permutation and scaling MPS
        iparm(12) = 0 ! not in use
        iparm(13) = 1 ! maximum weighted matching algorithm is switched-on (default for non-symmetric)
        iparm(14) = 0 ! Output: number of perturbed pivots
        iparm(15) = 0 ! not in use
        iparm(16) = 0 ! not in use
        iparm(17) = 0 ! not in use
        iparm(18) = -1 ! Output: number of nonzeros in the factor LU
        iparm(19) = -1 ! Output: Mflops for LU factorization
        iparm(20) = 0 ! Output: Numbers of CG Iterations
!        iparm(24) = 10 ! 

! Analysis, Numerical Factorization, Solution and Iterative refinement
        phase=13     
        CALL pardiso_64 (pt, maxfct, mnum, mtype, phase, nrow, a, ia, ja,  &
                         perm, nrhs, iparm, msglvl, inputData%gGlob, inputData%U_InterfaceSolution, error)
        
        !write(10,*)'Interface solution array:'
        !write(10,*)inputData%U_InterfaceSolution

! Release internal memory        
        phase=-1
        CALL pardiso_64 (pt, maxfct, mnum, mtype, phase, nrow, ddum, idum, idum,  &
                         idum, nrhs, iparm, msglvl, ddum, ddum, error)
        
    end subroutine calculateSparse
    
end module InterfaceSolutionCalculator_Module