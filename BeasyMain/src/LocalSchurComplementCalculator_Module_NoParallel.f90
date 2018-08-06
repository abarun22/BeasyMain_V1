module LocalSchurComplementCalculator_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    use omp_lib;
    implicit none;
    private;
    public :: DDDMS_LocalSchurComplementCalculator;
    public :: DDDMS_SMLocalSchurComplementCalculator;

    !╔═══════════════════════════════════════════════╗
    !║DDDMS_LOCALSCHURCOMPLEMENTCALCULATOR DEFINITION║
    !╚═══════════════════════════════════════════════╝
    !┌───────────────────────────────────────────────────────────────────────────────┐
    !│ABSTRACT DDDMS_LOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE DEFINITION│
    !└───────────────────────────────────────────────────────────────────────────────┘
    type, abstract :: DDDMS_LocalSchurComplementCalculator
        
    contains 
        procedure(CalculateABTRACT), deferred :: calculate;
        
    end type DDDMS_LocalSchurComplementCalculator
    
    !┌─────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_LOCALSCHURCOMPLEMENTCALCULATOR ABSTRACT USER-DEFINED DATA TYPE PROCEDURE DEFINITION│
    !└─────────────────────────────────────────────────────────────────────────────────────────┘
    abstract interface
        subroutine CalculateABTRACT(self, dataContainer, inputParamsData)
            import;
            class(DDDMS_LocalSchurComplementCalculator) :: self;
            class(DDDMS_DataContainer)                  :: dataContainer;
            class(DDDMS_InputParams)                    :: inputParamsData;
        end subroutine CalculateABTRACT
    end interface

    !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    !│CONCRETE DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE DEFINITION FOR SHARED MEMORY ARCHITECTURE│
    !└────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    type, extends(DDDMS_LocalSchurComplementCalculator) :: DDDMS_SMLocalSchurComplementCalculator
        
    contains 
        procedure :: calculate => calculateOnSM;
        
    end type DDDMS_SMLocalSchurComplementCalculator

    !┌────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SMLocalSchurComplementCalculator
        module procedure NewDDDMS_SMLocalSchurComplementCalculator; ! ADD CONSTRUCTOR TO DDDMS_SMLocalSchurComplementCalculator GENERIC INTERFACE
    end interface DDDMS_SMLocalSchurComplementCalculator

contains

    !┌────────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SMLOCALSCHURCOMPLEMENTCALCULATOR USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SMLocalSchurComplementCalculator) function NewDDDMS_SMLocalSchurComplementCalculator(self)
    
    implicit none;
        
    !********************************************
    !DECLARATIVE ZONE
    !********************************************
    class(DDDMS_SMLocalSchurComplementCalculator) :: self;
        
    !********************************************
    !BODY OF THE PROGRAM
    !********************************************        
        
    end function NewDDDMS_SMLocalSchurComplementCalculator

    !┌──────────────────────────────────┐
    !│CALCULATE PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────┘
    subroutine calculateOnSM(self, dataContainer, inputParamsData)
    
        implicit none;
    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SMLocalSchurComplementCalculator) :: self;
        class(DDDMS_DataContainer)                    :: dataContainer;
        class(DDDMS_InputParams)                      :: inputParamsData;
        integer                                       :: N;               !==>VARIABLE TO DELETE?
	    integer(INT64)                                :: i;               !==>VARIABLE TO DELETE?
        integer(INT64)                                :: j;               !==>VARIABLE TO DELETE?
        integer(INT64)                                :: k;               !==>VARIABLE TO DELETE?
        real(REAL64), allocatable                     :: TempMAT_(:, :);  !==>VARIABLE TO DELETE?
        real(REAL64), allocatable                     :: TempVEC_(:);     !==>VARIABLE TO DELETE?
        real(REAL64)                                  :: temp;            !==>VARIABLE TO DELETE?
        real(REAL64)                                  :: ALPHA;
        real(REAL64)                                  :: BETA;
        real(REAL64)                                  :: GAMMA;
        integer(INT32)                                :: numOfZones;
        integer(INT32)                                :: fid;
        integer(INT64), allocatable                   :: pivot(:);
        real(REAL64),   allocatable                   :: work(:);
        integer(INT32)                                :: info;
        integer(INT32)                                :: status;
        character(len = 4)                            :: seqstring;            !==> VARIABLE TO DELETE.
        integer(INT64)                                :: thread_number=1
    
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘

        numOfZones = size( dataContainer%ZonesData, 1 );               
        do while( thread_number <= numOfZones )        
            N = size( dataContainer%ZonesData( thread_number )%A00, 1 );

            !───────────────────────────────────────────────────────────────────────────           
            !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)
            write(OUTPUT_UNIT, '(A,I5,A,I5)')"(THREAD...: ",thread_number, ")---N = ", N; !TO DELETE --- JUST TO DEBUG 
!DEC$ ENDIF
            !====================TO PRINT OUT DURING DEBUG STAGE========================
            !───────────────────────────────────────────────────────────────────────────
            
            !┌──────────────────────────────────────────────────────┐
            !│PERFORME THE DIVISION OF RHS VECTOR BY G SCALAR FACTOR│
            !└──────────────────────────────────────────────────────┘        
            dataContainer%ZonesData( thread_number )%y = ( dataContainer%ZonesData( thread_number )%y/inputParamsData%gMatScaleFact );
            
            !┌─────────────────────────────────────────────────────────────────┐
            !│ALLOCATE THE INTEGER VECTOR "PIVOT" CONTAINING THE PIVOT INDICES │
            !└─────────────────────────────────────────────────────────────────┘
            allocate( pivot( N ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array pivot failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                pivot(:) = 0.0;
            end if

            !┌───────────────────────────────┐
            !│ALLOCATE THE REAL VECTOR "WORK"│
            !└───────────────────────────────┘
            allocate( work( N ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array work failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                work(:) = 0.0;
            end if
            
            !┌────────────────────────────────────────────────────────────────────────────────────┐
            !│PERFORME THE INVERSIONE OF THE MATRIX A AND REPLACE MATRIX A WITH ITS INVERSE MATRIX│
            !└────────────────────────────────────────────────────────────────────────────────────┘        
            
            !┌──────────────────────────────────────────────────────────────────────┐
            !│(0)--DGETRF: COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A│
            !│USING PARTIAL PIVOTING WITH ROW INTERCHANGES.                         │
            !└──────────────────────────────────────────────────────────────────────┘
            call DGETRF(N, N, dataContainer%ZonesData( thread_number )%A00, N, pivot, info);

            if(info /= 0) then
                pause;
                stop 'Matrix A00 is numerically singular!'
            end if

            !┌────────────────────────────────────────────────────────────────────────┐
            !│(0)--DGETRI: COMPUTES THE INVERSE OF A MATRIX USING THE LU FACTORIZATION│
            !│COMPUTED BY DGETRF.                                                     │
            !└────────────────────────────────────────────────────────────────────────┘
            call DGETRI(N, dataContainer%ZonesData( thread_number )%A00, N, pivot, work, N, info);

            if(info /= 0) then
                pause;
                stop 'Matrix inversion failed!'
            end if

            !───────────────────────────────────────────────────────────────────────────           
            !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)
            write (seqstring,'(I0)') thread_number;
            call Utility_OutputDataMatrix( "Output_FORTRAN_A00_Inverse_Zone_"//trim(seqstring)//".out", dataContainer%ZonesData( thread_number )%A00 );
!DEC$ ENDIF
            !====================TO PRINT OUT DURING DEBUG STAGE========================
            !───────────────────────────────────────────────────────────────────────────

            !┌──────────────────────────────────────────────────────────┐
            !│(1)--DGEMM: COMPUTES MATRIX PRODUCT: AL0 <= AL0*(A00)^(-1)│
            !└──────────────────────────────────────────────────────────┘
            
            !┌─────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└─────────────────────────────────────────┘
            allocate( TempMAT_( size( dataContainer%ZonesData( thread_number )%AL0, 1 ), &
                                size( dataContainer%ZonesData( thread_number )%AL0, 2 ) ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array _TempMAT failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                TempMAT_(:, :) = 0.0;
            end if

            ALPHA = 1.0; 
            BETA  = 0.0;
            GAMMA = 1.0;

            !┌───────────────────────┐
            !│C <= Alpha*A*B + Beta*C│
            !└───────────────────────┘
            call DGEMM('N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       'N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !A: M ROWS BY K COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%A00, 2 ), & !B: K ROWS BY N COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%A00, 1 ), & !C: M ROWS BY N COLUMNS.
                        ALPHA,                                                   & !REAL VALUE USED TO SCALE THE PRODUCT OF MATRICES A AND B.
                        dataContainer%ZonesData( thread_number )%AL0,            & !ARRAY USED TO STORE MATRIX A.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !LEADING DIMENSION OF ARRAY A.
                        dataContainer%ZonesData( thread_number )%A00,            & !ARRAY USED TO STORE MATRIX B.
                        size( dataContainer%ZonesData( thread_number )%A00, 1 ), & !LEADING DIMENSION OF ARRAY B.
                        BETA,                                                    & !REAL VALUE USED TO SCALE MATRIX C.
                        TempMAT_,                                                & !ARRAY USED TO STORE MATRIX C.
                        size( TempMAT_, 1 ) );                                     !LEADING DIMENSION OF ARRAY C.

            !┌───────────────────────────────────────────────────────┐
            !│FILL THE SUB-MATRIX AL0 USING NEW VALUES AL0*(A00)^(-1)│
            !└───────────────────────────────────────────────────────┘
            dataContainer%ZonesData( thread_number )%AL0 = TempMAT_;
            
            !┌───────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└───────────────────────────────────────────┘
            if( allocated( TempMAT_ ) ) then
                deallocate( TempMAT_ );
                if(status /= 0) then
                    print*, "Deallocation of array TempMAT_ failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') thread_number;
            !call Utility_OutputDataMatrix( "Output_FORTRAN_ALO_MOD_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%AL0 );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────
            ALPHA = +1.0; 
            BETA  = -1.0;

            !┌───────────────────────┐
            !│C <= Alpha*A*B + Beta*C│
            !└───────────────────────┘
            call DGEMM('N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       'N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !A: M ROWS BY K COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%B0L, 2 ), & !B: K ROWS BY N COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%B0L, 1 ), & !C: M ROWS BY N COLUMNS.
                        ALPHA,                                                   & !REAL VALUE USED TO SCALE THE PRODUCT OF MATRICES A AND B.
                        dataContainer%ZonesData( thread_number )%AL0,            & !ARRAY USED TO STORE MATRIX A.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !LEADING DIMENSION OF ARRAY A.
                        dataContainer%ZonesData( thread_number )%B0L,            & !ARRAY USED TO STORE MATRIX B.
                        size( dataContainer%ZonesData( thread_number )%B0L, 1 ), & !LEADING DIMENSION OF ARRAY B.
                        BETA,                                                    & !REAL VALUE USED TO SCALE MATRIX C.
                        dataContainer%ZonesData( thread_number )%BLL,            & !ARRAY USED TO STORE MATRIX C.
                        size( dataContainer%ZonesData( thread_number )%BLL, 1 ) ); !LEADING DIMENSION OF ARRAY C.   

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') i;
            !call Utility_OutputDataMatrix( "Output_FORTRAN_BLL_MOD_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%BLL );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────

            !┌───────────────────────────────────────────────────────────────────────┐
            !│(2)--DGEMM: COMPUTES MATRIX-ADDITION PRODUCT: ALL <= ALL - (AL0~)*(A0L)│
            !└───────────────────────────────────────────────────────────────────────┘

            ALPHA = -1.0; 
            BETA  = +1.0;

            !┌───────────────────────┐
            !│C <= Alpha*A*B + Beta*C│
            !└───────────────────────┘
            call DGEMM('N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       'N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !A: M ROWS BY K COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%A0L, 2 ), & !B: K ROWS BY N COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%A0L, 1 ), & !C: M ROWS BY N COLUMNS.
                        ALPHA,                                                   & !REAL VALUE USED TO SCALE THE PRODUCT OF MATRICES A AND B.
                        dataContainer%ZonesData( thread_number )%AL0,            & !ARRAY USED TO STORE MATRIX A.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !LEADING DIMENSION OF ARRAY A.
                        dataContainer%ZonesData( thread_number )%A0L,            & !ARRAY USED TO STORE MATRIX B.
                        size( dataContainer%ZonesData( thread_number )%A0L, 1 ), & !LEADING DIMENSION OF ARRAY B.
                        BETA,                                                    & !REAL VALUE USED TO SCALE MATRIX C.
                        dataContainer%ZonesData( thread_number )%ALL,            & !ARRAY USED TO STORE MATRIX C.
                        size( dataContainer%ZonesData( thread_number )%ALL, 1 ) ); !LEADING DIMENSION OF ARRAY C.

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') i;
            !call Utility_OutputDataMatrix( "Output_FORTRAN_ALL_MOD_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%ALL );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────

            !┌───────────────────────────────────────────────────────────────────────┐
            !│(4)--DGEMM: COMPUTES MATRIX-ADDITION PRODUCT: BL0 <= BL0 - (AL0~)*(B00)│
            !└───────────────────────────────────────────────────────────────────────┘
            
            !┌─────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└─────────────────────────────────────────┘
            allocate( TempMAT_( size( dataContainer%ZonesData( thread_number )%BL0, 1 ), &
                                size( dataContainer%ZonesData( thread_number )%BL0, 2 ) ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array _TempMAT failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                TempMAT_(:, :) = 0.0;
            end if
            
            ALPHA = -1.0; 
            BETA  = +1.0;
            
            !┌────────────────────────────────────────┐
            !│C <= Alpha*A*B + Beta*C --- (AL0~)*(B00)│
            !└────────────────────────────────────────┘
            call DGEMM('N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       'N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !A: M ROWS BY K COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%B00, 2 ), & !B: K ROWS BY N COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%B00, 1 ), & !C: M ROWS BY N COLUMNS.
                        ALPHA,                                                   & !REAL VALUE USED TO SCALE THE PRODUCT OF MATRICES A AND B.
                        dataContainer%ZonesData( thread_number )%AL0,            & !ARRAY USED TO STORE MATRIX A.
                        size( dataContainer%ZonesData( thread_number )%AL0, 1 ), & !LEADING DIMENSION OF ARRAY A.
                        dataContainer%ZonesData( thread_number )%B00,            & !ARRAY USED TO STORE MATRIX B.
                        size( dataContainer%ZonesData( thread_number )%B00, 1 ), & !LEADING DIMENSION OF ARRAY B.
                        BETA,                                                    & !REAL VALUE USED TO SCALE MATRIX C.
                        dataContainer%ZonesData( thread_number )%BL0,            & !ARRAY USED TO STORE MATRIX C.
                        size( dataContainer%ZonesData( thread_number )%BL0, 1 ) ); !LEADING DIMENSION OF ARRAY C.
            

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') i;
            !call Utility_OutputDataMatrix( "Output_FORTRAN_BL0_MOD_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%BL0 );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────

            !┌───────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY TEMPMAT_│
            !└───────────────────────────────────────────┘
            if( allocated( TempMAT_ ) ) then
                deallocate( TempMAT_ );
                if(status /= 0) then
                    print*, "Deallocation of array TempMAT_ failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

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
            
            !┌───────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY WORK│
            !└───────────────────────────────────────┘
            if( allocated( work ) ) then
                deallocate( work, STAT = status );
                if(status /= 0) then
                    print*, "Deallocation of array work failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌──────────────────────────────────────────────────────────────────────────────────────────────┐
            !│(7)--PERFORME THE INVERSIONE OF THE MATRIX BLL AND REPLACES MATRIX BLL WITH ITS INVERSE MATRIX│
            !└──────────────────────────────────────────────────────────────────────────────────────────────┘  
            N = size( dataContainer%ZonesData( thread_number )%BLL, 1 );
            
            !┌─────────────────────────────────────────────────────────────────┐
            !│ALLOCATE THE INTEGER VECTOR "PIVOT" CONTAINING THE PIVOT INDICES │
            !└─────────────────────────────────────────────────────────────────┘
            allocate( pivot( N ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array pivot failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                pivot(:) = 0.0;
            end if
            
            !┌───────────────────────────────┐
            !│ALLOCATE THE REAL VECTOR "WORK"│
            !└───────────────────────────────┘
            allocate( work( N ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array work failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                work(:) = 0.0;
            end if
            
            !┌──────────────────────────────────────────────────────────────────────┐
            !│(7)--DGETRF: COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A│
            !│USING PARTIAL PIVOTING WITH ROW INTERCHANGES.                         │
            !└──────────────────────────────────────────────────────────────────────┘
            call DGETRF(N, N, dataContainer%ZonesData( thread_number )%BLL, N, pivot, info);
            
            if(info /= 0) then
                pause;
                stop 'Matrix BLL is numerically singular!'
            end if
            
            !┌────────────────────────────────────────────────────────────────────────┐
            !│(7)--DGETRI: COMPUTES THE INVERSE OF A MATRIX USING THE LU FACTORIZATION│
            !│COMPUTED BY DGETRF.                                                     │
            !└────────────────────────────────────────────────────────────────────────┘
            call DGETRI(N, dataContainer%ZonesData( thread_number )%BLL, N, pivot, work, N, info);
            
            if(info /= 0) then
                pause;
                stop 'Matrix inversion failed!'
            end if

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

            !┌───────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY SUPPORT ARRAY WORK│
            !└───────────────────────────────────────┘
            if( allocated( work ) ) then
                deallocate( work, STAT = status );
                if(status /= 0) then
                    print*, "Deallocation of array work failed!.";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌────────────────────────────────────────────────────────────────────┐
            !│(8)--DGEMM: COMPUTES MATRIX-ADDITION PRODUCT: DL <= (BLL)^(-1)*(ALL)│
            !└────────────────────────────────────────────────────────────────────┘
            ALPHA = +1.0; 
            BETA  = +0.0;

            !┌───────────────────────┐
            !│C <= Alpha*A*B + Beta*C│
            !└───────────────────────┘
            call DGEMM('N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                       'N',                                                      & !MATRICES A AND B SHOULD NOT BE TRANSPOSED OR CONJUGATE TRANSPOSED BEFORE MULTIPLICATION.
                        size( dataContainer%ZonesData( thread_number )%BLL, 1 ), & !A: M ROWS BY K COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%ALL, 2 ), & !B: K ROWS BY N COLUMNS.
                        size( dataContainer%ZonesData( thread_number )%ALL, 1 ), & !C: M ROWS BY N COLUMNS.
                        ALPHA,                                                   & !REAL VALUE USED TO SCALE THE PRODUCT OF MATRICES A AND B.
                        dataContainer%ZonesData( thread_number )%BLL,            & !ARRAY USED TO STORE MATRIX A.
                        size( dataContainer%ZonesData( thread_number )%BLL, 1 ), & !LEADING DIMENSION OF ARRAY A.
                        dataContainer%ZonesData( thread_number )%ALL,            & !ARRAY USED TO STORE MATRIX B.
                        size( dataContainer%ZonesData( thread_number )%ALL, 1 ), & !LEADING DIMENSION OF ARRAY B.
                        BETA,                                                    & !REAL VALUE USED TO SCALE MATRIX C.
                        dataContainer%ZonesData( thread_number )%DL,             & !ARRAY USED TO STORE MATRIX C.
                        size( dataContainer%ZonesData( thread_number )%DL, 1 ) ); !LEADING DIMENSION OF ARRAY C.
                        
            write(10,*)'Size of LSC array DL:'
            write(10,*)'thread_number,size',thread_number,size(dataContainer%ZonesData( thread_number )%DL)

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') i;
            !call Utility_OutputDataMatrix( "Output_FORTRAN_DL_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%DL );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────
            
            !┌─────────────────────────────────────────────────────────┐
            !│(10)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: CB <= BL0*YB│
            !└─────────────────────────────────────────────────────────┘
            
            !┌─────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY SUPPORT ARRAY TEMPVEC_│
            !└─────────────────────────────────────────┘
            allocate( TempVEC_( size( dataContainer%ZonesData( thread_number )%BL0, 1 ) ), STAT = status );
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
            
            call DGEMV('N', &
                       size( dataContainer%ZonesData( thread_number )%BL0, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( dataContainer%ZonesData( thread_number )%BL0, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                                   & !SPECIFIES THE SCALAR ALPHA.
                       dataContainer%ZonesData( thread_number )%BL0,            & !MATRIX A, SIZE (LDA, N).
                       size( dataContainer%ZonesData( thread_number )%BL0, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       dataContainer%ZonesData( thread_number )%Y,              & !VECTOR X.
                       1,                                                       & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                                    & !SPECIFIES THE SCALAR BETA. 
                       TempVEC_,                                                & !VECTOR Y.
                       1 );                                      !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            !┌────────────────────────────────────────────────────────────────┐
            !│(11)--DGEMV: COMPUTES MATRIX-VECTOR PRODUCT: CB <= (BLL)^(-1)*CB│
            !└────────────────────────────────────────────────────────────────┘

            ALPHA = +1.0; 
            BETA  = +0.0;
            
            call DGEMV('N', &
                       size( dataContainer%ZonesData( thread_number )%BLL, 1 ), & !SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
                       size( dataContainer%ZonesData( thread_number )%BLL, 2 ), & !SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
                       ALPHA,                                                   & !SPECIFIES THE SCALAR ALPHA.
                       dataContainer%ZonesData( thread_number )%BLL,            & !MATRIX A, SIZE (LDA, N).
                       size( dataContainer%ZonesData( thread_number )%BLL, 1 ), & !LEADING DIMENSION OF ARRAY A.
                       TempVEC_,                                                & !VECTOR X.
                       1,                                                       & !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF X.
                       BETA,                                                    & !SPECIFIES THE SCALAR BETA. 
                       dataContainer%ZonesData( thread_number )%CB,             & !VECTOR Y.
                       1 );                                      !SPECIFIES THE INCREMENT FOR THE ELEMENTS OF Y.

            write(10,*)'Size of LSC array CB:'
            write(10,*)'thread_number,size',thread_number,size(dataContainer%ZonesData( thread_number )%CB)
            
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

            !─────────────────────────────────────────────────────           
            !====================TO DELETE========================
            !write (seqstring,'(I0)') thread_number;
            !call Utility_OutputDataVector( "Output_FORTRAN_CB_Vector_"//trim( seqstring )//".out", dataContainer%ZonesData( thread_number )%CB );

            !====================TO DELETE========================
            !─────────────────────────────────────────────────────

        !end do !END OF LOOP STATEMENT ON THE ZONES IN THE MODEL.
            
            thread_number = thread_number + 1;            
        end do !END OF DO-WHILE LOOP STATEMENT INTERNAL TO PARALLEL ZONE.       
       
    end subroutine calculateOnSM

end module LocalSchurComplementCalculator_Module