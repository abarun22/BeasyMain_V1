module GlobalSchurComplementAssembler_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public DDDMS_GlobalSchurComplementAssembler;

    !╔═══════════════════════════════════════════════╗
    !║DDDMS_GLOBALSCHURCOMPLEMENTASSEMBLER DEFINITION║
    !╚═══════════════════════════════════════════════╝
    !┌──────────────────────────────┐
    !│GLOBALSCHURCOMPLEMENTASSEMBLER│
    !└──────────────────────────────┘
    type :: DDDMS_GlobalSchurComplementAssembler
        
    contains
        procedure :: Assembly;
        
    end type DDDMS_GlobalSchurComplementAssembler
    
    !┌──────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_GLOBALSCHURCOMPLEMENTASSEMBLER USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└──────────────────────────────────────────────────────────────────────────────────┘
    interface DDDMS_GlobalSchurComplementAssembler
        module procedure NewDDDMS_GlobalSchurComplementAssembler;       ! ADD CONSTRUCTOR TO DDDMS_DATALOADERFROMFILE GENERIC INTERFACE
    end interface DDDMS_GlobalSchurComplementAssembler

contains

    !┌──────────────────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_GLOBALSCHURCOMPLEMENTASSEMBLER USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└──────────────────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_GlobalSchurComplementAssembler) function NewDDDMS_GlobalSchurComplementAssembler(  )
    
        implicit none;
        
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        
        !********************************************
        !BODY OF THE PROGRAM
        !******************************************** 
        
    end function NewDDDMS_GlobalSchurComplementAssembler !END OF FUNCTION NEWDDDMS_DATALOADERFROMFILE
    
    !┌─────────────────────────────────┐
    !│ASSEMBLY PROCEDURE IMPLEMENTATION│
    !└─────────────────────────────────┘
    subroutine Assembly(self, inputData, inputParamsData)

        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_GlobalSchurComplementAssembler) :: self;
        class(DDDMS_DataContainer)                  :: inputData;
        class(DDDMS_InputParams)                    :: inputParamsData;
        integer(INT32)                              :: numOfZones;
        integer(INT64)                              :: i;
        integer(INT64)                              :: j;
        integer(INT64)                              :: k;
        integer(INT32)                              :: status;
        integer(INT64)                              :: RowCounter;
        integer(INT64)                              :: ColCounter;        
        integer(INT64)                              :: LocRowX;
        integer(INT64)                              :: LocRowY;
        integer(INT64)                              :: LocRowZ;
        integer(INT64)                              :: GloRowX;
        integer(INT64)                              :: GloRowY;
        integer(INT64)                              :: GloRowZ;
        integer(INT64)                              :: LocColX;
        integer(INT64)                              :: LocColY;
        integer(INT64)                              :: LocColZ;
        integer(INT64)                              :: GloColX;
        integer(INT64)                              :: GloColY;
        integer(INT64)                              :: GloColZ;
        character(len = 4)                          :: seqstring;            !==> VARIABLE TO DELETE.
        
! APB
        integer(INT64)                              :: ifnode;
        integer(INT64)                              :: idim;
! APB       
       
!-------------------------------
! Initialisation
!-------------------------------        
        if (inputParamsData%analysType .eq. 1) then     ! Problem dimension
            idim=inputParamsData%analysType
        elseif (inputParamsData%analysType .eq. 2) then
            idim=3
        endif

        !********************************************
        !BODY OF THE PROGRAM
        !********************************************        
        !┌───────────────────────────────────────────────────────────┐
        !│ALLOCATE AND INITIALISE MATRIX "GGLOB" FOR THE CURRENT ZONE│
        !└───────────────────────────────────────────────────────────┘
!        allocate( inputData%gGlob( 3*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        allocate( inputData%gGlob( idim*size( inputData%IndependenInterfaceNodesArray, 1 ) ) , STAT = status );
        if(status /= 0) then
            print*, "Failed allocation of gGlob!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%gGlob(:) = 0.0; 
        end if

        !┌───────────────────────────────────────────────────────────┐
        !│ALLOCATE AND INITIALISE MATRIX "SGLOB" FOR THE CURRENT ZONE│
        !└───────────────────────────────────────────────────────────┘
        !allocate( inputData%SGlob( 3*size( inputData%IndependenInterfaceNodesArray, 1 ), &
        !                           3*size( inputData%IndependenInterfaceNodesArray, 1 ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        allocate( inputData%SGlob( idim*size( inputData%IndependenInterfaceNodesArray, 1 ), &
                                   idim*size( inputData%IndependenInterfaceNodesArray, 1 ) ), STAT = status );
        if(status /= 0) then
            print*, "Failed allocation of SGlob!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            inputData%SGlob(:, :) = 0.0; 
        end if
        
        
        numOfZones = size( inputData%ZonesData, 1 );
        
        do k = 1,numOfZones
            RowCounter = 0; 
            write(10,*)'Interface nodes for zone:',k
            !write(10,*)inputData%IndependenInterfaceNodesArray(:, k)
            
            !write(10,*)'Size of Interface nodes array:'
            !write(10,*)size( inputData%IndependenInterfaceNodesArray(:, k))
            
            !write(10,*)'Index for assembly'                   
            !write(10,*) 'LocRowX, LocRowY, LocRowZ'
            !write(10,*) 'GloRowX, GloRowY, GloRowZ'
            
            !write(10,*)'Size of CB array:'
            !write(10,*)size(inputData%ZonesData( k )%CB)
            
            do i = 1, size( inputData%IndependenInterfaceNodesArray(:, k) )
            
                if( inputData%IndependenInterfaceNodesArray(i, k) /= 0 ) then
                                
                    !┌────────────────────┐
                    !│INDEX FOR LOCAL ROWS│
                    !└────────────────────┘
                    !LocRowX = int( 3*RowCounter + 1 );
                    !LocRowY = int( 3*RowCounter + 2 );
                    !LocRowZ = int( 3*RowCounter + 3 );
                    
                    LocRowX = int( idim*RowCounter + 1 );
                    if (idim .ne. 1) then
                        LocRowY = int( idim*RowCounter + 2 );
                        LocRowZ = int( idim*RowCounter + 3 );
                    endif

                    !┌─────────────────────┐
                    !│INDEX FOR GLOBAL ROWS│
                    !└─────────────────────┘
                    !GloRowX = int( 3*( i - 1 ) + 1 );
                    !GloRowY = int( 3*( i - 1 ) + 2 );
                    !GloRowZ = int( 3*( i - 1 ) + 3 );                    
                    !
                    GloRowX = int( idim*( i - 1 ) + 1 );
                    if (idim .ne. 1) then
                        GloRowY = int( idim*( i - 1 ) + 2 );
                        GloRowZ = int( idim*( i - 1 ) + 3 );
                    endif
                    
                    inputData%gGlob( GloRowX ) = inputData%gGlob( GloRowX ) + inputData%ZonesData( k )%CB( LocRowX );  
                    if (idim .ne. 1) then
                        inputData%gGlob( GloRowY ) = inputData%gGlob( GloRowY ) + inputData%ZonesData( k )%CB( LocRowY );
                        inputData%gGlob( GloRowZ ) = inputData%gGlob( GloRowZ ) + inputData%ZonesData( k )%CB( LocRowZ );
                    endif
                    
                    !write(10,*) LocRowX, LocRowY, LocRowZ               
                    !ifnode=inputData%IndependenInterfaceNodesArray(i, k)
                    !write(10,*) GloRowX, GloRowY, GloRowZ
                    !write(10,*) 'i,ifnode',i,ifnode
                    
                    !=====================================================================        
                    ColCounter = 0;    
                    do j = 1, size( inputData%IndependenInterfaceNodesArray(:, k) )
                    
                        if( inputData%IndependenInterfaceNodesArray(j, k) /= 0 ) then
                             
                            !┌───────────────────────┐
                            !│INDEX FOR LOCAL COLUMNS│
                            !└───────────────────────┘
                            !LocColX = int( 3*ColCounter + 1 );
                            !LocColY = int( 3*ColCounter + 2 );
                            !LocColZ = int( 3*ColCounter + 3 );
                        
                            LocColX = int( idim*ColCounter + 1 );
                            if (idim .ne. 1) then
                                LocColY = int( idim*ColCounter + 2 );
                                LocColZ = int( idim*ColCounter + 3 );
                            endif
                            
                            !┌────────────────────────┐
                            !│INDEX FOR GLOBAL COLUMNS│
                            !└────────────────────────┘
                            !GloColX = int( 3*( j - 1 ) + 1 );
                            !GloColY = int( 3*( j - 1 ) + 2 );
                            !GloColZ = int( 3*( j - 1 ) + 3 );                        
                            
                            GloColX = int( idim*( j - 1 ) + 1 );
                            if (idim .ne. 1) then
                                GloColY = int( idim*( j - 1 ) + 2 );
                                GloColZ = int( idim*( j - 1 ) + 3 );                                    
                            endif
                            
                            !write(10,*) 'LocColX, LocColY, LocColZ'
                            !write(10,*) LocColX, LocColY, LocColZ
                            !
                            !write(10,*) 'GloColX, GloColY, GloColZ'
                            !write(10,*) GloColX, GloColY, GloColZ
                            !
                            !=========================================================        
                            inputData%SGlob( GloRowX, GloColX ) = inputData%SGlob( GloRowX, GloColX ) + inputData%ZonesData( k )%DL( LocRowX, LocColX );  
                            if (idim .ne. 1) then
                                inputData%SGlob( GloRowX, GloColY ) = inputData%SGlob( GloRowX, GloColY ) + inputData%ZonesData( k )%DL( LocRowX, LocColY );  
                                inputData%SGlob( GloRowX, GloColZ ) = inputData%SGlob( GloRowX, GloColZ ) + inputData%ZonesData( k )%DL( LocRowX, LocColZ );  
                            
                                inputData%SGlob( GloRowY, GloColX ) = inputData%SGlob( GloRowY, GloColX ) + inputData%ZonesData( k )%DL( LocRowY, LocColX );  
                                inputData%SGlob( GloRowY, GloColY ) = inputData%SGlob( GloRowY, GloColY ) + inputData%ZonesData( k )%DL( LocRowY, LocColY );                         
                                inputData%SGlob( GloRowY, GloColZ ) = inputData%SGlob( GloRowY, GloColZ ) + inputData%ZonesData( k )%DL( LocRowY, LocColZ ); 
                            
                                inputData%SGlob( GloRowZ, GloColX ) = inputData%SGlob( GloRowZ, GloColX ) + inputData%ZonesData( k )%DL( LocRowZ, LocColX );
                                inputData%SGlob( GloRowZ, GloColY ) = inputData%SGlob( GloRowZ, GloColY ) + inputData%ZonesData( k )%DL( LocRowZ, LocColY );  
                                inputData%SGlob( GloRowZ, GloColZ ) = inputData%SGlob( GloRowZ, GloColZ ) + inputData%ZonesData( k )%DL( LocRowZ, LocColZ );  
                            endif
                            ColCounter = ColCounter + 1;
                            !=========================================================
                        end if
                    end do
                
                    RowCounter = RowCounter + 1; 
                end if
            end do
            
        end do !END OF LOOP STATEMENT ON NUMBER OF ZONES IN THE MODEL 
        
        write(10,*)'Size of gGlob array:'
        write(10,*)size(inputData%gGlob)
        !write(10,*)inputData%gGlob
        !
        write(10,*)'Size of SGlob array:'
        write(10,*)size(inputData%SGlob)
        !write(10,*)inputData%SGlob
        
        !───────────────────────────────────────────────────────────────────────────           
        !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)
        call Utility_OutputDataMatrix( "Output_FORTRAN_SGLOB_Matrix.out", inputData%SGlob );
        call Utility_OutputDataVector( "Output_FORTRAN_gGlob_Vector.out", inputData%gGlob );
!DEC$ ENDIF
        !====================TO PRINT OUT DURING DEBUG STAGE========================
        !───────────────────────────────────────────────────────────────────────────
        
    end subroutine Assembly !END OF SUBROUTINE DELDDDMS_DATALOADERFROMFILE
    
end module GlobalSchurComplementAssembler_Module