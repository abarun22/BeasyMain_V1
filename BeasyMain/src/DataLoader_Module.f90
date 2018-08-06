module DataLoader_Module
    use, intrinsic :: iso_fortran_env;
    use DataStructure_Module;
    use Utility_Module;                                        !==> TO DELETE?
    implicit none;
    private;
    public :: DDDMS_DataLoaderFromFile;

    !╔═══════════════════════════╗
    !║DATALOADER CLASS DEFINITION║
    !╚═══════════════════════════╝
    !┌──────────┐
    !│DATALOADER│
    !└──────────┘
    type :: DDDMS_DataLoaderFromFile
        character(len = :), allocatable :: inputFilePathName;
        
    contains
        procedure :: CheckData;
        procedure :: LoadData;
        final     :: DelDDDMS_DataLoaderFromFile;
        
    end type DDDMS_DataLoaderFromFile

    !┌──────────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATALOADERFROMFILE USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└──────────────────────────────────────────────────────────────────────┘
    interface DDDMS_DataLoaderFromFile
        module procedure NewDDDMS_DataLoaderFromFile;       ! ADD CONSTRUCTOR TO DDDMS_DATALOADERFROMFILE GENERIC INTERFACE
    end interface DDDMS_DataLoaderFromFile

contains

    !┌──────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATALOADERFROMFILE USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└──────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_DataLoaderFromFile) function NewDDDMS_DataLoaderFromFile( inputFileName )
    
        implicit none;
        
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        character(len = *), intent(in) :: inputFileName;
        
        !********************************************
        !BODY OF THE PROGRAM
        !******************************************** 
        NewDDDMS_DataLoaderFromFile%inputFilePathName = inputFileName;
        
    end function NewDDDMS_DataLoaderFromFile !END OF FUNCTION NEWDDDMS_DATALOADERFROMFILE

    !┌─────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATALOADERFROMFILE USER-DEFINED DATA TYPE DESTRUCTOR IMPLEMENTATION│
    !└─────────────────────────────────────────────────────────────────────────┘
    elemental subroutine DelDDDMS_DataLoaderFromFile(self)

        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        type(DDDMS_DataLoaderFromFile), intent(inout) :: self;
        integer                                       :: deallocStatusErr;
        
        !********************************************
        !BODY OF THE PROGRAM
        !******************************************** 
        deallocate(self%inputFilePathName, STAT = deallocStatusErr);
        
    end subroutine DelDDDMS_DataLoaderFromFile !END OF SUBROUTINE DELDDDMS_DATALOADERFROMFILE

    !┌─────────────────────────────────┐
    !│LOADDATA PROCEDURE IMPLEMENTATION│
    !└─────────────────────────────────┘
    subroutine LoadData(self, dataContainer, inputParamsData)
    
        implicit none;
        
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_DataLoaderFromFile) :: self;
        class(DDDMS_DataContainer)      :: dataContainer;
        class(DDDMS_InputParams)        :: inputParamsData;
        integer(INT64)                  :: countLinesFile;
        integer(INT32)                  :: status;
        integer(INT64),  allocatable    :: IndepInterNodesArrayTEMP(:, :);
        integer(INT64)                  :: temporaryIntVal(8); 
        real(REAL64)                    :: temporaryDblVal(4);
        real(REAL64),    allocatable    :: tempA_Matrix(:, :);
        real(REAL64),    allocatable    :: tempB_Matrix(:, :);
        real(REAL64),    allocatable    :: recordRow(:);
        integer(INT64)                  :: k;
        integer(INT64)                  :: j;
        integer(INT64)                  :: i;
        integer(INT64)                  :: countDuplicates;
        integer(INT32)                  :: IndepInterfNodesCount;
        integer(INT64)                  :: scaleIndexVal;
        integer(INT64)                  :: CurrentNodeId;
        integer(INT32)                  :: fid;
        integer(INT32)                  :: fid_2;
        integer(INT64)                  :: numRowBlocksInZone;
        integer(INT64)                  :: recLen;
        integer(INT64)                  :: currRecord;
        integer(INT64)                  :: Res;                  !==> VARIABLE TO DELETE.
        character(len = 200)            :: currentLine;
        character(len = 200)            :: FileNameAMatrix;
        character(len = 200)            :: FileNameBMatrix;
        character(len = 200)            :: FileNameCoordData;
        character(len = 200)            :: interfNodes;
        character(len = 4)              :: seqstring;            !==> VARIABLE TO DELETE.
        
! APB
        integer(INT64),  allocatable    :: bccode(:), nodeid(:);
        real(REAL64),    allocatable    :: bcval(:);
        integer(INT64)                  :: index1;      
        integer(INT64)                  :: ibs,idim;
        integer(INT32)                  :: ZoneID1,ZoneID2,ZoneIDm,ifindex1,ifindex2
        real(REAL64)                    :: tmp
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
        !┌────────────────────────────────────────────────────────────────────────────────────┐
        !│COUNT HOW MANY LINES THERE ARE IN THE FILE ABOUT NODES ON THE INTERFACE IN THE MODEL│
        !└────────────────────────────────────────────────────────────────────────────────────┘
        countLinesFile = 0;
        open(NEWUNIT = fid, ACTION = 'read', FILE = inputParamsData%interfNodesFileName, STATUS = 'old');                  

        do
            read(fid, '(A)', IOSTAT = status) currentLine;
            if (status == IOSTAT_END) then
                exit;! END OF FILE REACHED
            endif  
            countLinesFile = countLinesFile + 1;
        end do
            
        close(fid);
        
        !┌────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
        !│ALLOCATE TEMPORARY ARRAY TO STORE DATA REGARDING NODE ID ON THE INTERFACE BEFORE THOSE DUPLICATES ARE ELIMINATED│
        !└────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
        allocate(IndepInterNodesArrayTEMP( ( countLinesFile - 1 ), 4 ), STAT = status);!--->REMEMBER TO DEALLOCATE DATA TYPE.
        if (status /= 0) then
            print*, "Failed allocation of array IndepInterNodesArrayTEMP!";
            print*, "Error code: ", status;
            pause;
            stop;
        else
            IndepInterNodesArrayTEMP(:, :) = -1; !INITIALISE "INDEPINTERNODESARRAYTEMP" ARRAY TO VALUE (-1).
        end if
        
        allocate(dataContainer%InterfaceNP(( countLinesFile - 1 ),2), STAT=status);
        if (status /= 0) then
            print*, "Failed allocation of array InterfaceNP!";
            print*, "Error code: ", status;
            pause;
            stop;
        else
!            dataContainer%InterfaceNP(:, :) = 0;
        end if
        
        !┌─────────────────────────────────────────────────────────┐
        !│LOAD DATA REGARDING NODES INDEX ON INTERFACE IN THE MODEL│
        !└─────────────────────────────────────────────────────────┘
        countLinesFile = 1;
        open(NEWUNIT = fid, ACTION = 'read', FILE = inputParamsData%interfNodesFileName, STATUS = 'old');
        read(fid, '(a)', IOSTAT = status) currentLine; !READ THE FIRST LINE IN THE FILE CONTAINING AN USELESS VALUE
        do
            read(fid, *, IOSTAT = status) ( temporaryIntVal(j), j = 1, 4 );
            if (status == IOSTAT_END) then
                exit; ! END OF FILE REACHED                
            end if              
                IndepInterNodesArrayTEMP(countLinesFile, 1) = temporaryIntVal(1);
                IndepInterNodesArrayTEMP(countLinesFile, 2) = temporaryIntVal(2);
                IndepInterNodesArrayTEMP(countLinesFile, 3) = temporaryIntVal(3);
                IndepInterNodesArrayTEMP(countLinesFile, 4) = temporaryIntVal(4);
                
            countLinesFile = countLinesFile + 1;
        end do
        close(fid); 

        !┌───────────────────────────────────────────────────────────┐
        !│REMOVE DUPLICATES REGARDING NODES ON INTERFACE IN THE MODEL│
        !└───────────────────────────────────────────────────────────┘
        countDuplicates = 0;
        do i = 1, size(IndepInterNodesArrayTEMP, 1)
            do j = (i + 1), size(IndepInterNodesArrayTEMP, 1)
                if( ( IndepInterNodesArrayTEMP(i, 1) == IndepInterNodesArrayTEMP(j, 1) ) .AND. &
                    ( IndepInterNodesArrayTEMP(i, 1) /= -1 ) ) then
                        
                    IndepInterNodesArrayTEMP(j, :) = -1;
                    countDuplicates = countDuplicates + 1;
                end if
            end do
        end do            

        write(10,*)'countDuplicates:',countDuplicates 
        
        !┌─────────────────────────────────────────────────────────────────────────┐
        !│ALLOCATE SUB DATA STRUCTURE "DATACONTAINER%INDEPENDENINTERFACENODESARRAY"│
        !└─────────────────────────────────────────────────────────────────────────┘
        allocate( dataContainer%IndependenInterfaceNodesArray( ( size( IndepInterNodesArrayTEMP, 1 ) - countDuplicates ), &
                                                                 inputParamsData%numberOfZones ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        if (status /= 0) then
            print*, "Failed allocation of array dataContainer%IndependenInterfaceNodesArray!";
            print*, "Errore code: ", status;
            pause;
            stop;
        else
            dataContainer%IndependenInterfaceNodesArray(:, :) = 0;    
        end if
        
        !┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
        !│COPY INDEPENDENT INTERFACE NODES DATA FROM TEMPORARY DATA STRUCTURE "INDEPINTERNODESARRAYTEMP" TO│
        !│DEFINITIVE DATA STRUCTURE "DATACONTAINER%INDEPENDENINTERFACENODESARRAY"                          │
        !└─────────────────────────────────────────────────────────────────────────────────────────────────┘
        countDuplicates = 1;
        do i = 1, size(IndepInterNodesArrayTEMP, 1)
            ifindex1=0
            ifindex2=0
            if( IndepInterNodesArrayTEMP(i, 1) /= -1 ) then                
                ZoneID1=IndepInterNodesArrayTEMP(i, 2)
                ZoneID2=IndepInterNodesArrayTEMP(i, 4)
                do j = 1,size(inputParamsData%zormap)                    
                    zoneIDm=inputParamsData%zormap(j)
                    if (ZoneID1.eq.zoneIDm)then
                        ifindex1=j
                    endif
                    if (ZoneID2.eq.zoneIDm)then
                        ifindex2=j
                    endif
                enddo
               
                dataContainer%IndependenInterfaceNodesArray( countDuplicates, ifindex1) = IndepInterNodesArrayTEMP(i, 1);
                dataContainer%IndependenInterfaceNodesArray( countDuplicates, ifindex2) = IndepInterNodesArrayTEMP(i, 3);
                
                dataContainer%InterfaceNP(i,1)=IndepInterNodesArrayTEMP(i, 1);
                dataContainer%InterfaceNP(i,2)=IndepInterNodesArrayTEMP(i, 3);
                countDuplicates = countDuplicates + 1;
            end if
        end do     
        
        !───────────────────────────────────────────────────────────────────────────           
        !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)        
        call Utility_OutputDataMatrix( 'Output_FORTRAN_InterfaceNodesArray.out', dataContainer%IndependenInterfaceNodesArray );
!DEC$ ENDIF
        !====================TO PRINT OUT DURING DEBUG STAGE========================
        !───────────────────────────────────────────────────────────────────────────

        !┌───────────────────────────────────────────────────────────────────────────────────────────────────────────┐
        !│DEALLOCATE TEMPORARY DATA STRUCTURE "INDEPINTERNODESARRAYTEMP" AFTER INTERFACE NODE DATA HAS BEEN COPIED IN│ 
        !│DEFINITIVE DATA STRUCTURE "DATACONTAINER%INDEPENDENINTERFACENODESARRAY"                                    │
        !└───────────────────────────────────────────────────────────────────────────────────────────────────────────┘
        if( allocated( IndepInterNodesArrayTEMP ) ) then
            deallocate( IndepInterNodesArrayTEMP );
            if(status /= 0) then
                print*, "failed deallocation of array independeninterfacenodesarraytemp!";
                print*, "Error code: ", status;
                pause;
                stop;
            end if
        end if

        !┌───────────────────────────────────────────────────────────────────────────────┐
        !│ALLOCATE DATA STRUCTURE "DATACONTAINER%ZONESDATA" CONTAINING DATA FOR ALL ZONES│
        !└───────────────────────────────────────────────────────────────────────────────┘
        allocate( dataContainer%ZonesData( inputParamsData%numberOfZones ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
        if(status /= 0) then
            print*, "Failed allocation of array dataContainer%ZonesData!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if

        !┌────────────────────────────────────────────────────────────────────────────────────────┐
        !│LOOP OVER NUMBER OF ZONES IN THE MODEL TO ALLOCATE DATA SUB-STRUCTURE FOR DATA CONTAINER│
        !│NEXT RELEASE===>THE FOLLOWING LOOP COULD BE REPLACED WITH A MULTITHREADED BLOCK         │
        !└────────────────────────────────────────────────────────────────────────────────────────┘
        do i = 1, inputParamsData%numberOfZones
            IndepInterfNodesCount = count( dataContainer%IndependenInterfaceNodesArray(:, i) /= 0 ); !NUMBER OF INDEPENDENT NODES LYING ON THE INTERFACE FOR THE CURRENT ZONE

            !┌─────────────────────────────────────────────────────┐
            !│RETRIVE THE NAME OF THE FILE CONTAINING A MATRIC DATA│
            !└─────────────────────────────────────────────────────┘
            open(NEWUNIT = fid, ACTION = 'read', FILE = self%inputFilePathName, STATUS = 'old');       
            do j = 1, (6 + (i - 1)*4) 
                read(fid, '(A)', IOSTAT = status) FileNameAMatrix;      
            end do
            
            !┌─────────────────────────────────────────────────────┐
            !│RETRIVE THE NAME OF THE FILE CONTAINING B MATRIC DATA│
            !└─────────────────────────────────────────────────────┘
            read(fid, '(A)', IOSTAT = status) FileNameBMatrix;     

            !┌──────────────────────────────────────────────────────────────────────────────┐
            !│RETRIVE THE NAME OF THE FILE CONTAINING NODES COORDINATES FOR THE CURRENT ZONE│
            !└──────────────────────────────────────────────────────────────────────────────┘           
            read(fid, '(A)', IOSTAT = status) FileNameCoordData;    

            close( fid );
            
            !┌───────────────────────────────────────────────────────────────────────────────────┐
            !│OPEN FILE CONTAINING NODES COORDINATES AND BOUNDARY CONDITIONS FOR THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────────────────────┘
            open(NEWUNIT = fid, ACTION = 'read', FILE = FileNameCoordData, status = 'old'); 
            read(fid, '(I8)', IOSTAT = status) temporaryIntVal(1); !VARIABLE CONTAINING THE NUMBER OF NODES BELONGING TO THE CURRENT ZONE           
                      
            write(*,*)'Zone #:',i
            write(*,*)'Reading nodal coordinates file....'
            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "A00" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%A00( 3*( temporaryIntVal(1) - IndepInterfNodesCount ), &
            !                                            3*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%A00( idim*( temporaryIntVal(1) - IndepInterfNodesCount ), &
                                                        idim*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix A00!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
               dataContainer%ZonesData( i )%A00 = 0.0; 
            end if
            
!            write(*,*)'Allocated A00....'
            
            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "A0L" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%A0L( 3*( temporaryIntVal(1) - IndepInterfNodesCount ), &
            !                                            3*IndepInterfNodesCount ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%A0L( idim*( temporaryIntVal(1) - IndepInterfNodesCount ), &
                                                        idim*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix A0L!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%A0L = 0.0;    
            end if            
            
!            write(*,*)'Allocated A0L....'

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "AL0" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%AL0( 3*IndepInterfNodesCount, &
            !                                            3*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%AL0( idim*IndepInterfNodesCount, &
                                                        idim*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix AL0!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
               dataContainer%ZonesData( i )%AL0 = 0.0; 
            end if
            
!            write(*,*)'Allocated AL0....'

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "ALL" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%ALL( 3*IndepInterfNodesCount, &
            !                                            3*IndepInterfNodesCount ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%ALL( idim*IndepInterfNodesCount, &
                                                        idim*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix ALL!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
               dataContainer%ZonesData( i )%ALL = 0.0; 
            end if

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "B00" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%B00( 3*( temporaryIntVal(1) - IndepInterfNodesCount ), &
            !                                            3*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%B00( idim*( temporaryIntVal(1) - IndepInterfNodesCount ), &
                                                        idim*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix B00!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%B00 = 0.0;    
            end if

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "B0L" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%B0L( 3*( temporaryIntVal(1) - IndepInterfNodesCount ), &
            !                                            3*IndepInterfNodesCount ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%B0L( idim*( temporaryIntVal(1) - IndepInterfNodesCount ), &
                                                        idim*IndepInterfNodesCount ), STAT = status );
            
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix B0L!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%B0L = 0.0;    
            end if

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "BL0" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%BL0( 3*IndepInterfNodesCount, &
            !                                            3*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%BL0( idim*IndepInterfNodesCount, &
                                                        idim*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix BL0!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%BL0 = 0.0;    
            end if

            !┌─────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "BLL" FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%BLL( 3*IndepInterfNodesCount, &
            !                                            3*IndepInterfNodesCount ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%BLL( idim*IndepInterfNodesCount, &
                                                        idim*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix BLL!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%BLL = 0.0;    
            end if

            !┌────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB MATRIX "DL" FOR THE CURRENT ZONE│
            !└────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%DL( ( 3*IndepInterfNodesCount ), &
            !                                           ( 3*IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%DL( ( idim*IndepInterfNodesCount ), &
                                                       ( idim*IndepInterfNodesCount ) ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix DL!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%DL = 0.0;    
            end if

            !┌────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "CB" FOR THE CURRENT ZONE│
            !└────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%CB( 3*IndepInterfNodesCount ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%CB( idim*IndepInterfNodesCount ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-matrix DL!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%CB = 0.0;    
            end if

            !┌───────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "Y" FOR THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%Y( 3*temporaryIntVal(1) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%Y( idim*temporaryIntVal(1) ), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-vector Y!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%y = 0.0;    
            end if
            
            !┌────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "XB" FOR THE CURRENT ZONE│
            !└────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%XB( 3*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i )%XB( idim*( temporaryIntVal(1) - IndepInterfNodesCount ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            if(status /= 0) then
                print*, "Failed allocation of sub-vector XB!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%XB = 0.0;    
            end if

            !┌────────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "ULI" FOR THE CURRENT ZONE   │
            !└────────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%ULI( 3*( IndepInterfNodesCount)), STAT = status );
            allocate( dataContainer%ZonesData( i )%ULI( idim*( IndepInterfNodesCount)), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-vector ULI!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%ULI = 0.0;    
            end if

            !┌────────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "resVec" FOR THE CURRENT ZONE│
            !└────────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i )%resVec( 3*( temporaryIntVal(1))), STAT = status );
            allocate( dataContainer%ZonesData( i )%resVec( idim*( temporaryIntVal(1))), STAT = status );
            if(status /= 0) then
                print*, "Failed allocation of sub-vector resVec!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                dataContainer%ZonesData( i )%resVec = 0.0;    
            end if
            
!            write(*,*)'Allocated resvec....'
            
            !┌───────────────────────────────────────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "COORDSNODESINZONE" TO STORE NODES DATA FOR THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────────────────────────────────┘
            allocate( dataContainer%ZonesData( i )%CoordsNodesInZone( temporaryIntVal(1), 4 ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            if(status /= 0) then
                print*, "Failed allocation of sub-vector CoordsNodesInZone!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if            
            
            !┌──────────────────────────────────────────────────────────────────────────────────────────────┐
            !│LOAD NODES COORDINATES DATA FOR THE CURRENT ZONE IN THE SUB-STRUCTURE VECTOR COORDSNODESINZONE│
            !└──────────────────────────────────────────────────────────────────────────────────────────────┘
            countLinesFile = 1;
            do j = 1, temporaryIntVal(1) 
                read(fid, '(I8,3E20.8)', IOSTAT = status) temporaryIntVal(2), temporaryDblVal(1), &
                                                          temporaryDblVal(2), temporaryDblVal(3);  
            
                dataContainer%ZonesData( i ).CoordsNodesInZone( countLinesFile, 1) = temporaryIntVal(2);
                dataContainer%ZonesData( i ).CoordsNodesInZone( countLinesFile, 2) = temporaryDblVal(1);
                dataContainer%ZonesData( i ).CoordsNodesInZone( countLinesFile, 3) = temporaryDblVal(2);
                dataContainer%ZonesData( i ).CoordsNodesInZone( countLinesFile, 4) = temporaryDblVal(3);
                
                countLinesFile = countLinesFile + 1;
                
            end do !END OF LOOP OVER LOADING NODES COORDINATES DATA

            !┌──────────────────────────────────────────────────────────────────────────────────────────────────────┐
            !│ALLOCATE AND INITIALISE SUB VECTOR "BCSTYPE" TO STORE BOUNDARY CONDITIONS APPLIED FOR THE CURRENT ZONE│
            !└──────────────────────────────────────────────────────────────────────────────────────────────────────┘
            !allocate( dataContainer%ZonesData( i ).BCsType( temporaryIntVal(1), 3), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            allocate( dataContainer%ZonesData( i ).BCsType( temporaryIntVal(1), idim), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE.
            if(status /= 0) then
                print*, "Failed allocation of sub-vector BCsType!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if

            allocate(nodeid(1:idim), STAT = status )
            if(status /= 0) then
                print*, "Failed allocation of vector nodeid!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
            allocate(bccode(1:idim), STAT = status )
            if(status /= 0) then
                print*, "Failed allocation of vector bccode!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
            allocate(bcval(1:idim), STAT = status )
            if(status /= 0) then
                print*, "Failed allocation of vector bcval!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
                        
            !┌──────────────────────────────────────────────────┐
            !│LOAD BOUNDARY CONDITIONS DATA FOR THE CURRENT ZONE│
            !└──────────────────────────────────────────────────┘
            read(fid, '(I8)', IOSTAT = status) temporaryIntVal(2); !VARIABLE CONTAINS THE NUMBER OF BOUNDARY CONDITIONS APPLIED
            scaleIndexVal = minval( dataContainer%ZonesData( i )%CoordsNodesInZone( :, 1) );
            
            !do j = 1, temporaryIntVal(2), 3
            !    read(fid, '(2I8,E20.8)', IOSTAT = status) temporaryIntVal(3), temporaryIntVal(4), temporaryDblVal(1);             
            !    read(fid, '(2I8,E20.8)', IOSTAT = status) temporaryIntVal(5), temporaryIntVal(6), temporaryDblVal(2);
            !    read(fid, '(2I8,E20.8)', IOSTAT = status) temporaryIntVal(7), temporaryIntVal(8), temporaryDblVal(3);                    
            !
            !    if (status == IOSTAT_END) then
            !        stop;
            !    endif  
            !
            !    CurrentNodeId = int( temporaryIntVal(3) - scaleIndexVal + 1 );
            !        
            !    !┌────────────────────────────────────────────────────────────────────────────┐
            !    !│CHECK FOR THE BC'S TYPE APPLIED AT THE FIRST LOCAL NODE FOR THE CURRENT ZONE│
            !    !└────────────────────────────────────────────────────────────────────────────┘
            !    if( ( temporaryIntVal(4) == 5) .or. ( temporaryIntVal(4) == 8) .or. ( temporaryIntVal(4) == 39) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 2) )  = temporaryDblVal(1);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  1) = temporaryIntVal(4);
            !            
            !    elseif( ( temporaryIntVal(4) == 6) .or. ( temporaryIntVal(4) == 9) .or. ( temporaryIntVal(4) == 40) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 1) )  = temporaryDblVal(1);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  2) = temporaryIntVal(4);
            !            
            !    elseif( ( temporaryIntVal(4) == 7) .or. ( temporaryIntVal(4) == 10) .or. ( temporaryIntVal(4) == 41) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 0) )  = temporaryDblVal(1);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  3) = temporaryIntVal(4);
            !        
            !    endif
            !
            !    !┌─────────────────────────────────────────────────────────────────────────────┐
            !    !│CHECK FOR THE BC'S TYPE APPLIED AT THE SECOND LOCAL NODE FOR THE CURRENT ZONE│
            !    !└─────────────────────────────────────────────────────────────────────────────┘
            !    if( ( temporaryIntVal(6) == 5) .or. ( temporaryIntVal(6) == 8) .or. ( temporaryIntVal(6) == 39) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 2) )  = temporaryDblVal(2);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  1) = temporaryIntVal(6);
            !        
            !    elseif( ( temporaryIntVal(6) == 6) .or. ( temporaryIntVal(6) == 9) .or. ( temporaryIntVal(6) == 40) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 1) )  = temporaryDblVal(2);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  2) = temporaryIntVal(6);                    
            !        
            !    elseif( ( temporaryIntVal(6) == 7) .or. ( temporaryIntVal(6) == 10) .or. ( temporaryIntVal(6) == 41) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 0) )  = temporaryDblVal(2);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  3) = temporaryIntVal(6);                
            !        
            !    endif
            !        
            !    !┌────────────────────────────────────────────────────────────────────────────┐
            !    !│CHECK FOR THE BC'S TYPE APPLIED AT THE THIRD LOCAL NODE FOR THE CURRENT ZONE│
            !    !└────────────────────────────────────────────────────────────────────────────┘
            !    if( ( temporaryIntVal(8) == 5) .or. ( temporaryIntVal(8) == 8) .or. ( temporaryIntVal(8) == 39) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 2) ) = temporaryDblVal(3);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  1) = temporaryIntVal(8);                    
            !        
            !    elseif( ( temporaryIntVal(8) == 6) .or. ( temporaryIntVal(8) == 9) .or. ( temporaryIntVal(8) == 40) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 1) ) = temporaryDblVal(3);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  2) = temporaryIntVal(8);                    
            !        
            !    elseif( ( temporaryIntVal(8) == 7) .or. ( temporaryIntVal(8) == 10) .or. ( temporaryIntVal(8) == 41) ) then
            !        dataContainer%ZonesData( i )%y( (3*CurrentNodeId - 0) )  = temporaryDblVal(3);
            !        dataContainer%ZonesData( i )%BCsType( CurrentNodeId,  3) = temporaryIntVal(8);                                    
            !        
            !    endif
            !        
            !enddo !END OF LOOP STATEMENT OVER LOADING BOUNDARY CONDITIONS DATA
            
! Read the zonal node ID and boundary condition data from *.z00.txt file            
            do j = 1, temporaryIntVal(2), idim
                do k =1,idim
                    read(fid, '(2I8,E20.8)', IOSTAT = status) nodeid(k), bccode(k), bcval(k);
                enddo                
            
                if (status == IOSTAT_END) then
                    stop;
                endif  
            
                CurrentNodeId = int( nodeid(1) - scaleIndexVal + 1 );
                    
                do k =1,idim
                    if (bccode(k).eq.5 .or. bccode(k).eq.8 .or. bccode(k).eq.39 .or. bccode(k).eq.1 .or. &
                        bccode(k).eq.2 .or. bccode(k).eq.3 .or. bccode(k).eq.4) then                    
                        dataContainer%ZonesData(i)%BCsType( CurrentNodeId,1) = bccode(k);
                        if (inputParamsData%analysType .ne. 1) then
                            dataContainer%ZonesData(i)%y((idim*CurrentNodeId-2)) = bcval(k);
                        else
                            dataContainer%ZonesData(i)%y((CurrentNodeId)) = bcval(k);
                        endif
                    elseif (bccode(k).eq.6 .or. bccode(k).eq.9 .or. bccode(k).eq.40) then                    
                        dataContainer%ZonesData(i)%BCsType( CurrentNodeId,2) = bccode(k);
                        dataContainer%ZonesData(i)%y((idim*CurrentNodeId-1)) = bcval(k);
                    elseif (bccode(k).eq.7 .or. bccode(k).eq.10 .or. bccode(k).eq.41) then                    
                        dataContainer%ZonesData(i)%BCsType( CurrentNodeId,3) = bccode(k);
                        dataContainer%ZonesData(i)%y((idim*CurrentNodeId-0)) = bcval(k);
                    endif
                enddo                    
            enddo 
            
            !┌────────────────────────────────────────────────────────────────────────────────────┐
            !│CLOSE FILE CONTAINING NODES COORDINATES AND BOUNDARY CONDITIONS FOR THE CURRENT ZONE│
            !└────────────────────────────────────────────────────────────────────────────────────┘
            close(fid);
            
            !───────────────────────────────────────────────────────────────────────────           
            !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)        
            write (seqstring,'(I0)') i;
            call Utility_OutputDataMatrix( "Output_FORTRAN_BC_Type_Matrix_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%BCsType );
!DEC$ ENDIF
            !====================TO PRINT OUT DURING DEBUG STAGE========================
            !───────────────────────────────────────────────────────────────────────────
            
!            WRITE(*,*)size( dataContainer%ZonesData( i )%CoordsNodesInZone( :, 1 ) , 1)

!            numRowBlocksInZone = ceiling( dble( 3*size( dataContainer%ZonesData( i )%CoordsNodesInZone( :, 1 ) , 1) )/dble( inputParamsData%blockSizeValue ) ); 
            numRowBlocksInZone = ceiling( dble( idim*size( dataContainer%ZonesData( i )%CoordsNodesInZone( :, 1 ) , 1) )/dble( inputParamsData%blockSizeValue ) );
            
            !┌───────────────────────────────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY DATA STRUCTURE TEMPA_MATRIX FOR THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────┘
            allocate( tempA_Matrix( ( inputParamsData%blockSizeValue*numRowBlocksInZone ), &
                                    ( inputParamsData%blockSizeValue*numRowBlocksInZone ) ), STAT = status );!--->REMEMBER TO DEALLOCATE TEMPA_MATRIX DATA TYPE.
            if(status /= 0) then
                print*, "Failed allocation of array tempA_Matrix!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                tempA_Matrix(:, :) = 0.0;
            end if

            !┌───────────────────────────────────────────────────────────────────┐
            !│ALLOCATE TEMPORARY DATA STRUCTURE TEMPB_MATRIX FOR THE CURRENT ZONE│
            !└───────────────────────────────────────────────────────────────────┘
            allocate( tempB_Matrix( ( inputParamsData%blockSizeValue*numRowBlocksInZone ), &
                                    ( inputParamsData%blockSizeValue*numRowBlocksInZone ) ), STAT = status );!--->REMEMBER TO DEALLOCATE DATA TYPE TEMPB_MATRIX.
            if (status /= 0) then
                print*, "Failed allocation of array tempB_Matrix!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                tempB_Matrix(:, :) = 0.0;
            end if

            !┌──────────────────────────────────────────────────────┐
            !│ALLOCATE DATA STRUCTURE RECORDROW FOR THE CURRENT ZONE│
            !└──────────────────────────────────────────────────────┘
            allocate( recordRow( inputParamsData%blockSizeValue*inputParamsData%blockSizeValue + 2 ), STAT = status );
            if(status /= 0) then
                print*, "Allocation of array recordRow failed!.";
                print*, "Error code: ", status;
                pause;
                stop;
            else
                recordRow(:) = 0.0;
            end if

            !┌───────────────────────────────────────────────────────────────────────────────┐
            !│LOAD DATA CONCERNING MATRIX A AND B FOR THE CURRENT ZONE FROM UNFORMATTED FILES│
            !└───────────────────────────────────────────────────────────────────────────────┘
            inquire( IOLENGTH = recLen ) recordRow;            

            open(NEWUNIT = fid,   ACTION = 'read', FILE = FileNameAMatrix, STATUS = 'old', ACCESS = 'direct', FORM = 'unformatted', RECL = recLen);
            open(NEWUNIT = fid_2, ACTION = 'read', FILE = FileNameBMatrix, STATUS = 'old', ACCESS = 'direct', FORM = 'unformatted', RECL = recLen);
            
            write(*,*)'Reading A and B matrices....'

!            outp=RESHAPE ((/3, 4, 5, 6, 7, 8, 9 , 10/), (/2, 4/));
            
            currRecord = 0;
            ibs=inputParamsData%blockSizeValue;
            do k = 1,numRowBlocksInZone
                do j = 1,numRowBlocksInZone
                    currRecord = currRecord + 1;
                
                    read(fid, REC = currRecord, IOSTAT = status) recordRow;                    
                    if( status /= 0) then
                        pause;
                        exit;
                    endif                    
                    
!                    write(*,*)'Copying record for iteration: k=', k, 'and j=',j, 'Zone ID:',i
                                  
                   tempA_Matrix(((k-1)*ibs+1):(k*ibs),((j-1)*ibs+1):(j*ibs)) &
                                = reshape(recordRow(3:((ibs*ibs)+2)), &
                                  [ibs,ibs] );
                    
                     
                    read(fid_2, REC = currRecord, IOSTAT = status) recordRow;                    
                    if( status /= 0) then
                        exit;
                    endif
                    
                   tempB_Matrix(((k-1)*ibs+1):(k*ibs),((j-1)*ibs+1):(j*ibs)) &
                                = reshape(recordRow(3:((ibs*ibs)+2)), &
                                  [ibs,ibs] );                                  
                end do     
            end do

            close(fid);                 
            close(fid_2); 
            
            write(*,*)'Assigning A and B matrices data ....'
            write(*,*)

            !┌────────────────────────────────────────┐
            !│FILL A00 SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘            
            write(*,*)'Creating A00 ....'
            
            index1=idim*(size(dataContainer%ZonesData( i )%CoordsNodesInZone( :, 1 ) , 1)- IndepInterfNodesCount);
            
            dataContainer%ZonesData( i )%A00(:, :) = tempA_Matrix( 1:index1, 1:index1);                                                                 
            
            !┌────────────────────────────────────────┐
            !│FILL A0L SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘            
            
            write(*,*)'Creating A0L ....'
            dataContainer%ZonesData( i )%A0L(:, :) = tempA_Matrix( 1:index1, (index1)+1:);                                                                 

            !┌────────────────────────────────────────┐
            !│FILL AL0 SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating AL0 ....'
            dataContainer%ZonesData( i )%AL0(:, :) = tempA_Matrix( index1+1:, 1:index1);

            !┌────────────────────────────────────────┐
            !│FILL ALL SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating ALL ....'
            dataContainer%ZonesData( i )%ALL(:, :) = tempA_Matrix( (index1)+1:, (index1)+1:);

            !┌────────────────────────────────────────┐
            !│FILL B00 SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating B00 ....'
            dataContainer%ZonesData( i )%B00(:, :) = tempB_Matrix( 1:index1, 1:index1);
            
            !┌────────────────────────────────────────┐
            !│FILL B0L SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating B0L ....'
            dataContainer%ZonesData( i )%B0L(:, :) = tempB_Matrix( 1:index1, (index1)+1:);
                        
            !┌────────────────────────────────────────┐
            !│FILL BL0 SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating BL0 ....'
            dataContainer%ZonesData( i )%BL0(:, :) = tempB_Matrix((index1)+1:, 1:index1);
                       
            !┌────────────────────────────────────────┐
            !│FILL BLL SUB-MATRIX FOR THE CURRENT ZONE│
            !└────────────────────────────────────────┘
            
            write(*,*)'Creating BLL ....'
            dataContainer%ZonesData( i )%BLL(:, :) = tempB_Matrix( (index1)+1:, (index1)+1:);
            
            if( allocated(nodeid)) then
                deallocate(nodeid);
                if(status /= 0) then
                    print*, "Failed deallocation of array nodeid";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            if( allocated(bccode)) then
                deallocate(bccode);
                if(status /= 0) then
                    print*, "Failed deallocation of array bccode";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            if( allocated(bcval)) then
                deallocate(bcval);
                if(status /= 0) then
                    print*, "Failed deallocation of array bcval";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if
            
            !┌──────────────────────────────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY DATA STRUCTURE RECORDROW FOR THE CURRENT ZONE│
            !└──────────────────────────────────────────────────────────────────┘
            if( allocated( recordRow ) ) then
                deallocate( recordRow );
                if(status /= 0) then
                    print*, "Failed deallocation of array recordRow";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌─────────────────────────────────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY DATA STRUCTURE TEMPA_MATRIX FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────────────┘
            if( allocated( tempA_Matrix ) ) then
                deallocate( tempA_Matrix );
                if(status /= 0) then
                    print*, "Failed deallocation of array tempA_Matrix";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !┌─────────────────────────────────────────────────────────────────────┐
            !│DEALLOCATE TEMPORARY DATA STRUCTURE TEMPB_MATRIX FOR THE CURRENT ZONE│
            !└─────────────────────────────────────────────────────────────────────┘
            if( allocated( tempB_Matrix ) ) then
                deallocate( tempB_Matrix );
                if(status /= 0) then
                    print*, "Failed deallocation of array tempA_Matrix";
                    print*, "Error code: ", status;
                    pause;
                    stop;
                end if
            end if

            !───────────────────────────────────────────────────────────────────────────           
            !====================TO PRINT OUT DURING DEBUG STAGE========================
!DEC$ IF DEFINED(_DEBUG)        
            write (seqstring,'(I0)') i;
            call Utility_OutputDataMatrix( "Output_FORTRAN_A00_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%A00 );
            call Utility_OutputDataMatrix( "Output_FORTRAN_A0L_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%A0L );
            call Utility_OutputDataMatrix( "Output_FORTRAN_AL0_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%AL0 );
            call Utility_OutputDataMatrix( "Output_FORTRAN_ALL_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%ALL );
            
            call Utility_OutputDataMatrix( "Output_FORTRAN_B00_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%B00 );
            call Utility_OutputDataMatrix( "Output_FORTRAN_B0L_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%B0L );
            call Utility_OutputDataMatrix( "Output_FORTRAN_BL0_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%BL0 );
            call Utility_OutputDataMatrix( "Output_FORTRAN_BLL_Zone_"//trim( seqstring )//".out", dataContainer%ZonesData( i )%BLL );
!DEC$ ENDIF
            !====================TO PRINT OUT DURING DEBUG STAGE========================
            !───────────────────────────────────────────────────────────────────────────
            
        end do !END OF LOOP STATEMENT OVER NUMBER OF ZONES
        
    end subroutine LoadData !END OF SUBROUTINE LOADDATA

    !┌──────────────────────────────────┐
    !│CHECKDATA PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────┘
    logical function CheckData(self)
    
        implicit none;        
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_DataLoaderFromFile) :: self;
        character(len = 200)            :: currentLine;
        integer(INT32)                  :: indexPath;
        integer(INT32)                  :: countLinesFile;
        integer(INT32)                  :: status;
        integer(INT64)                  :: file_size;
        integer(INT64)                  :: NumOfZones;
        integer(INT32)                  :: i;
        integer(INT32)                  :: fid;
        logical                         :: file_exists; 
        
        !********************************************
        !BODY OF THE PROGRAM
        !******************************************** 
        
        !********************************************************************************************************
        !CHECK IF THE ENTERED FILE NAME self%inputFilePathName EXISTS AND COUNTS HOW MANY LINES THERE ARE INSIDE IT
        !********************************************************************************************************
        inquire(FILE = self%inputFilePathName , EXIST = file_exists); !CHECK IF THE INPUT FILE "SELF%inputFilePathName" EXISTS.
        inquire(FILE = self%inputFilePathName , SIZE  = file_size);   !CHECK IF THE INPUT FILE "SELF%inputFilePathName" IS EMPTY.
        
        indexPath = scan( self%inputFilePathName, "\", BACK = .TRUE.);
        
        if( file_exists .AND. (file_size /= 0) ) then
            CheckData = .TRUE.;
            open(NEWUNIT = fid, ACTION = 'read', FILE = self%inputFilePathName, STATUS = 'old');
        
            do
                read(fid, '(A)', IOSTAT = status) currentLine;
                if (status == IOSTAT_END) then
                    exit; ! END OF FILE REACHED.ABM
                end if
                
                if( ( index( trim(currentLine), '.txt') /= 0 ) .OR. &
                    ( index( trim(currentLine), '.c')   /= 0 ) .OR. & 
                    ( index( trim(currentLine), '.d')   /= 0 ) )then !THE NAME OF FILE WAS FOUND
                    
                    inquire(FILE = self%inputFilePathName(1:indexPath)//currentLine , EXIST = file_exists); !CHECK IF THE INPUT FILE "SELF%inputFilePathName" EXISTS.
                    inquire(FILE = self%inputFilePathName(1:indexPath)//currentLine , SIZE  = file_size);   !CHECK IF THE INPUT FILE "SELF%inputFilePathName" IS EMPTY.
                    
                    if( ( .not. file_exists) .AND. (file_size == 0) )then
                        print*, "File ", currentLine, " does not exist .";
                        print*, "Please, check input files path name folder ";
                        CheckData = .FALSE.;
                        
                    end if
                end if
            end do  !END OF DO STATEMENT TO THE END OF THE FILE SELF%inputFilePathName    
            
            close(fid);
        else
            write(OUTPUT_UNIT, *) "File name entered does not exist";
            write(OUTPUT_UNIT, *) "or its size is equal to zero!";
            pause;
            stop;
        end if
        
    end function CheckData !END OF FUNCTION CHECKDATA

end module DataLoader_Module