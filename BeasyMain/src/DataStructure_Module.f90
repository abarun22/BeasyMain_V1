module DataStructure_Module
    use, intrinsic :: iso_fortran_env;
    implicit none;
    private;
    public :: DDDMS_DataContainer;
    public :: DDDMS_InputParams;

    !╔═════════════════╗
    !║DDDMS_INPUTPARAMS║
    !╚═════════════════╝
    type :: DDDMS_InputParams
        integer(INT32)                  :: analysType     = 2;
        real(REAL64)                    :: gMatScaleFact  = 1000.0;
        integer(INT32)                  :: numberOfZones  = 1;
        integer(INT32), allocatable     :: zormap(:);
        integer(INT64)                  :: blockSizeValue = 100;
        character(len = :), allocatable :: interfNodesFileName;
    contains
        procedure                       :: loadInputParams;
        procedure                       :: isNodMin        
    end type DDDMS_InputParams  
    
    !┌───────────────────────────────────────────────────────────────┐
    !│DDDMS_INPUTPARAMS USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└───────────────────────────────────────────────────────────────┘
    interface DDDMS_InputParams
        module procedure NewDDDMS_InputParams;       ! ADD CONSTRUCTOR TO DDDMS_INPUTPARAMS GENERIC INTERFACE
    end interface DDDMS_InputParams

    !╔════════════╗
    !║DDDMS_MATRIX║
    !╚════════════╝
    !┌──────────────────────────────────────────────┐
    !│DDDMS_MATRIX USER-DEFINED DATA TYPE DEFINITION│
    !└──────────────────────────────────────────────┘
    type :: DDDMS_Matrix
        real(REAL64),   allocatable :: A00(:, :);
        real(REAL64),   allocatable :: A0L(:, :);
        real(REAL64),   allocatable :: AL0(:, :);
        real(REAL64),   allocatable :: ALL(:, :);
        real(REAL64),   allocatable :: B00(:, :);
        real(REAL64),   allocatable :: B0L(:, :);
        real(REAL64),   allocatable :: BL0(:, :);
        real(REAL64),   allocatable :: BLL(:, :);
        real(REAL64),   allocatable :: DL(:, :);
        real(REAL64),   allocatable :: CB(:);
        real(REAL64),   allocatable :: Y(:);                     
        real(REAL64),   allocatable :: XB(:);
        real(REAL64),   allocatable :: ULI(:);
        real(REAL64),   allocatable :: CoordsNodesInZone(:, :);   
        real(REAL64),   allocatable :: resVec(:);
        integer(INT32), allocatable :: BCsType(:, :);
    end type DDDMS_Matrix
    
    !┌────────────────────────────────────────────────────┐
    !│MATRIX USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────┘
    interface DDDMS_Matrix
        module procedure NewDDDMS_Matrix;       ! ADD CONSTRUCTOR TO DDDMS_MATRIX GENERIC INTERFACE
    end interface DDDMS_Matrix

    !╔═══════════════════╗
    !║DDDMS_DATACONTAINER║
    !╚═══════════════════╝
    !┌─────────────────────────────────────────────────────┐
    !│DDDMS_DATACONTAINER USER-DEFINED DATA TYPE DEFINITION│
    !└─────────────────────────────────────────────────────┘
    type :: DDDMS_DataContainer
        real(REAL64),       allocatable :: U_InterfaceSolution(:);        
        integer(INT64),     allocatable :: IndependenInterfaceNodesArray(:, :);
        integer(INT64),     allocatable :: InterfaceNP(:, :);
        type(DDDMS_Matrix), allocatable :: ZonesData(:);
        real(REAL64),       allocatable :: SGlob(:, :);
        real(REAL64),       allocatable :: gGlob(:);
!    contains
        
    end type DDDMS_DataContainer
    
    !┌─────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATACONTAINER USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└─────────────────────────────────────────────────────────────────┘
    interface DDDMS_DataContainer
        module procedure NewDDDMS_DataContainer;       ! ADD CONSTRUCTOR TO DDDMS_DATACONTAINER GENERIC INTERFACE
    end interface DDDMS_DataContainer

contains
    
    !┌─────────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATACONTAINER USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└─────────────────────────────────────────────────────────────────────┘
    type(DDDMS_DataContainer) function NewDDDMS_DataContainer(self)
    
        implicit none;
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_DataContainer) :: self;
        
        !********************************************
        !BODY OF THE PROGRAM
        !********************************************    
        write(OUTPUT_UNIT, '("DDDMS_DataContainer constructor was called.")');
        flush OUTPUT_UNIT;
        
        allocate(self%gGlob(10));
        self%gGlob(:) = 1.0;
        
    end function NewDDDMS_DataContainer
    
    

    !┌────────────────────────────────────────────────────────────────────┐
    !│DDDMS_DATACONTAINER USER-DEFINED DATA TYPE DESTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────┘
    !elemental subroutine DelDDDMS_DataContainer(self)
    !
    !    implicit none;
    !    !********************************************
    !    !DECLARATIVE ZONE
    !    !********************************************
    !    class(DDDMS_DataContainer) :: self;
    !    
    !    !********************************************
    !    !BODY OF THE PROGRAM
    !    !********************************************    
    !    
    !end subroutine DelDDDMS_DataContainer

    !┌──────────────────────────────────────────────────────────────┐
    !│DDDMS_MATRIX USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└──────────────────────────────────────────────────────────────┘
    type(DDDMS_Matrix) function NewDDDMS_Matrix()
    
        implicit none;
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        
        !********************************************
        !BODY OF THE PROGRAM
        !********************************************        
       
    end function NewDDDMS_Matrix

    !┌───────────────────────────────────────────────────────────────────┐
    !│DDDMS_INPUTPARAMS USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└───────────────────────────────────────────────────────────────────┘
    type(DDDMS_InputParams) function NewDDDMS_InputParams()
    
        implicit none;
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        
        !********************************************
        !BODY OF THE PROGRAM
        !********************************************        
       
    end function NewDDDMS_InputParams

    !┌────────────────────────────────────────┐
    !│LOADINPUTPARAMS PROCEDURE IMPLEMENTATION│
    !└────────────────────────────────────────┘
    subroutine loadInputParams(self, inputPathFileName, inputBlockFileName)
    
        implicit none;
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_InputParams) :: self;
        character(len = *)       :: inputPathFileName;
        character(len = *)       :: inputBlockFileName;
        character(len = 100)     :: currentLine;
        integer(INT64)           :: file_size_1;
        integer(INT64)           :: file_size_2;
        integer(INT32)           :: status,nzones,ZoneID,ZorderID,ij;
        logical                  :: file_exists_1; 
        logical                  :: file_exists_2; 

        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        inquire( FILE = inputPathFileName , EXIST = file_exists_1 ); !CHECK IF THE INPUT FILE "INPUTPATHFILENAME" EXISTS.
        inquire( FILE = inputPathFileName , SIZE = file_size_1 );    !CHECK IF THE INPUT FILE "INPUTBLOCKFILENAME" IS EMPTY.

        inquire( FILE = inputBlockFileName , EXIST = file_exists_2 ); !CHECK IF THE INPUT FILE "INPUTPATHFILENAME" EXISTS.
        inquire( FILE = inputBlockFileName , SIZE = file_size_2 );    !CHECK IF THE INPUT FILE "INPUTBLOCKFILENAME" IS EMPTY.

        if( (file_exists_1 .AND. (file_size_1 /= 0) ) .AND. (file_exists_2 .AND. (file_size_2 /= 0) ) ) then
 
            !┌────────────────────────────────────────────────────┐
            !│OPEN FILE INPUTPATHFILENAME TO LOAD INPUT PARAMETERS│
            !└────────────────────────────────────────────────────┘
            open(UNIT = 10, ACTION = 'read', FILE = inputPathFileName, STATUS = 'old');

            !┌───────────────────────────────────────────┐
            !│READ THE VALUE INDICATING THE ANALYSIS TYPE│
            !└───────────────────────────────────────────┘
            read(10, '(I6)', IOSTAT = status) self%analysType;
            if (status /= 0) then
                print*, "Failed reading analysis type in file : ", inputPathFileName;
                print*, "Errore code: ", status;
                pause
                stop;
            end if

            !┌───────────────────────────────────────────────────┐
            !│READ THE VALUE INDICATING THE G MATRIX SCALE FACTOR│
            !└───────────────────────────────────────────────────┘
            read(10, '(E18.8)', IOSTAT = status) self%gMatScaleFact;
            if (status /= 0) then
                print*, "Failed reading G matrix scale factor in file : ", inputPathFileName;
                print*, "Errore code: ", status;
                pause
                stop;
            end if
            
            !┌──────────────────────────────────────────────────────────┐
            !│READ THE VALUE INDICATING THE NUMBER OF ZONES IN THE MODEL│
            !└──────────────────────────────────────────────────────────┘
            read(10, '(I6)', IOSTAT = status) self%numberOfZones;
            nzones=self%numberOfZones;
            if (status /= 0) then
                print*, "Failed reading number of zones in file : ", inputPathFileName;
                print*, "Errore code: ", status;
                pause
                stop;
            end if

            !┌──────────────────────────────────────────────────────────────────────┐
            !│READ THE NAME OF THE FILE CONTAINING DATA ABOUT NODES ON THE INTERFACE│
            !└──────────────────────────────────────────────────────────────────────┘
            read(10, '(A)', IOSTAT = status) currentLine;
            if (status /= 0) then
                print*, "Failed reading number of zones in file : ", inputPathFileName;
                print*, "Errore code: ", status;
                pause
                stop;
            end if
            
            allocate( self%interfNodesFileName, STAT = status, SOURCE = trim(currentLine) );
            if (status /= 0) then
                print*, "Failed allocation of self%interfNodesFileName!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if  !CLOSE IF STATEMENT ON ALLOCATE STATUS FOR LOADER OBJECT.
            self%interfNodesFileName = trim(currentLine);                    
            
            allocate( self%zormap(self%numberOfZones), STAT = status);
            if (status /= 0) then
                print*, "Failed allocation of self%zormap!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
            do ij = 1,nzones
                read(10, fmt="(3 (I8))",IOSTAT = status) ZoneID, ZorderID;
                self%zormap(ZorderID)=ZoneID;
                read(10,*)
                read(10,*)
                read(10,*)
            enddo
            
            close(10);       

            !┌─────────────────────────────────────────────────────┐
            !│OPEN FILE INPUTBLOCKFILENAME TO LOAD INPUT PARAMETERS│
            !└─────────────────────────────────────────────────────┘
            open(UNIT = 10, ACTION = 'read', FILE = inputBlockFileName, STATUS = 'old');
            do
                read(10, '(a)', IOSTAT = status) currentLine;
                if (status == iostat_end) then
                    exit; ! END OF FILE REACHED.
                endif 
                if( index( trim(currentLine), 'BLOCK SIZE') /= 0 ) then !CHECK FOR FILE CONTAINING DUMP BLOCK SIZE PARAMETER
                    
                    read( currentLine( ( index( trim( currentLine ), ':' ) + 1): ), '(I11)', IOSTAT = status ) self%blockSizeValue;
                    if (status /= 0) then
                        print*, "Failed reading block size to dump system in file : ", inputBlockFileName;
                        print*, "Errore code: ", status;
                        pause
                        stop;
                    end if                   
                    
                endif
               
            end do !END OF DO_LOOP STATEMENT GOES TROUGH FILE CONTAINING BLOCK SIZE INFORMATION
        
            close(10);            
        endif ! END IF STATEMENT OVER CHECK IF FILES EXIST AND ARE NOT EMPTY
   
    end subroutine loadInputParams
!
!********************************************************************************************
    logical function isNodMin(self,dataContainer,intfnod)
!********************************************************************************************
!   Objective: To check if a given interface node corresponds to the lower node ID as per  
!              the node pair specification in the interface nodes file
!********************************************************************************************
!
        class(DDDMS_InputParams)                :: self
        class(DDDMS_DataContainer)              :: dataContainer
        integer(INT32)                          :: inp,mnod,intfnod;
    
        isNodMin=.false.
        do inp=1,size(dataContainer%InterfaceNP(:,1))
            mnod=min(dataContainer%InterfaceNP(inp,1),dataContainer%InterfaceNP(inp,2))            
            if (intfnod.eq.mnod) then
                isNodMin=.true.
            endif
        enddo    
    
    end function isNodMin
        
end module DataStructure_Module

!********************************************************************************************