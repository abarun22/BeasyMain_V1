module SolverController_Module
    use, intrinsic :: iso_fortran_env;
    use DataLoader_Module;
    use LocalSchurComplementCalculator_Module;
    use GlobalSchurComplementAssembler_Module;
    use InterfaceSolutionCalculator_Module;
    use BoundarySolutionCalculator_Module;
    implicit none;
    private;
    public DDDMS_SolverController;
    
    !╔══════════════════════╗
    !║DDDMS_SOLVERCONTROLLER║
    !╚══════════════════════╝
    !┌────────────────────────────────────────────────────────┐
    !│DDDMS_SOLVERCONTROLLER USER-DEFINED DATA TYPE DEFINITION│
    !└────────────────────────────────────────────────────────┘
    type :: DDDMS_SolverController
        integer :: solverType;
    contains
        procedure :: getDataLoader;
        procedure :: getLocalSchurComplementCalculator;
        procedure :: getGlobalSchurComplementAssembler;
        procedure :: getInterfaceSolutionCalculator;
        procedure :: getBoundarySolutionCalculator;
        !procedure :: getResultsExporter;
        !final     :: DelDDDMS_SolverController;
    end type DDDMS_SolverController

    !┌────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SOLVERCONTROLLER USER-DEFINED DATA TYPE CONSTRUCTOR DEFINITION│
    !└────────────────────────────────────────────────────────────────────┘
    interface DDDMS_SolverController
        module procedure NewDDDMS_SolverController;       ! ADD CONSTRUCTOR TO DDDMS_SOLVERCONTROLLER GENERIC INTERFACE
    end interface DDDMS_SolverController

contains
    
    !┌────────────────────────────────────────────────────────────────────────┐
    !│DDDMS_SOLVERCONTROLLER USER-DEFINED DATA TYPE CONSTRUCTOR IMPLEMENTATION│
    !└────────────────────────────────────────────────────────────────────────┘
    type(DDDMS_SolverController) function NewDDDMS_SolverController(self, solverTypeInput)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SolverController) :: self;
        integer                       :: solverTypeInput;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        NewDDDMS_SolverController%solverType = solverTypeInput;
        print*, "Faccia di merda!";
        
    end function NewDDDMS_SolverController
   
    !┌──────────────────────────────────────┐
    !│GETDATALOADER PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────────┘
    function getDataLoader(self, pathFileName) result(dataLoader)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SolverController) :: self;
        class(*), allocatable         :: dataLoader;
        integer                       :: status;
        character(len = *)            :: pathFileName;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        allocate( dataLoader, STAT = status, SOURCE = DDDMS_DataLoaderFromFile( pathFileName ) );
        if (status /= 0) then
            print*, "Failed allocation of DDM loader!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if  
        
    end function getDataLoader

    !┌──────────────────────────────────────────────────────────┐
    !│GETLOCALSCHURCOMPLEMENTCALCULATOR PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────────────────────────────┘
    function getLocalSchurComplementCalculator(self, HPC_Arch) result(LSC_Calculator)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SolverController) :: self;
        class(*), allocatable         :: LSC_Calculator;
        integer(INT32)                :: status;
        integer(INT32)                :: HPC_Arch;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
        !┌───────────────────────────────────┐
        !│SHARED MEMORY HARDWARE ARCHITECTURE│
        !└───────────────────────────────────┘        
        if( HPC_Arch == 1 )then
            allocate( LSC_Calculator, STAT = status, SOURCE = DDDMS_SMLocalSchurComplementCalculator( ) );
            if (status /= 0) then
                print*, "Failed allocation of DDM LSC_Calculator!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
        !┌────────────────────────────────────────┐
        !│DISTRIBUTED MEMORY HARDWARE ARCHITECTURE│
        !└────────────────────────────────────────┘        
        elseif( HPC_Arch == 2 )then
            
        end if
                
    end function getLocalSchurComplementCalculator

    !┌──────────────────────────────────────────────────────────┐
    !│GETGLOBALSCHURCOMPLEMENTASSEMBLER PROCEDURE IMPLEMENTATION│
    !└──────────────────────────────────────────────────────────┘
    function getGlobalSchurComplementAssembler(self) result(GSC_Assembler)
    
        implicit none;
        
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        class(DDDMS_SolverController)                            :: self;
        class(DDDMS_GlobalSchurComplementAssembler), allocatable :: GSC_Assembler;
        integer(INT32)                                           :: status;
        
        !********************************************
        !BODY OF THE PROGRAM
        !********************************************  
        allocate( GSC_Assembler, STAT = status, SOURCE = DDDMS_GlobalSchurComplementAssembler( ) );
        if (status /= 0) then
            print*, "Failed allocation of DDM GSC_Assembler!";
            print*, "Errore code: ", status;
            pause;
            stop;
        end if
                
    end function getGlobalSchurComplementAssembler
    
    !┌───────────────────────────────────────────────────────┐
    !│GETINTERFACESOLUTIONCALCULATOR PROCEDURE IMPLEMENTATION│
    !└───────────────────────────────────────────────────────┘
    function getInterfaceSolutionCalculator(self, HPC_Arch) result(IS_Calculator)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SolverController) :: self;
        class(*), allocatable         :: IS_Calculator;
        integer(INT32)                :: status;
        integer(INT32)                :: HPC_Arch;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
        !┌───────────────────────────────────┐
        !│SHARED MEMORY HARDWARE ARCHITECTURE│
        !└───────────────────────────────────┘        
        if( HPC_Arch == 1 )then
            allocate( IS_Calculator, STAT = status, SOURCE = DDDMS_SMInterfaceSolutionCalculator( ) );
            if (status /= 0) then
                print*, "Failed allocation of DDM Interface Calculator!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
        !┌────────────────────────────────────────┐
        !│DISTRIBUTED MEMORY HARDWARE ARCHITECTURE│
        !└────────────────────────────────────────┘        
        elseif( HPC_Arch == 2 )then
            
        end if
                
    end function getInterfaceSolutionCalculator

    !┌───────────────────────────────────────────────────────┐
    !│getBoundarySolutionCalculator PROCEDURE IMPLEMENTATION│
    !└───────────────────────────────────────────────────────┘
    function getBoundarySolutionCalculator(self, HPC_Arch) result(BS_Calculator)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        class(DDDMS_SolverController) :: self;
        class(*), allocatable         :: BS_Calculator;
        integer(INT32)                :: status;
        integer(INT32)                :: HPC_Arch;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        
        !┌───────────────────────────────────┐
        !│SHARED MEMORY HARDWARE ARCHITECTURE│
        !└───────────────────────────────────┘        
        if( HPC_Arch == 1 )then
            allocate( BS_Calculator, STAT = status, SOURCE = DDDMS_SMBoundarySolutionCalculator( ) );
            if (status /= 0) then
                print*, "Failed allocation of DDM Boundary Calculator!";
                print*, "Errore code: ", status;
                pause;
                stop;
            end if
            
        !┌────────────────────────────────────────┐
        !│DISTRIBUTED MEMORY HARDWARE ARCHITECTURE│
        !└────────────────────────────────────────┘        
        elseif( HPC_Arch == 2 )then
            
        end if
                
    end function getBoundarySolutionCalculator

end module SolverController_Module