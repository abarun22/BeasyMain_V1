module Utility_Module
    use iso_fortran_env;
    use DataStructure_Module;
    implicit none;
    
contains
  
    !┌──────────────────────────────────────────────────────────────────────┐
    !│PROCEDURE TO PRINT OUT DATA CONTAINED IN A MATRIX OF REAL COEFFICIENTS│
    !└──────────────────────────────────────────────────────────────────────┘    
    subroutine Utility_OutputDataMatrix(fileName, dataToPrint)

        implicit none;    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        character(len = *), intent(in) :: fileName;
        character(len = 4)             :: seqstring; 
        class(*), intent(in)           :: dataToPrint(:, :);
        integer(INT32)                 :: fid;
        integer(INT64)                 :: k;
        integer(INT64)                 :: j;
        integer(INT64)                 :: i;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        select type(dataToPrint)        ! APB:  Polymorphism with type bound Procedures
            
        type is ( real(REAL64) )            
            open( newunit = fid, ACTION = 'write', FILE = fileName, status = 'unknown' );       
            do j = 1, size( dataToPrint, 1)
                do k = 1, size( dataToPrint, 2)
                    write(fid, '(E12.6,X)', ADVANCE = 'NO') dataToPrint(j, k);
                end do
                write(fid, *);
            end do                        
            close( fid );
            
        type is ( integer(INT32) )            
            open( newunit = fid, ACTION = 'write', FILE = fileName, status = 'unknown' );       
            do j = 1, size( dataToPrint, 1)
                do k = 1, size( dataToPrint, 2)
                    write(fid, '(I6,X)', ADVANCE = 'NO') dataToPrint(j, k);
                end do
                write(fid, *);
            end do                        
            close( fid );
            
        type is ( integer(INT64) )            
            open( newunit = fid, ACTION = 'write', FILE = fileName, status = 'unknown' );       
            do j = 1, size( dataToPrint, 1)
                do k = 1, size( dataToPrint, 2)
                    write(fid, '(I6,X)', ADVANCE = 'NO') dataToPrint(j, k);
                end do
                write(fid, *);
            end do                        
            close( fid );

            
        end select
                
    end subroutine Utility_OutputDataMatrix
    
    !┌──────────────────────────────────────────────────────────────────────┐
    !│PROCEDURE TO PRINT OUT DATA CONTAINED IN A VECTOR OF REAL COEFFICIENTS│
    !└──────────────────────────────────────────────────────────────────────┘        
    subroutine Utility_OutputDataVector(fileName, dataToPrint)

        implicit none;
        !********************************************
        !DECLARATIVE ZONE
        !********************************************
        character(len = *), intent(in) :: fileName;
        character(len = 4)             :: seqstring; 
        real(REAL64), intent(in)       :: dataToPrint(:);
        integer(INT32)                 :: fid;
        integer(INT64)                 :: k;
        integer(INT64)                 :: j;
        integer(INT64)                 :: i;
        
        !********************************************
        !BODY OF THE PROGRAM
        !********************************************    
        open( newunit = fid, ACTION = 'write', FILE = fileName, status = 'unknown' );       
        do j = 1, size( dataToPrint, 1)
            write(fid, "(E12.6,X)") dataToPrint(j);
        end do                        
        close( fid );
        
    end subroutine Utility_OutputDataVector

    !┌─────────────────────────────────────────────────────┐
    !│PROCEDURE TO COLLECT DATA FROM COMMAND LINE ARGUMENTS│
    !└─────────────────────────────────────────────────────┘    
    subroutine Utility_CommandParameters(inputSolverPathName, &
                                         inputModelFileName,  &
                                         inputPathFileName,   &
                                         inputBlockFileName,  &
                                         batchFileCommands,   &
                                         HPC_Arch)
    
        implicit none;
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        character(len = 200)              :: inputSolverPathName;      !==>TO DELETE?
        character(len = 200)              :: inputModelFileName;       !==>TO DELETE?
        character(len = 200)              :: inputPathFileName;        !==>TO DELETE?
        character(len = 200)              :: inputBlockFileName;       !==>TO DELETE?
        character(len = 200)              :: currentLine;              !==>TO DELETE?
        character(len = 100)              :: currCommandArg;           !==>TO DELETE?
        character(len = 100), allocatable :: batchFileCommands(:);
        integer(INT32)                    :: HPC_Arch;
        integer(INT64)                    :: k;
        integer(INT64)                    :: j;
        integer(INT64)                    :: i;
        integer(INT64)                    :: file_size;
        integer(INT64)                    :: count;
        integer(INT32)                    :: status;
        integer(INT32)                    :: indexPath;
        integer(INT32)                    :: fid;
        logical                           :: file_exists;        
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        count = command_argument_count()
        if( count > 1 )then
            
            do i = 1,count
                call get_command_argument( i, currCommandArg);
                if( len_trim(currCommandArg) == 0 ) exit;
            
                if( index( trim(currCommandArg), '-pf=') /= 0 ) then !CHECK FOR FILE CONTAINING INPUT PARAMETERS
                    
                    indexPath = scan( currCommandArg, "=" );
            
                    !┌──────────────────────────────────────────────────────┐
                    !│CHECK IF THE ENTERED FILE NAME "CURRCOMMANDARG" EXISTS│
                    !└──────────────────────────────────────────────────────┘
                    inquire( FILE = currCommandArg( ( indexPath + 1): ) , EXIST = file_exists );  !CHECK IF THE INPUT FILE CURRCOMMANDARG EXISTS.
                    inquire( FILE = currCommandArg( ( indexPath + 1): ) , SIZE  = file_size );    !CHECK IF THE INPUT FILE CURRCOMMANDARG IS EMPTY.
            
                    if( file_exists .AND. (file_size /= 0) ) then
                        open(NEWUNIT = fid, ACTION = 'read', FILE = currCommandArg( ( indexPath + 1): ), STATUS = 'old');
                        read(fid, '(A)', IOSTAT = status) inputSolverPathName;
                        read(fid, '(A)', IOSTAT = status) inputModelFileName;
                        read(fid, '(A)', IOSTAT = status) inputPathFileName;
                        read(fid, '(A)', IOSTAT = status) inputBlockFileName;
                        read(fid, '(I)', IOSTAT = status) HPC_Arch;
                        close(fid);       
                    endif
              
                elseif( index( trim(currCommandArg), '-bf=') /= 0 ) then !CHECK FOR FILE CONTAINING BATCH COMMANDS
                    
                    indexPath = scan( currCommandArg, "=" );
                    
                    !┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
                    !│CHECK IF THE ENTERED FILE NAME "CURRCOMMANDARG" EXISTS AND COUNTS HOW MANY LINES THERE ARE INSIDE IT│
                    !└────────────────────────────────────────────────────────────────────────────────────────────────────┘
                    inquire( FILE = currCommandArg( ( indexPath + 1): ) , EXIST = file_exists );  !CHECK IF THE INPUT FILE CURRCOMMANDARG EXISTS.
                    inquire( FILE = currCommandArg( ( indexPath + 1): ) , SIZE  = file_size );    !CHECK IF THE INPUT FILE CURRCOMMANDARG IS EMPTY.
            
                    if( file_exists .AND. (file_size /= 0) ) then
                        open( NEWUNIT = fid, ACTION = 'read', FILE = currCommandArg( ( indexPath + 1): ), STATUS = 'old');
            
                        count = 0;
                        do
                            read(fid, '(A)', IOSTAT = status) currentLine;
                            if (status == IOSTAT_END) then
                                exit; ! END OF FILE REACHED.ABM
                            end if                
                        
                            count = count + 1;
                        end do  !END OF DO STATEMENT TO THE END OF THE FILE SELF%inputFilePathName    
            
                        close(fid); 
                        
                        !┌──────────────────────────────────────────────────┐
                        !│ALLOCATE DATA FOR DATA STRUCTURE BATCHFILECOMMANDS│
                        !└──────────────────────────────────────────────────┘
                        allocate( batchFileCommands(count), STAT = status );
                        if (status /= 0) then
                            print*, "Failed allocation of batchFileCommands array!";
                            print*, "Errore code: ", status;
                            pause;
                            stop;
                        end if  
            
                        batchFileCommands(:) = '*';
                        !┌──────────────────────────────────────────────────────────┐
                        !│FILL BATCHFILECOMMANDS WITH COMMANDS FROM INPUT BATCH FILE│
                        !└──────────────────────────────────────────────────────────┘
                        open( NEWUNIT = fid, ACTION = 'read', FILE = currCommandArg( ( indexPath + 1): ), STATUS = 'old');        
                        count = 1;
                        do
                            read(fid, '(A)', IOSTAT = status) currentLine;
                            if (status == IOSTAT_END) then
                                exit; ! END OF FILE REACHED.ABM
                            end if                
                        
                            batchFileCommands(count) = currentLine;
                            count = count + 1;
                        end do  !END OF DO STATEMENT TO THE END OF THE FILE SELF%inputFilePathName    
            
                        close(fid); 
                     
                    endif !END OF IF STATEMENT ON CHECKING IF INPUT BATCH FILE EXISTS AND IS NOT EMPTY
                    
                endif !END OF IF STATEMENT ABOUT CHECKING FOR FILE CONTAINING INPUT PARAMETERS
            
            end do !END OF LOOP STATEMENT OVER NUMBER OF COMMAND LINE ARGUMENTS
        
        else

            !╔════════════════════════════════════════════╗
            !║BODY OF THE MAIN PROGRAM inputSolverPathName║
            !╚════════════════════════════════════════════╝
            
            !┌──────────────────────────────────────────┐
            !│ENTER PATHNAME CONTAINING THE BEASY SOLVER│
            !└──────────────────────────────────────────┘
            write(OUTPUT_UNIT, '("Enter pathname containing the beasy solver...:")', ADVANCE = "NO");
            read(OUTPUT_UNIT, *)inputSolverPathName;
            write(OUTPUT_UNIT, *)"Beasy solver pathname entered is...:", inputSolverPathName; 
            
            !┌──────────────────────────────────────────────────────┐
            !│ENTER FILE PATHNAME CONTAINING ALL INPUT FILES TO LOAD│
            !└──────────────────────────────────────────────────────┘
            write(OUTPUT_UNIT, '("Enter input file pathname containing model informations...:")', ADVANCE = "NO");
            read(OUTPUT_UNIT, *)inputModelFileName;
            write(OUTPUT_UNIT, *)"input file pathname entered is...:", inputModelFileName; 
            
            !┌──────────────────────────────────────────────────────┐
            !│ENTER FILE PATHNAME CONTAINING ALL INPUT FILES TO LOAD│
            !└──────────────────────────────────────────────────────┘
            write(OUTPUT_UNIT, '("Enter file pathname containing all input files to load...:")', ADVANCE = "NO");
            read(OUTPUT_UNIT, *)inputPathFileName;
            write(OUTPUT_UNIT, *)"folder pathname entered is...:", inputPathFileName; 
            
            !┌────────────────────────────────────────────────────┐
            !│ENTER FILE PATHNAME CONTAINING DUMP SYSTEM SIZE USED│
            !└────────────────────────────────────────────────────┘
            write(OUTPUT_UNIT, '("Enter file pathname containing dump system size used...:")', ADVANCE = "NO");
            read(OUTPUT_UNIT, *)inputBlockFileName;
            write(OUTPUT_UNIT, *)"folder pathname entered is...:", inputBlockFileName; 
            
            !┌─────────────────────────────────────────────┐
            !│SELECT HPC ARCHITECTURE WHERE DDM WILL BE RUN│
            !└─────────────────────────────────────────────┘
            write(OUTPUT_UNIT, '("Select HPC architecture...:")');

            write(OUTPUT_UNIT, '("Shared Memory arch...............==> 1 ")');
            write(OUTPUT_UNIT, '("Distributed Memory arch..........==> 2 ")');
            read(OUTPUT_UNIT, *)HPC_Arch;
            write(OUTPUT_UNIT, *)"HPC arch...:", HPC_Arch;   
            
        endif ! END OF IF STATEMENT ABOUT INPUT PARAMETERS
        
    end subroutine Utility_CommandParameters

    !┌────────────────────────────────────────────────────────────────────┐
    !│PROCEDURE TO COLLECT DATA FROM COMMAND TO PERFORME BOUNDARY SOLUTION│
    !└────────────────────────────────────────────────────────────────────┘ 
    function Utility_ZoneBoundarySolution( inputStringLine, inputParamsData ) result(zonesBoundSolArray)
    
        implicit none;
        
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        character(len = *), intent(in) :: inputStringLine;
        character(len = :), allocatable:: param;             
        class(DDDMS_InputParams)       :: inputParamsData;
        integer(INT32), allocatable    :: zonesBoundSolArray(:);
        integer(INT32)                 :: indexPath;
        integer(INT32)                 :: indexPath2;
        integer(INT32)                 :: status;
        integer(INT32)                 :: i;
        integer(INT32)                 :: numericParam1;
        integer(INT32)                 :: numericParam2;   
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        indexPath = scan( trim(inputStringLine), "," );

        param = inputStringLine( (indexPath + 1):len_trim(inputStringLine) );
        read(param(1:), '(I)', IOSTAT = status) numericParam1;

        !┌─────────────────────────────┐
        !│OPTION ALL ZONES: CALC_BS,ALL│
        !└─────────────────────────────┘        
        if( param == "ALL" ) then
            
            allocate( zonesBoundSolArray( inputParamsData%numberOfZones ), STAT = status );
            if (status /= 0) then
                print*, "Failed allocation of DDM zonesBoundSolArray array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                zonesBoundSolArray = 0;
            end if
            
            zonesBoundSolArray = [ (i, i = 1, inputParamsData%numberOfZones ) ];

        !┌─────────────────────────────┐
        !│OPTION SINGLE ZONE: CALC_BS,N│
        !└─────────────────────────────┘        
        elseif( ( numericParam1 >= 1 )                             .AND. &
                ( numericParam1 <= inputParamsData%numberOfZones ) .AND. &
                ( scan( param, ",") == 0) ) then
            
            allocate( zonesBoundSolArray( 1 ), STAT = status );
            if (status /= 0) then
                print*, "Failed allocation of DDM zonesBoundSolArray array!";
                print*, "Errore code: ", status;
                pause;
                stop;
            else
                zonesBoundSolArray = 0;
            end if
            
            zonesBoundSolArray = [ numericParam1 ];

        !┌──────────────────────────────────────────┐
        !│OPTION RANGE: CALC_BS,N-M -- (WITH N <= M)│
        !└──────────────────────────────────────────┘        
        elseif( scan( trim(param), "-" ) /= 0 ) then
                    
            indexPath2 = scan( trim(param), "-" );
            read( param( 1:(indexPath2 - 1) ),               '(I)', IOSTAT = status ) numericParam1;
            read( param( (indexPath2 + 1):len_trim(param) ), '(I)', IOSTAT = status ) numericParam2;
            
            if( (numericParam1 <= numericParam2) .AND. &
                (numericParam1 >= 1)             .AND. &
                (inputParamsData%numberOfZones >= numericParam2) ) then
                
                allocate( zonesBoundSolArray( numericParam2 - numericParam1 + 1 ), STAT = status );
                if (status /= 0) then
                    print*, "Failed allocation of DDM zonesBoundSolArray array!";
                    print*, "Errore code: ", status;
                    pause;
                    stop;
                else
                    zonesBoundSolArray = 0;
                end if
            
                zonesBoundSolArray = [ (i, i = numericParam1, numericParam2 ) ];
                
            else
                pause;
                stop;
                
            end if

        !┌─────────────────────────────────────────┐
        !│OPTION LIST: CALC_BS,N,M -- (WITH N <= M)│
        !└─────────────────────────────────────────┘        
         elseif( scan( trim(param), "," ) /= 0 ) then
                    
            indexPath2 = scan( trim(param), "," );
            read( param( 1:(indexPath2 - 1) ),               '(I)', IOSTAT = status ) numericParam1;
            read( param( (indexPath2 + 1):len_trim(param) ), '(I)', IOSTAT = status ) numericParam2;
            
            if( (numericParam1 <= numericParam2) .AND. &
                (numericParam1 >= 1)             .AND. &
                (inputParamsData%numberOfZones >= numericParam2) ) then
                
                allocate( zonesBoundSolArray( 2 ), STAT = status );
                if (status /= 0) then
                    print*, "Failed allocation of DDM zonesBoundSolArray array!";
                    print*, "Errore code: ", status;
                    pause;
                    stop;
                else
                    zonesBoundSolArray = 0;
                end if
            
                zonesBoundSolArray(1) = numericParam1;
                zonesBoundSolArray(2) = numericParam2;
                
            else
                pause;
                stop;
                    
            end if
                       
        else
            write(OUTPUT_UNIT, '("Parameter was not recognised!")');
            write(OUTPUT_UNIT, '("Please, check it!")');
                
        end if      
        
    end function Utility_ZoneBoundarySolution
 
    !┌──────────────────────────────────────────────────────────┐
    !│PROCEDURE TO CONVERT A GENERIC STRINT TO UPPER CASE STRING│
    !└──────────────────────────────────────────────────────────┘ 
    subroutine To_upper( str )
        character(*) :: str;
        integer      :: i;
 
        do i = 1, len( trim(str) )
            select case( str(i:i) )
                case("a":"z")
                str(i:i) = achar( iachar( str( i:i ) ) - 32 )
            end select
        end do 
                 
    end subroutine To_upper
    
    !┌──────────────────────────────────────────────────────────────────┐
    !│PROCEDURE TO PRINT OUT NODES DEFORMED COORDINATES DATA TO PARAVIEW│
    !└──────────────────────────────────────────────────────────────────┘    
    subroutine Fortran2VTK( fileName, dataToPrint )
    
        implicit none;    
        !┌────────────────┐
        !│DECLARATIVE ZONE│
        !└────────────────┘
        character(len = *), intent(in) :: fileName;
        integer(INT32)                 :: fid;
        real(REAL64),       intent(in) :: dataToPrint(:, :);
        integer(INT64)                 :: i;
        
        !┌───────────────────┐
        !│BODY OF THE PROGRAM│
        !└───────────────────┘
        open( NEWUNIT = fid, ACTION = 'write', FILE = fileName, STATUS = 'unknown' );       
            write(fid, FMT = '("# vtk DataFile Version 2.0")');                         
            write(fid, FMT = '("InputFile4 Output")');
            write(fid, FMT = '("ASCII")');
            write(fid, FMT = '("DATASET UNSTRUCTURED_GRID")');
            write(fid, FMT = '("POINTS",I5,X,"float")')size(dataToPrint, 1);
            
            do i = 1,size(dataToPrint, 1)
                write(fid, FMT = '(3(E12.6,X))')dataToPrint(i, 2), dataToPrint(i, 3), dataToPrint(i, 4);    
            end do

            write(fid, FMT = '("CELLS",2(I5,X))')size(dataToPrint, 1), (2*size(dataToPrint, 1));
            do i = 1,size(dataToPrint, 1)
                write(fid, FMT = '(2(I5,X))')1, (i - 1);    
            end do

            write(fid, FMT = '("CELL_TYPES",I5,X)')size(dataToPrint, 1);
            do i = 1,size(dataToPrint, 1)
                write(fid, FMT = '(I5)')1;    
            end do
            
        close( fid );        
                 
    end subroutine Fortran2VTK

    
end module Utility_Module
    