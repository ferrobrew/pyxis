@echo off
REM Set up the environment for MSVC
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" >NUL

REM List of class names
set classes=Derived_A0_B0_0 Derived_A0_B0_1 Derived_A0_B1_0 Derived_A0_B1_1 Derived_A1_B0_0 Derived_A1_B0_1 Derived_A1_B1_0 Derived_A1_B1_1

REM Output file
set output_file=output.txt

REM Clear the output file
echo. > %output_file%

REM Loop through each class and dump the layout
for %%c in (%classes%) do (
    REM Delete all PDBs
    del *.pdb

    REM Compile the code with the class layout flag
    echo =============================================== >> %output_file%
    echo Class layout for %%c >> %output_file%
    echo =============================================== >> %output_file%
    cl /Zi /c /d1reportSingleClassLayout%%c /EHsc test.cpp >> %output_file%
    echo. >> %output_file%
)

echo Class layouts dumped to %output_file%
