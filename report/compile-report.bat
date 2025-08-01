@echo off
REM LaTeX Report Compilation Script for Windows
REM Compiles the lab report from LaTeX source to PDF

echo === Call of Duty Weapon Knowledgebase Report Compilation ===
echo.

REM Check if pdflatex is available
where pdflatex >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: pdflatex not found. Please install a LaTeX distribution.
    echo Recommended: MiKTeX for Windows
    pause
    exit /b 1
)

echo Compiling lab-report.tex to PDF...
echo.

REM Compile the LaTeX document
REM Run twice to ensure proper cross-references and table of contents
echo Attempting compilation with pdflatex...
pdflatex -interaction=nonstopmode lab-report.tex
if %ERRORLEVEL% NEQ 0 (
    echo First compilation had issues, trying again...
    pdflatex -interaction=nonstopmode lab-report.tex
)

REM Check if compilation was successful
if exist "lab-report.pdf" (
    echo ✅ Compilation successful!
    echo 📄 Output: lab-report.pdf
    echo.
    echo The report includes:
    echo   - Beautiful front page matching the template
    echo   - Complete project documentation
    echo   - Sample input/output examples
    echo   - Technical analysis and conclusions
    echo.
    echo Ready for submission! 🎉
) else (
    echo ❌ Compilation failed!
    echo Please check the LaTeX source for errors.
    pause
    exit /b 1
)

REM Clean up auxiliary files (optional)
set /p cleanup="Clean up auxiliary files? (y/n): "
if /i "%cleanup%"=="y" (
    del /q *.aux *.log *.out *.toc *.fls *.fdb_latexmk 2>nul
    echo 🧹 Auxiliary files cleaned up.
)

pause
