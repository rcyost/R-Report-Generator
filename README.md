# Report-Generator-in-R

A system to create country reports in R. 

By defining a template and passing new metadata, say in a loop, the system can generate a copy of the report for a new country. 

This is a dummy version meant to share the code, not share any data.

This should give someone more than a headstart in creating their own reporting tool.

An example output named RandomData_MCD_.pdf is included for reference.

    .
    ├── data  
    │   ├── 01- Raw             dump for raw data
    │   ├── 02- Clean           dump for clean data
        └── ...                 individual data collecting scripts to put data in raw or clean
    ├── master.Rmd              loads requirements, utilities, and runs report generating code
    ├── output.Rmd              the report template that is run in a loop by master.Rmd
    ├── requirements.R          R packages and libraries needed
    ├── utilities.R             helper and chart creating functions
    └── README.md



<object data="RandomData_MCD_.pdf" type="application/pdf" width="700px" height="700px"></object>

