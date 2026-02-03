# LibreOffice Calc Search

A graphical search too l for searching in LibreOffice Calc .ods files

### Problem

When searching multiple LibreOffice Calc spreadsheets, it is necessary  to open every single file and perform a search inside the spreadsheet.

### What This System Does

- Show a dialog for selecting files to search
- Advanced search details
- Show list with file name, sheet name, row and column and the found content.

### Main Functions

- Search LibreOffice Calc .ods files
- Percents findings, click able to go to file and location
- Exports function of findings

## Functional descriptions

### UI

When opening the main window opens with a old classic menu bar at the top. The menu bar has for now only a few items, File with New Search, Open search, Save Search, Save Search As, Export, and Exit. Settings with Preferences.

On loading program Search Box open automatically, if the user want to load a saved search just click [Cancel] in the Search Box. 

#### Search Box contains.

- What to find
- File Filters
- Directory 
- Depth 0-#  0 = no sub folders  or tick box for In all sub folders  
- Tick box for "Match whole word only" and "Match Case"
- Radio buttons search mode "normal" or "Regular expression"

#### Preferences contains.

- Search Box open automatically tick box.
- Default search Directory

### Main window contains

- The menu bar
- Main area
  Two tabs: A grid with the found results. If result found in more files, the location (file name) is shown over the grid. The tab can have more grids with location over each.
  One file only have one grid.
  The other tab contains search log. ((a standard application log)

#### Other informations.

- Project location /run/media/michael/SSD_Data/Udvikling/LibreOffice_Calc_Search
- The application is written in Pascal using RAD IDE Lazarus Pascal using fpspreadsheet
- Primary target is Fedora Linux, Secondary Linux general and if possible windows  



