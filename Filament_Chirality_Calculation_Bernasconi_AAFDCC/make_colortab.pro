;******************************************************************************
;***                                                                        ***
      function make_colortab, coltabnam, GREY_LIMIT = max_grey, $
                              RED = red, GREEN = green, BLUE = blue
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    MAKE_COLORTAB
;
; PURPOSE:
;    This function is used in the case that a NON true color (24 bits) is
;    used. It downloads colors form a color table and sets the  rest to a
;    grey table
;
; CALLING SEQUENCE:
;    color_palette = make_colortab( [colortab_name, GREY_LIMIT = max_grey,$
;                    RED = red, GREEN = green, BLUE =blue] )
;
; INPUTS:
;    None
;
; OPTIONAL INPUT PARAMETERS:
;    colortab_name = string: Name of the table with the list of colors
;           The data in the table must have the following format:
;
;           ;* Comment line
;           color_name_1
;           red    green   blue
;           color_name_2
;           red    green   blue
;           ......
;
;           NO BLANK LINES ARE ALLOWED BETWEEN color name and the next line
;           with the color code! Elsewhere blank lines are allowed.
;           A `;*` indicates that the line is a comment and it will be
;           ignored
;           Default is a table located in this same package directory
;           called: my_colortab.dat
;
; KEYWORD PARAMETERS:
;    GREY_LIMIT = int: code for the upper limit of the gray scale
;    RED   = intarr(255) = color table for red channel
;    GREEN = intarr(255) = color table for green channel
;    BLUE  = intarr(255) = color table for blue channel
;
; OUTPUTS:
;    color_palette = structure containing the color table list.
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
;--- Gets the location and name of the colortable:
if (n_elements(coltabnam) le 0) then $
  coltabnam = filam_get_paths(/idl)+'my_colortab.dat'

max_c = !d.n_colors - 1

if (max_c gt 255) then begin
    print,'% MAKE_COLORTAB: This is apparently a true color device! NOT NEEDED'
    return, -1
endif

;--- Gets the number of different colors: -------------------------------------
openr,unit,coltabnam,/get_lun, error=err
if (err ne 0) then begin
    print,'% MAKE_COLORTAB: file "',coltabnam,'" does not exist'
    close, unit  &  free_lun,unit
    return, -1
endif
s=''
n_cols = 0
while (EOF(unit) le 0) do begin
    readf,unit,s
    if ((strpos(s,';*') ne 0) and (s ne '')) then begin
        n_cols = n_cols + 1
        readf,unit,s
    endif
endwhile
close,unit

;--- Creating the gray part of the colortable:
max_grey = max_c - n_cols
tmp=bytarr(256)
tmp(0:max_grey) = byte(dindgen(max_grey + 1) * 255 / max_grey)
red   = tmp
green = tmp
blue  = tmp

;--- Creating the colored part of the colortable:
color_entry = {name:'', code:0, rgb: intarr(3)}
color_tab = replicate(color_entry, n_cols)
r=0 & g=0 & b=0
min_cols = max_grey + 1

openr,unit,coltabnam
col = 0
while (EOF(unit) le 0) do begin
    readf,unit,s
    if ((strpos(s,';*') ne 0) and (s ne '')) then begin
        code = min_cols+col
        readf,unit,r,g,b
        color_tab(col).name = s
        color_tab(col).rgb  = [r,g,b]
        color_tab(col).code = code
        red(code) = r
        green(code) = g
        blue(code) = b
        col = col + 1
    endif
endwhile

;--- loads the color table in IDL:
tvlct,red,green,blue

close,unit  &  free_lun,unit
return, color_tab

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: make_colortab.pro,v $
; Revision 3.1  2004/12/30 19:24:53  pietro
; Now getting the default paths from filam_get_paths.
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.3  2004/06/21 21:53:58  pietro
; Added keywords RED, GREEn , BLUE
;
; Revision 2.2  2004/06/21 18:42:22  pietro
; modified
;
; Revision 2.1  2004/06/17 20:56:12  pietro
; Created
;
; Written by Pietro N. Bernasconi JHU/APL
