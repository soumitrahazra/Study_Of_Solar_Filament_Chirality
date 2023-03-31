;******************************************************************************
;***                                                                        ***
     function ctb_color, c_name, colortable
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    CTB_COLOR
;
; PURPOSE:
;    This function gets the proper color code for a given color name, by
;    looking into the list of allowed colors for the downloaded color table.
;
; CALLING SEQUENCE:
;    code = ctb_color( color_name, colortable)
;
; INPUTS:
;    color_name = string: name of the color for which a color code is needed
;    colortable = sructure array .name, .code: structure with table of
;                   colors. For each color name it has the corresponding
;                   color code.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    core = int: numerical code for the requested color name.
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

;--- Checking the input:
if (n_params() lt 2) then message,'Two inputs needed: color_name, table_name'

sz=size(colortable,/dim)

code = 255       ;--> color code inintialization

for i=0,sz(0)-1 do begin
    if colortable(i).name eq c_name then code = colortable(i).code
endfor

return, code

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: ctb_color.pro,v $
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.2  2004/08/24 17:43:21  pietro
; Added documentation in program header
;
; Revision 2.1  2004/06/21 20:15:49  pietro
; Created and tested
;
;
; Written by Pietro N. Bernasconi JHU/APL
