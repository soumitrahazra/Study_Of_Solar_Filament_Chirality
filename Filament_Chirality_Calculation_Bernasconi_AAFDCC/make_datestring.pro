;******************************************************************************
;***                                                                        ***
      function make_datestring, date1, date2, date3, separator=separ     
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    MAKE_DATESTRING
;
; PURPOSE:
;    This function makes a date in string format
;
; CALLING SEQUENCE:
;    datestr = make_datestring(date1, date2, date3 [, separator=separator])
;         OR
;    datestr = make_datestring(date [, separator=separator])
;
; INPUTS:
;    date1, date2, date3 = int: elements of the date. For example tey can
;            be: year, month, day.
;            SUGGESTION: always make the year a 4-digit number!
;    OR
;    date = intarr(3): All three elements of the date in one array.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;
;    separator = string: what goes between the date elements: eg '_', '-',
;                          ' ', ...
;                          Default is nothing i.e. ''
;
; OUTPUTS:
;    datestr = string: date in string format
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
if (n_params() eq 3) then $
    date=[date1,date2,date3] $
else begin
    if (n_params() eq 1) then begin
        if (n_elements(date1) ne 3) then $
          message,'date must be a 3-element array'
        date=date1
    endif else message,'incorrect number of input parameters: 3 single '+$
      'elements OR 1 3-element array'
endelse

if (n_elements(separ) le 0) then separ = ''

out=''
for i=0,2 do begin
    if (date(i) lt 10) then out=out+'0'
    out=out+string(strcompress(date(i),/rem))
    if (i ne 2) then out=out+separ
endfor

return, out
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: make_datestring.pro,v $
; Revision 3.3  2004/11/23 14:37:21  pietro
; Simplifyed the routine
;
; Revision 3.2  2004/11/23 14:22:40  pietro
; Done something
;
; Revision 3.1  2004/11/22 22:08:28  pietro
; Created
;
; Written by Pietro N. Bernasconi JHU/APL
