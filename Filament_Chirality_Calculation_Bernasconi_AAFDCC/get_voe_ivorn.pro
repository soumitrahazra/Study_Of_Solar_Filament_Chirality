;******************************************************************************
;***                                                                        ***
     function get_voe_ivorn, voe_fnam
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    GET_VOE_IVORN
;
; PURPOSE:
;    This function retrieves the IVORN unique ID of a preexisting VOEvent XML
;    file.
;
; CALLING SEQUENCE:
;    ivorn = get_voe_ivorn( voevent_file_name )
;
; INPUTS:
;    voevent_file_name = string: path and name of VOEvent XML file from which
;                          the IVORN is to be extracted
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    ivorn = string: AUTHOR IVORN of the given file
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; COMMON BLOCKS:
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
autoivorn = ''

openr,unit,voe_fnam,/get_lun,error=err
if (err ne 0) then begin
    print,'%GET_VOE_IVORN: ERROR! can not open file. Returning empty string'
    return, autoivorn
endif

s=''
while not(EOF(unit)) do begin
    readf,unit,s
    ps =  strpos(s,'<AuthorIVORN>')
    if (ps gt 0) then begin
        pe = strpos(s,'</AuthorIVORN>')
        autoivorn = strmid(s, ps+13, pe-ps-13)
        break
    endif
endwhile
close,unit
free_lun,unit

if (strlen(autoivorn) eq 0) then $
  print,'%GET_VOE_IVORN: WARNING! Can not find AuthorIVORN.',$
        ' Returning empty string'

return,  autoivorn

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_voe_ivorn.pro,v $
; Revision 3.2  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.1  2010/05/03 14:37:07  bernapn1
; Created
;
; Written by Pietro N. Bernasconi JHU/APL
