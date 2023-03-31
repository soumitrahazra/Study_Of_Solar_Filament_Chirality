;******************************************************************************
;***                                                                        ***
     function get_fil_chir, barblen, barbdir, REQ_DIFF = reqdiff
;***                                                                        ***
;******************************************************************************
;+
; NAME:   
;    GET_FIL_CHIR
;
; PURPOSE:
;    This function determines the filament chirality from the number and type
;    of barbs that the filament has
;
; CALLING SEQUENCE:
;    chirality = get_fil_chir(barblen, barbdir, REQ_DIFF = req_diff)
;
; INPUTS:
;    barblen = fltarr(N_brbs): array containing the length of each found barb
;    barbdir = fltarr(n_brbs): array containing the direction of each barb:
;                   -1 = bearing right
;                   +1 = bearing left
;                    0 = undetermined
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    REQ_DIFF  = int: minimum difference between left and right barbs to
;                     allow discrimination of filament chirality. Default=2
;
; OUTPUTS:
;    chirality = int: -1 = Right-handed, +1 = Left-handed, 0 = undetermined
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
if (n_elements(reqdiff) le 0) then reqdiff = 2

w=where(barbdir lt 0, nRight)
w=where(barbdir gt 0, nLeft)

chirality = 0
if (nRight ge (nLeft + reqdiff)) then begin
    ;-- Left handed filament: more L barbs than R barbs
    chirality = -1
endif else if (nLeft ge (nRight + reqdiff)) then begin
    ;-- Right handed filament: more R barbs than L barbs
    chirality = 1
endif else begin
    ;-- Case still undecided ==> try to see the total chirality:
    tot_chir = total(barblen*barbdir)
    if (tot_chir le -15) then chirality = -1
    if (tot_chir ge +15) then chirality =  1
endelse

return, chirality

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_fil_chir.pro,v $
; Revision 3.2  2010/10/27 19:42:25  bernapn1
; Improved
;
; Revision 3.1  2010/10/05 15:11:38  bernapn1
; New
;
; Written by Pietro N. Bernasconi JHU/APL
