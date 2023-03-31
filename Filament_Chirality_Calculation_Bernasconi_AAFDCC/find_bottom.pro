;******************************************************************************
;***                                                                        ***
     function find_bottom,d,MAX_INDEX=maxind,DIRECTION=diri,$
                          PAST_MIN_SEARCH=mincc
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FIND_BOTTOM
;
; PURPOSE:
;    This function finds the bottom of a given 1D curve
;
; CALLING SEQUENCE:
;    endp = find_bottom(d, [MAX_INDEX=maxind,DIRECTION=diri,$
;                           PAST_MIN_SEARCH=mincc])
;
; INPUTS:
;    d = intarr(NPTS): array with all the distances of the boundary points
;                      from the spine
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    MAX_INDEX = int: index of the d array where the maximum is from which you
;                     want to start. If MAX_INDEX is not given then the
;                     routine will use the index for the maximum that it finds
;                     in the d array.
;    DIRECTION = int: this gives in which direction past the maximum to search
;                     for the bottom. If -1 then it searches to the "left" ie
;                     towards smaller index numbers. If +1 then it searches to
;                     the "right".  Default = +1
;    PAST_MIN_SEARCH = int: this sets how many indexes past a found minimum it
;                     will keep searching to see whether this is just a small
;                     local minimum and not the REAL minimum. Default = 10.
;
; OUTPUTS:
;    endp = int: index of the minimum ind the d curve found.
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
if (n_elements(maxind) le 0) then aa = max(d,maxind)

if (n_elements(diri) le 0) then diri = 1
dir = sign_pb(diri)

if (n_elements(mincc) le 0) then mincc = 10

nels = size(d,/dim) & nels = nels(0)

cc    = 0
end1  = maxind
h0 = d(end1)
repeat begin
    end1 = end1+dir
    if (end1 lt 0) then end1 = nels + end1
    if (end1 ge nels) then end1 = end1 - nels
    h = d(end1)
    ;print,format='(I5,f10.4,f10.4,I3,$)',end1,h,h0,(h lt h0)
    if (h lt h0) then begin
        h0 = h
        cc = 0
        ;print,format='(f10.4,I3)',h0,cc
        continue
    endif
    cc = cc+1
    ;print,format='(f10.4,I3)',h0,cc
endrep until (cc gt mincc)
end1 = end1-dir*cc
if (end1 lt 0) then end1 = nels + end1
if (end1 ge nels) then end1 = end1 - nels

return,end1

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: find_bottom.pro,v $
; Revision 3.3  2010/10/27 20:21:21  bernapn1
; Using sign_pb instead of sign
;
; Revision 3.2  2010/10/07 16:55:12  bernapn1
; Added documentation in the header.
;
; Revision 3.1  2010/10/07 16:42:41  bernapn1
; New
;
;
; Written by Pietro N. Bernasconi JHU/APL
