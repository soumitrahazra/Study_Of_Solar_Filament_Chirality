;******************************************************************************
;***                                                                        ***
    function spine_mask,spine,LL=ll,DX=dx,DY=dy
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    SPINE_MASK
;
; PURPOSE:
;    This function creates a mask with areas of a certain width dx and dy
;    around each point in the spine with value 1. Everywhere elese is 0. This
;    routine is useful to do tracking of filaments across different images.
;
; CALLING SEQUENCE:
;    mask = spine_mask(spine  [, LL=ll, DX=dx, DY=dy]
;
; INPUTS:
;    spine = array(2,N_pts) : array with the x any y coordinates of each point
;             in the spine
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    LL = intarr(2): X and Y coordinates of the lower left corner of the mask
;             box. This is useful for determining where to insert this submask
;             in a larger full-disk mask.
;    DX = int or float: +/- widht in X in pixels before and after the points
;             in the spine where to put 1s. Default = 20.
;    DY = int or folat: same as DX but for the Y direction. Default = 20.
;
; OUTPUTS:
;    mask = intarr(X_size, Y_size): array mask with value 1 around points of
;             the spine, and 0 everywhere
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; COMMON BLOCKS:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

if (n_elements(dx) le 0) then dx = 20
if (n_elements(dy) le 0) then dy = 20

szs = size(spine,/dim)

bxmn = min(spine(0,*), max=bxmx)
bxmn = bxmn - dx - 2
bxmx = bxmx + dx + 2
bymn = min(spine(1,*), max=bymx)
bymn = bymn - dy - 2
bymx = bymx + dy + 2

bxs = bxmx - bxmn + 1
bys = bymx - bymn + 1

mask = intarr(bxs,bys)

for i=0,szs(1)-1 do begin  &$
    ll = [spine(0,i)-bxmn-dx,spine(1,i)-bymn-dy]  &$
    ur = [spine(0,i)-bxmn+dx,spine(1,i)-bymn+dy]  &$
    mask(ll(0):ur(0),ll(1):ur(1)) = 1  &$
endfor

ll=[bxmn,bymn]

return,mask

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: spine_mask.pro,v $
; Revision 3.2  2010/04/29 21:00:44  bernapn1
; Not sure what changed but must be something minor
;
; Revision 3.1  2010/04/20 17:14:47  bernapn1
; Created
;
; Written by Pietro N. Bernasconi JHU/APL
