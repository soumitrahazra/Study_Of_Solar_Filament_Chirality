;******************************************************************************
;***                                                                        ***
      function find_spine, xpoints, ypoints, factor, mindist=min_mindist,$
                           width = width, apex_move=apex_move, bnd=bnd,$
                           NEW_VERT_CRIT=new_v_crit, weight=weight
;***                                                                        ***
;******************************************************************************
;+
; Project: Automated Solar Filament Detection and Characterization
;
; Name:    find_spine
;
; Purpose: Return the coordinates of the spine along a cluster of points,
;          fit to the filament points in (xpoints, ypoints).
;          Seek "enough" control points in the spine.
;
; Calling sequence:
;    spine = find_spine(xpoints, ypoints [factor, weight=weight, $
;                     mindist=min_mindist, width = width, $
;                     apex_move=apex_move, bnd=bnd,$
;                     NEW_VERT_CRIT=new_v_crit])
;
; Inputs:
;    xpoints = fltarr(n_pix): x coordinates of the filament pixels
;    ypoints = fltarr(n_pix): y coordinates of the filament pixels
;
; Keywords:
;    weight  = fltarr(n_pix): weight of each filament pixel. This is used to
;                 give for example more weight to the filament pixels in the
;                 core of the filament.
;    mindist = minimum distance allowed between vertices. When mindist is
;                 reached, the curve "fitting" stops and the spine is
;                 returned. Default = 20
;    width   = width of the points along the line perpendicular to a
;                 vertex where the vertex optimization occurs. Default = 5
;    apex_move = max number of iterations (vertices) after which the apices
;                 positions will no longer be optimized. Default = 50
;    bnd = fltarr(2,npts): boundary coordinates data
;    NEW_VERT_CRIT = int: Parameter that determines which criterion to be
;                         used when adding new vertices:
;                         0 = Default. halves all segments
;                         1 = halves only longest segment
;
; Outputs: spine = A two-dimensional array containing the spine vertices
;
; Procedures used:
;    distance,  optim_vertex
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
pltdb = 0  ;<-- do some plots for debugging purposes

if (n_elements(xpoints) ne n_elements(ypoints)) then $
  message,'xpoints and ypoints must have the same size.'

; useful for all of the indexing of (x,y) points we'll be doing:
x = 0 & y = 1

if (n_elements(min_mindist) le 0) then min_mindist = 20
if (n_elements(width) le 0) then width = 5
if (n_elements(apex_move) le 0) then apex_move = 50

if (n_elements(new_v_crit) le 0) then new_v_crit = 0
if (new_v_crit gt 1) then new_v_crit = 1

;--- start out with 2 spine vertices:
spine = replicate(-1,2,2)
n = n_elements(xpoints)

if (n_elements(weight) gt 0) then begin
    if (n_elements(weight) ne n) then begin
        print,'FIND_SPINE: WEIGHT parameter is given but its size does not'
        print,'            match number of pixels in filament.'
        print,'            SETTING it all to 1.'
        weight = replicate(1,n)
    endif
endif else weight = replicate(1,n)


;------- Algorithm modified from Kegl et al. 1999. -------
;--- 1. Initialize to a principal curve of 2 points
minx = min(xpoints, minxind)
maxx = max(xpoints, maxxind)
miny = min(ypoints, minyind)
maxy = max(ypoints, maxyind)

;- Finds whether the filament is more elongated in X or Y:
if (maxx-minx ge maxy-miny) then begin
    minindex = minxind
    maxindex = maxxind
endif else begin
    minindex = minyind
    maxindex = maxyind    
endelse

;--- start with 2 points
spine[x,0] = xpoints[minindex] & spine[y,0] = ypoints[minindex]
spine[x,1] = xpoints[maxindex] & spine[y,1] = ypoints[maxindex]
nvert = 2

;--- The following is to make sure that spine has at least 4 segments:
sp_len = distance(spine[x,0],spine[y,0],spine[x,1],spine[y,1])
if (sp_len lt min_mindist*4.) then min_mindist = sp_len/4.-1.
;print,'mindist = ',min_mindist,'  sp_len = ',sp_len

;--- Optimize these vertices (this is not how Kegl et al do it)
spine = optim_vertex(spine, xpoints, ypoints, width = width, factor=factor)

;--- 2. Iterate until the two closest vertices are less than 20 pixels apart
mindist = distance(spine[x,0], spine[y,0], spine[x,1], spine[y,1])
no_apex_optim = 0
while ((nvert lt 2) or (mindist gt min_mindist)) do begin
    if (pltdb) then begin
        erase
        plots,bnd/factor,/dev,color=fsc_color('yellow')
        plots,spine/factor,/dev,psym=-2,color=fsc_color('green')
    endif

    ;--- 2a. Add a new vertex : --------------------------------------
    if (new_v_crit eq 0) then begin
        ;--- Halves each segment in the spine
        newnvert = nvert*2-1
        newspine = intarr(2, newnvert)
        for i=0,newnvert-1 do begin
            if (i/2 eq i/2.) then $
              newspine(*,i) = spine(*,i/2) $
            else begin
                newspine(*,i) = (spine(*,i/2)+spine(*,i/2+1) ) / 2.
                ;print,'New vertex: (',newspine[x,i],',',newspine[y,i],')'
                if (pltdb) then plots,[newspine[x,i],newspine[y,i]]/factor,$
                  /dev,psym=2,color=fsc_color('red')        
            endelse
        endfor
    endif else begin
        newnvert = nvert+1
        newspine = intarr(2, newnvert)
        newspine[*, 0:nvert-1] = spine
        
        ;--- Find the longest segment:
        maxlen = 0
        ind = 0
        for s = 0, nvert-2 do begin
            len = distance(spine(x,s),spine(y,s),spine(x,s+1),spine(y,s+1))
            if (len gt maxlen) then begin
                maxlen = len
                ind = s
            endif
        endfor

        ;--- Move everyone down
        for i=nvert, ind+2, -1 do newspine[*,i] = newspine[*,i-1]
        ;--- Insert a new one, midway between them
        newspine[*,ind+1] = (newspine[*,ind+2]+newspine[*,ind])/2.0
        ;print,'New vertex: (',newspine[x,ind+1],',',newspine[y,ind+1],')'
    endelse
    spine = newspine
    nvert = newnvert
    ;print,spine
    ;-------------------------------------------------------------------

    if (nvert gt apex_move) then no_apex_optim = 1

    ;--- 2b. Optimize positions of all vertices
    spine = optim_vertex(spine, xpoints, ypoints, weight = weight, $
                         width = width, factor = factor, $
                         no_apex_optim = no_apex_optim)
    if (pltdb) then plots,spine/factor,/dev,psym=-2,color=fsc_color('blue')

    ; Update the mindist (smallest distance between two vertices)
    for v=0,nvert-2 do begin
        d = distance(spine[x,v], spine[y,v], spine[x,v+1], spine[y,v+1])
        if (d lt mindist) then mindist = d
    endfor
    ;print,'mindist =', mindist
endwhile

;--- One last optimization before leaving
spine = optim_vertex(spine, xpoints, ypoints, weight = weight, $
                     width = width, factor = factor, $
                     no_apex_optim = no_apex_optim)
if (pltdb) then plots,spine/factor,/dev,psym=-2,color=fsc_color('green')

pts = where(spine[x,*] ne -1, npts)
goodspine = spine[*,pts]
return, goodspine
end
;******************************************************************************
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: find_spine.pro,v $
; Revision 3.3  2010/10/27 19:40:08  bernapn1
; New, more efficient way to optimize location of new points without using
; assing points
;
; Revision 3.2  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.1  2004/12/07 21:24:14  pietro
; Removed one option for how to determine the next vertex.
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
; Revision 1.2  2004/06/04 20:27:20  pietro
; Removed file `message.tmp' form directory
;
; Revision 1.1.1.1  2004/06/04 14:34:11  pietro
; Imported sources
;
; Revision 1.2  2003/10/02 13:59:28  wagstkl1
; Using a slightly larger tolerance for curvy spines (20).
;
; Revision 1.1  2003/10/02 13:32:38  wagstkl1
; Initial revision
;
; Written: Kiri Wagstaff, JHU/APL, May 2003.
