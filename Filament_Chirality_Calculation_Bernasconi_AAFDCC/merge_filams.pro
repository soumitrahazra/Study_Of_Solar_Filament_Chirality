;**** Internal routine spine_juggle: ******************************************
pro spine_juggle, cur_fil, filam_data, s_fil, e_fil, s_se, e_se
;Combine the spines of the two filaments
;After this has been run, the spine of the starting filament will be
;the start and the end point on the other side of the ending
;filament will be the end.  
if keyword_set(verbose) then $
  print, 'spine juggling- called by filament -', cur_fil+1

spines= s_fil.spine
verts=s_fil.nverts
verte=e_fil.nverts
spinee= e_fil.spine
if keyword_set(verbose) then print, 's_se=',s_se,'e_se=',e_se,'verts=',verts

;start to start
if ((s_se eq 0) and (e_se eq 0)) then $
  spine = shift(reverse(spines(*,0:verts+verte-1),2),0,verts) + $
          shift(spinee,0,verts)

;end to start
if ((s_se eq 1) and (e_se eq 0)) then $
  spine = spines + shift(spinee,0,verts)

;end to end
if ((s_se eq 1) and (e_se eq 1)) then $
  spine = spines + shift(reverse(spinee(*,0:verts+verte-1),2),0,verts+verte)

;start to end
if ((s_se eq 0) and (e_se eq 1)) then $
  spine = spinee(*,0:verte+verts-1) + shift(spines(*,0:verts+verte-1),0,verte)

filam_data(cur_fil).spine=spine
end 
;******************************************************************************


;**** Internal routine boundary_juggle: ***************************************
     pro boundary_juggle, cur_fil, filam_data, s_fil, e_fil, s_se, e_se
;*** This routine combines in a seamless way the boundaries of two filaments

;--- Finds the nearby two boundary pixels closest to each other:
dd = fltarr(s_fil.NBND_P, e_fil.NBND_P)
for i=0,s_fil.NBND_P-1 do begin  &$
   for j=0,e_fil.NBND_P-1 do begin  &$
       dd(i,j) = distance(s_fil.bnd(0,i),s_fil.bnd(1,i),$
                          e_fil.bnd(0,j),e_fil.bnd(1,j))  &$
   endfor  &$
endfor
ww = where(dd eq min(dd))
ww = ww(0)
wss = ww mod s_fil.NBND_P
wss = (wss + s_fil.NBND_P - 1) mod s_fil.NBND_P
wse = (wss + 2) mod s_fil.NBND_P

wes = ww / s_fil.NBND_P
wes = (wes + 1) mod e_fil.NBND_P

;--- 1st segment from 1st bnd to start connection
tmp_b = s_fil.bnd
bnd_p = wss + 1
if (wse eq 1) then begin  &$
    tmp_b = shift(tmp_b,0,-1)  &$
    bnd_p = bnd_p-1  &$
endif

;--- 2nd segment from start connection to end of 2nd bnd
nn    = e_fil.NBND_P - wes
if (wes eq 0) then nn = nn-1
tmp_b(*,bnd_p:bnd_p+nn-1)=e_fil.bnd(*,wes:wes+nn-1)
bnd_p = bnd_p+nn

;--- 3rd segment from 2nb bnd to end connection (optional)
if (wes ge 2) then begin  &$
    nn = wes-1  &$
    tmp_b(*,bnd_p:bnd_p+nn-1)=e_fil.bnd(*,0:nn-1)  &$
    bnd_p=bnd_p+nn  &$
endif

;--- 4th segment from end connection to end 1st bnd (optional)
if (wse gt 1) then begin  &$
    nn = s_fil.NBND_P - wse  &$
    tmp_b(*,bnd_p:bnd_p+nn-1)=s_fil.bnd(*,wse:wse+nn-1)  &$
    bnd_p=bnd_p+nn  &$
endif

filam_data(cur_fil).bnd = tmp_b
filam_data(cur_fil).nbnd_p = bnd_p
end
;******************************************************************************


;**** Internal routine connect: ***********************************************
pro connect, filam_data, w, endpts, disk_center, clusters
;This will connect the filaments and merge the data into the current
;or starting filament, The variable 'How' is a string that is printed
;as a description of how the connection was found.  If a new way to
;connect filaments is found, the only input that changes is the How,
;every other input can stay the same.  
;------------------------------------------------------------------------------

szd = n_elements(filam_data)

do_clusters = 0
if (n_elements(clusters) gt 0) then begin
    do_clusters = 1
    szc = size(clusters)
    if (szc(0) ne 2) then begin
        print,'MERGE_FILAMS:  WARNING! clusters map dimension is NOT 2.'+$
          ' Ignore it'
        do_clusters = 0
    endif
    if (max(clusters) ne szd) then begin
        print,'MERGE_FILAMS:  WARNING! clusters map does not seem to have'+$
          ' right data in it. Ignore it'
        do_clusters = 0
    endif
endif

cur_fil = endpts(w,0)  ;<-- filament in which the other will merge into
trg_fil = endpts(w,1)  ;<-- filament thaendt the end of this will disappear
cur_fil = cur_fil(0)  &  trg_fil = trg_fil(0)

s_se=endpts(w,2)       ;<-- which end of the cur filament is connected
e_se=endpts(w,3)       ;<-- which end of the trg filament is connected
s_se = s_se(0)  &  e_se = e_se(0)
s_fil=filam_data(cur_fil)
e_fil=filam_data(trg_fil)

;**Just A Reminder**
;    filam_data   = struct_array(nFilaments) with all the filaments parameters:
;        --  area   =  area of the filament in pixels
;        --  coords = intarr(2): X & Y coords of filament location (in pixels)
;        --  bnd    = uintarr(2,nbnd_p): array with (X,Y) coordinates of
;                       filament boundary. Max is 3000 points
;        --  nbnd_p = int: number of poins in the bnd array
;        --  spine  = intarr(2,nverts): array with the (X,Y) coordinates of
;                       the spine vertices. Max is 100 vertices
;        --  angle  = float: filament orientation angle in degrees
;        --  brb_coord = intarr(2,2,nbarbs): array with the (X,Y) coordinates
;                       of line defining a barb. Max # of barbs is 30
;        --  nbarbs = int: number of barbs in the filament
;        --  nRight = int: number of barbs bearing right
;        --  nLeft  = int: number of barbs bearing left
;        --  chir   = int: filament handyness: 1=right, -1=left, 0=unspecifyed
;        --  barbdir= intarr:vector of barb chiralities (directions)
;        --  barblen= fltarr:vector of babs lengths
;    endpts = the endpoints of all other filaments that are
;             near the start of the filament, up to ten
;****************

;---- Merge the two filaments into a single one that is cur_fil: -------------
;- Merges the spines into cur_fil's spine data
spine_juggle, cur_fil, filam_data, s_fil, e_fil, s_se, e_se

;- Gets the new average filament angle
filam_data(cur_fil).angle= filament_angle(filam_data(cur_fil).spine, /DEGREES)

;- Merges the boundaries into cur_fil's boundary data
boundary_juggle, cur_fil, filam_data, s_fil, e_fil, s_se, e_se

;- Moves the array over and combine:
filam_data(cur_fil).brb_coord=s_fil.brb_coord+$
  shift(e_fil.brb_coord,0,0,s_fil.nbarbs)
filam_data(cur_fil).barbdir=s_fil.barbdir+shift(e_fil.barbdir,s_fil.nbarbs)
filam_data(cur_fil).barblen=s_fil.barblen+shift(e_fil.barblen,s_fil.nbarbs)

;- number of barbs + number of barbs = number of barbs combined:
filam_data(cur_fil).nbarbs=s_fil.nbarbs+e_fil.nbarbs
filam_data(cur_fil).nRight=s_fil.nRight+e_fil.nRight
filam_data(cur_fil).nLeft=s_fil.nLeft+e_fil.nLeft
filam_data(cur_fil).nverts=s_fil.nverts+e_fil.nverts
area_tot = s_fil.area+e_fil.area
filam_data(cur_fil).area=area_tot
filam_data(cur_fil).area_rat = (s_fil.area_rat * s_fil.area + $
                                e_fil.area_rat * e_fil.area ) / float(area_tot)

;- Determines the new chirality
filam_data(cur_fil).chir = get_fil_chir(filam_data(cur_fil).barblen,$
                                        filam_data(cur_fil).barbdir)

;- Finds the combined filament bounding box
mincrds=[min(filam_data(cur_fil).bnd(0,0:filam_data(cur_fil).nbnd_p-1),$
             max=maxx), $
         min(filam_data(cur_fil).bnd(1,0:filam_data(cur_fil).nbnd_p-1),$
             max=maxy)]
maxcrds=[maxx, maxy]
filam_data(cur_fil).bbox(*,0) = mincrds - 10
filam_data(cur_fil).bbox(*,1) = maxcrds + 10

;- Re determines the location of the center of the spine: 
nv = filam_data(cur_fil).nverts
spc = spine_center(filam_data(cur_fil).spine(*,0:nv-1), length=splen)
filam_data(cur_fil).coords = spc - disk_center
;Re enter the value of the lenght of the spine
filam_data(cur_fil).length = round(splen)

;--- Prevent any other connections to be made to the two end points
;- 1) Erases that specific connection:
endpts(w,*) = [-1, -1, -1, -1, 1000]

;- 2) Erases other connections to the two end points & flips ends if needed
for j=0,1 do begin  ;<-- first for the end point from the cur_fil
    ww = where(endpts(*,j) eq cur_fil, nww)
    if (nww gt 0) then begin
        www = where(endpts(ww,j+2) eq s_se, nwww)
        if (nwww gt 0) then $
          for k=0,nwww-1 do endpts(ww(www(k)),*) = [-1, -1, -1, -1, 1000]
    endif
endfor
for j=0,1 do begin  ;<-- second for the end point from the trg_fil
    ww = where(endpts(*,j) eq trg_fil, nww)
    if (nww gt 0) then begin
        www = where(endpts(ww,j+2) eq e_se, nwww)
        if (nwww gt 0) then $
          for k=0,nwww-1 do endpts(ww(www(k)),*) = [-1, -1, -1, -1, 1000]
    endif
endfor


;- 3) Removes residual connections from the other ends of the two merged
;     filaments ==> prevents to get a loop connection -> doughnot
for j=0,1 do begin
    ww = where(endpts(*,j) eq cur_fil, nww)
    if (nww gt 0) then begin
        www = where(endpts(ww,((j+1) mod 2) ) eq trg_fil, nwww)
        if (nwww gt 0) then $
          for k=0,nwww-1 do endpts(ww(www(k)),*) = [-1, -1, -1, -1, 1000]
    endif
endfor

;- 4) Flips the connection endpoint if necessary:
for j=0,1 do begin
    ;First for the end point from the cur_fil:
    ww = where(endpts(*,j) eq cur_fil, nww)
    if (nww gt 0) then begin
        ;start-to-start
        if ((s_se eq 0) and (e_se eq 0)) then begin
            endpts(ww,j+2) = (endpts(ww,j+2) + 1) mod 2
            ;print,'FLIPPING current',cur_fil
        endif
    endif

    ;Second for the end point from the trg_fil
    ww = where(endpts(*,j) eq trg_fil, nww)
    if (nww gt 0) then begin
        ;end-to-end
        if ((s_se eq 1) and (e_se eq 1)) then begin
            endpts(ww,j+2) = (endpts(ww,j+2) + 1) mod 2
            ;print,'FLIPPING target',trg_fil
        endif
    endif
endfor

;--- Handles the cluster map by assigning cur_fil to pixels labeled trg_fil 
if (do_clusters) then begin
    w = where(clusters eq trg_fil+1)
    clusters(w) = cur_fil+1
endif

;--- Shifts up all filaments above the upper filament in the list:
;- 1) Changes the trg_fil nuber in endpts to the cur_fil number
endpts_fn = endpts(*,0:1)
w = where(endpts_fn eq trg_fil, nw)
if (nw gt 0) then endpts_fn(w) = cur_fil

;- 2) Changes all the ones above trg_fil by one less:
if (trg_fil ne szd-1) then begin
    mm = max(endpts_fn)
    ww = where( ((endpts_fn gt trg_fil) and (endpts_fn le mm) ), nww)
    if (nww gt 0) then endpts_fn(ww) = endpts_fn(ww) - 1

    ; handles the cluster_map:
    if (do_clusters) then begin
        www = where(clusters gt trg_fil+1, nwww)
        if (nwww gt 0) then clusters(www) = clusters(www) - 1
    endif
endif
endpts(*,0:1) = endpts_fn

;- 3) does the actual shifting in 
if (trg_fil ne szd-1) then begin
    filam_data(trg_fil:szd-2) = filam_data(trg_fil+1:szd-1)
endif
filam_data = filam_data(0:szd-2)

end
;******************************************************************************


;**** internal routine calc_seg_ang: ******************************************
function calc_seg_ang, filam_data, fil_num, se, fil_num2, se2
;Given the spine, filament number, the spine, and whether to get the spine at
;the start point or the endpoint, it returns the angle of the first (or final)
;spine segment se=Start/End, for start, se=0, for end, se=1
;It the # of parameters is > 3 then the calculation is for the segment
;connecting the two spines endpoints:

;-- Define the points pp(<x,y>, <2-points.>)
    pp = fltarr(2,2)
if (n_params() eq 3) then begin
    nn = 0
    if (se eq 1) then nn = filam_data(fil_num).nverts-2
    for i=0,1 do for j=0,1 do pp(j,i) = filam_data(fil_num).spine(j,nn+i)
endif else begin
    fn  = [fil_num,fil_num2]
    se0 = [se,se2]
    for i=0,1 do begin &$
        nn = 0 &$
        if (se0(i) eq 1) then nn = filam_data(fn(i)).nverts-1 &$
        for j=0,1 do pp(j,i) = filam_data(fn(i)).spine(j,nn) &$
    endfor
endelse

;-- First comes the one on the left, i.e. smaller x valu
if (pp(0,1) lt pp(0,0)) then pp = shift(pp,0,1)

;-- calculates the angle
if (pp(0,0) eq pp(0,1)) then $
    angle = !PI/2. $
else $
    angle = atan(pp(1,1)-pp(1,0), pp(0,1)-pp(0,0))

return, angle
end
;******************************************************************************



;******************************************************************************
;***                                                                        ***
    pro merge_filams, filam_data,disk_center, clusters, MAX_ANG=max_ang,$
                        MAX_DIST=max_dist,VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    MERGE_FILAMS
;
; PURPOSE:
;    Identify filaments that are very, very close to each other or
;    that are near and have spines that point generally toward each
;    other. Then combines combine all their filament data
;    into a single filament.
;
; CALLING SEQUENCE:
;    merge_filams,filam_data, disk_center [, MAX_ANG=max_ang, $
;                 MAX_DIST = max_dist, verbose=verbose]
;
; INPUTS:    
;    filam_data: where all the data for filaments is stored
;    disk_center: coordinates of the center of the sun
;
; OPTIONAL INPUT PARAMETERS:
;    clusters = fltarr(Xsz,Ysz): map array with the numbered pixels belonging
;                 to each individual filament.  This file gets modified
;                 whenever a new merge is done.
;
; KEYWORD PARAMETERS:
;    MAX_DIST    = float: the distance below which two nearby filaments are
;       merged together
;    MAX_ANG     = float: the amount in radians that the slope of the
;       spine segment nearest the imaginary connection line can differ
;       and still have the connection be recognized
;    VERBOSE     = flag: if set then print some infos in the sdout
;
; OUTPUTS:
;  Not a function, filam_data is passed by reference however, and will
;  be changed in this process.  
;
;    filam_data   = struct_array(nFilaments) with all the filaments parameters:
;          area   = intarr: area of the filament in pixels
;          coords = intarr(2): X & Y coords of filament location (in pixels)
;          bnd    = uintarr(2,nbnd_p): array with (X,Y) coordinates of
;                       filament boundary. Max is 3000 points
;          nbnd_p = int: number of poins in the bnd array
;          spine  = intarr(2,nverts): array with the (X,Y) coordinates of
;                       the spine vertices. Max is 100 vertices
;          angle  = float: filament orientation angle in degrees
;          brb_coord = intarr(2,2,nbarbs): array with the (X,Y) coordinates
;                       of line defining a barb. Max # of barbs is 30
;          nbarbs = int: number of barbs in the filament
;          nRight = int: number of barbs bearing right
;          mLeft  = int: number of barbs bearing left
;          chir   = int: filament handyness: -1=right, 1=left, 0=unspecifyed
;          endpts = the endpoints of all other filaments that are
;                   near the start of the filament, up to ten*******After
;                   this process, this array will be completely
;                   indecipherable to an outside user and probably
;                   useless to the program, if this array can be used
;                   for something else, a temporary copy of it can be
;                   stored at the beginning and this can be replaced
;                   at the end of the call -- Daniel Hakim
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    These are in this same file and therefore uploaded when merge_filams is
;    uploaded: connect, spine_juggle, boundary_juggle, calc_seg_ang 
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if n_elements(verbose) le 0 then verbose=0
if n_elements(Max_Ang) le 0 then Max_Ang = !pi/8  ;<-- 22.5 deg
if n_elements(max_dist) le 0 then max_dist = 25

szf=n_elements(filam_data)
fil_data = filam_data

max_dist_i = 75  ;<-- This is the absolute maximum distance ever considered
if (max_dist gt max_dist_i) then max_dist_i = max_dist

if verbose then begin
    print
    print,'Merging nearby filaments ...'
endif

;---- Determining all the small distance connections possible: ---
dists = fltarr(4)
max_endpts = 100  &  incr_endpts = 4
endpts = fltarr(max_endpts,5)
i = 0
for cf=0,szf-2 do begin 
    xcs = fil_data(cf).spine(0,0)
    ycs = fil_data(cf).spine(1,0)
    xce = fil_data(cf).spine(0,fil_data(cf).nverts-1)
    yce = fil_data(cf).spine(1,fil_data(cf).nverts-1)

    for ofl = cf+1, szf-1 do begin
        xos = fil_data(ofl).spine(0,0)
        yos = fil_data(ofl).spine(1,0)
        xoe = fil_data(ofl).spine(0,fil_data(ofl).nverts-1)
        yoe = fil_data(ofl).spine(1,fil_data(ofl).nverts-1)

        dists(0) = distance(xcs,ycs,xos,yos)
        dists(1) = distance(xcs,ycs,xoe,yoe)
        dists(2) = distance(xce,yce,xos,yos)
        dists(3) = distance(xce,yce,xoe,yoe)

        w = where(dists le max_dist_i, nw)
        if ( nw le 0) then continue
        
        for j=0,nw-1 do begin
            endpts(i,0:1) = [cf,ofl]
            endpts(i,4) = dists(w(j))
            case w(j) of
                0: endpts(i,2:3) = [0,0] ;start-to-start connection
                1: endpts(i,2:3) = [0,1] ;start-to-end connection
                2: endpts(i,2:3) = [1,0] ;end-to-start connection
                3: endpts(i,2:3) = [1,1] ;end-to-end connection
            endcase
            i = i +1
            if (i ge max_endpts) then begin
                tmp = endpts
                max_endpts = max_endpts + incr_endpts
                endpts = fltarr(max_endpts,5)
                endpts(0:i-1,*) = tmp
            endif
        endfor
    endfor
endfor
if (i eq 0) then return

endpts = endpts(0:i-1,*)
endpts_orig = endpts

;--- Determining which spines to connect when the 2 are closer than max_dist:
while (min(endpts(*,4)) le  max_dist) do begin
    w = where( endpts(*,4) eq min(endpts(*,4)))
    w=w(0)
    if verbose then print, format='(A,I2," and ",I2,A)',$
      '    Filaments ',endpts_orig(w,0)+1, endpts_orig(w,1)+1,$
      ' are the same - Small Dist. Connection = '+$
      strcompress(string(endpts_orig(w,4)),/rem)
    connect, filam_data, w, endpts, disk_center, clusters
endwhile

;--- Determining which spines to connect when the 2 ends have ~ same angle
while (min(endpts(*,4)) le max_dist_i) do begin
    w = where( endpts(*,4) eq min(endpts(*,4)))
    w=w(0)

    angles = calc_seg_ang(filam_data,endpts(w,0),endpts(w,2))
    anglee = calc_seg_ang(filam_data,endpts(w,1),endpts(w,3))
    anglem = calc_seg_ang(filam_data,endpts(w,0),endpts(w,2),$
                                     endpts(w,1),endpts(w,3))

    em_p_pi_p_mxa = anglem + !pi + max_ang
    em_p_pi_m_mxa = anglem + !pi - max_ang
    em_m_pi_p_mxa = anglem - !pi + max_ang
    em_m_pi_m_mxa = anglem - !pi - max_ang

    if ((angles lt anglem+max_ang and angles gt anglem-max_ang) or $
        (angles lt em_p_pi_p_mxa and angles gt em_p_pi_m_mxa)   or $
        (angles lt em_m_pi_p_mxa and angles gt em_m_pi_m_mxa)) $
      AND $                     ;If angles is within maxang of anglem
       ((anglee lt anglem+max_ang and anglee gt anglem-max_ang) or $
        (anglee lt em_p_pi_p_mxa and anglee gt em_p_pi_m_mxa)   or $
        (anglee lt em_m_pi_p_mxa and anglee gt em_m_pi_m_mxa)) $
      then begin
        if verbose then print, format='(A,I2," and ",I2,A)',$
          '    Filaments ',endpts_orig(w,0)+1, endpts_orig(w,1)+1,$
          ' are the same - Spine Angle Connection = '+$
          strcompress(string(endpts_orig(w,4)),/rem)
        connect, filam_data, w, endpts, disk_center, clusters
    endif else begin
        ; Removes this point from the list of selectable
        endpts(w,*) = [-1, -1, -1, -1, 1000]
    endelse
endwhile

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: merge_filams.pro,v $
; Revision 3.13  2010/10/05 15:22:27  bernapn1
; Now using function get_fil_chir to determine the chirality of the merged
; filament.
;
; Revision 3.12  2010/04/29 20:55:12  bernapn1
; Improved verbose printouts
;
; Revision 3.11  2010/04/15 13:41:53  bernapn1
; Fixed some bugs
;
; Revision 3.10  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.9  2010/04/13 15:18:26  bernapn1
; Totally new routine. Performs better and its logic is more clear. Some
; subroutines are still from the original code written by Daniel Hakim
;
;
; Written by Pietro N. Bernasconi JHU/APL
