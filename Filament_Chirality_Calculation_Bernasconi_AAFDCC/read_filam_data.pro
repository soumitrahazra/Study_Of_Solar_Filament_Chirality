;******************************************************************************
;***                                                                        ***
      function read_filam_data, infile, SUN_PARAMS=sun_p, $
                                DISP_IMAGE=disp_img, $
                                CORRECT_IMAGE=corr_img, $
                                C_PATH=c_path
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    READ_FILAM_DATA
;
; PURPOSE:
;    This function reads the filament data from a .dat file that was
;    created by the program solar_filaments.
;
; CALLING SEQUENCE:
;    filam_data = read_filam_data(infile [SUN_PARAMS=sun_params, $
;                     DISP_IMAGE=disp_image, /CORRECT_IMAGE, C_PATH=c_path])
;
; INPUTS:
;    infile = string: name of the file with the saved data
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    SUN_PARAMS = intarr(3): Solar disk parameters:
;                   0) Sun radius; 1) X coord of center; 2) Y coord of center
;    DISP_DIMAGE = fltarr(Xs,Ys): Image used for the display.
;    CORRECT_IMAGE = flag: if set then performs a fit of the image surface
;        to remove residual inhomogeneitier (SLOW!)
;    C_PATH = string: Directory path where the c executables are located.
;                   Default is given by the call to the routine:
;                   filam_get_paths(/bin)
;
; OUTPUTS:
;    filam_data = structure with all the filament data. See header of
;                   routine study_filaments for a description.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(infile) le 0) then begin
    print,'% READ_FILAM_DATA: missing input file name!'
    return, -1
endif

if (size(infile,/type) ne 7) then begin
    print,'% READ_FILAM_DATA: input parameter is not a string!'
    return, -1
endif

if (n_elements(c_path) le 0) then c_path=filam_get_paths(/bin) $
else if (size(c_path, /type) ne 7) then begin
    print,'% READ_FILAM_DATA: C_PATH is not a string!'
    c_path=filam_get_paths(/bin)
    print,'                reset to directory '+c_path
endif

;--- Checks if infile exists:
tmp = file_search(infile, count = n_files)
if (n_files le 0) then begin
    print,'% READ_FILAM_DATA: input file ',infile,' does NOT esist!'
    return, -1
endif

;--- Uncompress the file if necessary:
if (strmid(infile,2,/reverse) eq '.gz') then begin
    spawn, '/bin/gunzip '+infile
    infile_u = strmid(infile,0,strlen(infile)-3)
endif else infile_u=infile

;--- Open the data file
openr, index, infile_u, /get_lun

;--- Get the name of the fits file with the image:
fits_img = ''
readf, index, fits_img
;- Checks if fits file exists:
tmp = file_search(fits_img, count = n_files)
if (n_files le 0) then begin
    print,'% READ_FILAM_DATA: fits file ',fits_img,' does NOT esist!'
    close,index & free_lun,index
    return, -1
endif

;--- Get the sun_p parameter
sun_p = fltarr(7)
readf, index, sun_p

;--- Reads the fits image:
disp_img = readfits(fits_img, header)
;- If a KANZELHOEHE image then applies some fixes:
if (sxpar(header,'ORIGIN') eq 'KANZELHOEHE') then begin
    jd_kanz_switch = 2452918.5 ;<-- Julian Date when Kanzelhoehe switched
                               ;    format: 2003/10/06
    
    ;--- Determine date of the observation:
    date = sxpar(header,'DATE_OBS') & szt=size(date,/type)
    if (szt ne 7) then begin
        date = sxpar(header,'DATE-OBS') & szt=size(date,/type)
        if (szt eq 7) then date = date + 'T' + sxpar(header,'TIME-OBS') + $
          '.000Z'
    endif
    if (szt ne 7) then date = pars.image
    jul_dat = get_jdate(date)   ;<-- calculate Julian Date

    ;--- account ing for a specular reverse in the Kanzelhoe data:
    if (jul_dat lt jd_kanz_switch) then disp_img = reverse(disp_img,1)

    ;- remove the P angle roll:
    if  (sun_p(4) ne -1) then begin
        pang = sun_p(5)
        disp_img = rot(disp_img, pang, 1., sun_p(1), sun_p(2), /pivot, /interp)
    endif
endif
;- Apply a correction to the surface if requested:
if keyword_set(corr_img) then begin
    cf = fit_surface(disp_img, /force, /adj_offs)
    disp_img = disp_img - cf
endif

;--- Get the structure dimensions
readf, index, nfilams, mxnbnd, mxnverts, mxnbarbs

;--- Create the filaments data structure:
fil={area:0, coords: intarr(2), bnd:uintarr(2,mxnbnd), nbnd_p:0, $
     spine:intarr(2,mxnverts), angle:0., length:0, nverts:0, $
     brb_coord:intarr(2,2,mxnbarbs), barbdir:intarr(mxnbarbs), nbarbs:0, $
     nRight:0, nLeft:0, chir:0}
filam_data = replicate(fil, nfilams)

;--- Read the filaments data:
tmp = 0.
coord = intarr(2)
bnd   = uintarr(2,mxnbnd)
spine = intarr(2,mxnverts)
brb_coord = intarr(2,2,mxnbarbs)
barbdir = intarr(mxnbarbs)
for i=0,nfilams-1 do begin
    readf, index
    readf, index, tmp   & filam_data(i).area = tmp
    readf, index, coord & filam_data(i).coords = coord
    readf, index, bnd   & filam_data(i).bnd = bnd
    readf, index, tmp   & filam_data(i).nbnd_p = tmp
    readf, index, spine & filam_data(i).spine = spine
    readf, index, tmp   & filam_data(i).angle = tmp
    readf, index, tmp   & filam_data(i).length = tmp
    readf, index, tmp   & filam_data(i).nverts = tmp
    readf, index, brb_coord & filam_data(i).brb_coord = brb_coord
    readf, index, barbdir   & filam_data(i).barbdir = barbdir
    readf, index, tmp   & filam_data(i).nbarbs = tmp
    readf, index, tmp   & filam_data(i).nright = tmp
    readf, index, tmp   & filam_data(i).nleft = tmp
    readf, index, tmp   & filam_data(i).chir = tmp
endfor
close,index & free_lun,index

if (strmid(infile,2,/reverse) eq '.gz') then spawn, '/bin/gzip '+infile_u

return, filam_data

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: read_filam_data.pro,v $
; Revision 3.2  2010/04/02 19:11:06  bernapn1
; Removed absolute path in call to gzip
;
; Revision 3.1  2004/12/30 19:24:53  pietro
; Now getting the default paths from filam_get_paths.
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.2  2004/07/16 19:28:27  pietro
; Now handels properly kanzelhoehe images prior to 2003/10.06
;
; Revision 2.1  2004/07/15 20:01:44  pietro
; Added length in filam_data
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.4  2004/06/14 16:28:57  pietro
; Apply P angle roll correction if a KANZELHOEHE image
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: 05/05/04
