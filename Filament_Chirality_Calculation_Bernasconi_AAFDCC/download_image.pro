;******************************************************************************
;***                                                                        ***
    function download_image, datei, BBSO=bbso, KANZ=kanz, OUT_IMAGE=out_image,$
                             IMG_PATH=img_pathi, VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    DOWNLOAD_IMAGE
;
; PURPOSE:
;    This function checks a location in a remote server for H_alpha images
;    to download. After the download the image is analyzed and the results
;    are written in output files.
;
; CALLING SEQUENCE:
;    status = download_image([date, /BBSO, /KANZ, OUT_IMAGE=out_image, $
;                            IMG_PATH=img_path, /VERBOSE])
;
; INPUTS:
;    No required inputs
;
; OPTIONAL INPUT PARAMETERS:
;    date = [YYYY, MM, DD]: A 3 elements integer array with the date of the
;                day for which the image is downloaded.
;
; KEYWORD PARAMETERS:
;    BBSO = flag: If set then images will be downloaded from the BBSO server
;                 This is also the default in case neither BBSO not KANZ is
;                 selected
;    KANZ = flag: If set then images will be downloaded from the KANZ server
;    OUT_IMAGE = string: Name of the downloaded image, ! WITH FULL PATH !
;    IMG_PATH = string: Path in the local file system where the downloaded
;                image will be stored.
;    VERBOSE = flag: If set then some additional information is dispalyed
;                on the screen
;
; OUTPUTS:
;    status = int: 0 if no errors were encountered
;                  1 if some type of error occurred
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES CALLED:
;    get_ftp_listing, get_ftp_file, create_input_file, solar_filaments,
;    make_datestring
;
;-
; MODIFICATION HISTORY: See end of file
;******************************************************************************

print, 'download_image.pro: '+strtrim(string(datei),2)

if (n_elements(verbose) le 0) then verbose = 0

if (not(keyword_set(bbso)) and not(keyword_set(kanz))) then begin
    print,'% DOWNLOAD_IMAGE: neither BBS nor KANZ server specified. Using BBSO'
    bbso=1
endif

;--- Gets the date to be used:
datenow=1
if (n_elements(datei) gt 2) then begin
    type = size(datei,/type)
    if ( (type ne 2) and (type ne 3)) then begin
        print,'% DOWNLOAD_IMAGE: Incorrect size of date paremeter!',$
          ' Use current date.'
    endif else datenow = 0
    date = datei
endif
if datenow then begin
    date = bin_date()
    date = date(0:2)
endif
if (n_elements(d_dayi) gt 0) then begin
    d_day=fix(abs(d_dayi)+0.5)
    caldat, julday(date(1),date(2),date(0))-d_day, month, day, year
    date(0) = year
    date(1) = month
    date(2) = day
endif

;--- Builds the local file system path where images are stored:
if (n_elements(img_pathi) gt 0) then begin
    img_path = img_pathi
    if (size(img_path,/type) ne 7) then begin
        print,'% DOWNLOAD_IMAGE: IMG_PATH must be a string!'
        return, 1
    endif
    if (strmid(img_path,strlen(img_path)-1) ne '/') then $
      img_path = img_path+'/'         ;<-- add a '/' at end if missing  
endif else img_path='./'

;------------------------------------------------------------------------------
;--- Getting the file to download if any --------------------------------------
found = 0
strdate = make_datestring(date,sep='/')

if (keyword_set(bbso)) then begin
    server = 'ftp://ftp.bbso.njit.edu'
    serv_nam = 'BBSO'

    if verbose then print,'Looking for images on BBSO server:'

    ;--- Builds the path in bbso file system:
    path = 'pub/archive/' + strdate + '/'

    ;--- Getting remote listing for specifyed day:
    list = get_ftp_listing(server,path, verbose=verbose)
    print, server
    print, path
    print, list
    if ((strpos(list(0), 'error') ne -1) or $
        (strpos(list(0), 'empty') ne -1)) then begin
        print,'% DOWNLOAD_IMAGE: Premature exit.'
        return, 1
    endif

    ;--- First looks for a BBSO Ha image:
    w = where( strpos(list, 'bbso_halph_fr') ne -1, nw)
    if (nw gt 0) then begin
        newlist = list(w)
        ww = where(strpos(newlist, '.jpg') ne -1, nww)
        if (nww gt 0) then begin
            ;-- first trys to get the one that also has a jpg
            jpg_ha = newlist(ww(0))
            ha = strmid(jpg_ha, 0, strpos(jpg_ha,'.jpg')) + '.fts.gz'
            ;- Checks if the "artificially" composed file actually exist:
            w = where(list eq ha, nw)
            if (nw gt 0) then $
              found = 1 $
            else begin
                ;- Tries to see if a non compressed fits exist
                ha = strmid(jpg_ha, 0, strpos(jpg_ha,'.jpg')) + '.fts'
                w = where(list eq ha, nw)
                if (nw gt 0) then found = 1
            endelse
        endif
        if (not found) then begin
            ;- Else gets the last one in the list
            w = where(strpos(newlist, 'fts.gz') ne -1, nw)
            if (nw gt 0) then begin
                ha = newlist(w(0))
                found = 1
            endif
        endif
    endif

    ;--- Second looks for a KANZELHOEHE Ha image:
    if (not found) then begin
        w = where(strpos(list, 'kanz_halph_fr') ne -1, nw)
        if (nw gt 0) then begin
            newlist = list(w)
            ww = where(strpos(newlist, '.jpg') ne -1, nww)
            if (nww gt 0) then begin
                ;- first trys to get the one that also has a jpg
                jpg_ha = newlist(ww(0))
                ha = strmid(jpg_ha, 0, strpos(jpg_ha,'.jpg')) + '.fts.gz'
                ;- Checks if the "artificially" composed file actually exist:
                w = where(list eq ha, nw)
                if (nw gt 0) then found = 1
            endif
            if (not found) then begin
                ;- else gets the last one in the list
                w = where(strpos(newlist, 'fts.gz') ne -1, nw)
                if (nw gt 0) then begin
                    ha = newlist(w(0))
                    found = 1
                endif
            endif
        endif
    endif

    ;--- Makes a last attempt to see if there are any fts.gz images usable:
    ;if (not found) then begin
    ;    w = where(strpos(list, 'fts.gz') ne -1, nw)
    ;    if (nw gt 0) then begin
    ;        ha = list(w(0))
    ;        found = 1
    ;    endif
    ;endif

    ;--- Selects only the H-alpha images that are either from BBSO or KANZ:
    ;w = where( (strpos(list, 'bbso_halph_fr') ne -1) or $
    ;           (strpos(list, 'kanz_halph_fr') ne -1), nfl)
    ;if (nfl lt 1) then found = 0
    ;list = list(w)

endif

if (keyword_set(kanz)) then begin
    server = 'http://cesar.kso.ac.at'
    serv_nam = 'KANZ'

    if verbose then print,'Looking for images on KANZELHOEHE server:'

    ;--- Builds the path in kanz file system:
    ;path = 'halpha2k/recent/'+string(date(0),format='(I4)')+'/'
    path = 'halpha4M/FITS/normal/'+string(date(0),format='(I4)')+'/'

    ;--- Getting remote listing for specifyed year:
    list = get_ftp_listing(server,path, verbose=verbose)
    nlist = strarr(n_elements(list))
    ; ARD strip out gzipped fits filenames from list
    for i=0, n_elements(list)-1 do begin
      nlist[i] = strmid(list[i], strpos(list[i], 'kanz'), strlen(list[i]))
    end
    if ((strpos(list(0), 'error') ne -1) or $
        (strpos(list(0), 'empty') ne -1)) then begin
        print,'% DOWNLOAD_IMAGE: Premature exit.'
        return, 1
    endif

;    w= where(strpos(list,make_datestring(date)) ne -1, nw)
    w= where(strpos(nlist,make_datestring(date)) ne -1, nw)

    if (nw gt 0) then begin
        ha = nlist(w(0))
        found = 1
    endif
endif

if (found) then begin
    if (verbose) then print,'    Found H_alpha image: "',ha,'"'
endif else begin
    if (verbose) then print,'    No H_alpha images found on ',serv_nam,$
      ' server for date ', strdate
    return, 1
endelse
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


out_image = img_path+ha

tmp = file_search(out_image, count = n_files)
if (n_files le 0) then begin
    tmp = file_search(out_image, count = n_files)
    if (n_files gt 0) then out_image = out_image+'.gz'
endif
if (n_files gt 0) then begin
    if (verbose) then print,'    H_alpha already in file system. No download!'
    return, 0
endif

;------------- Download H_alpha image: ----------------------------------------
status = get_ftp_file(server, path+ha, out_path=img_path, $
                      verbose=verbose)

if (strpos(status,'error') ne -1) then begin
    print,'% DOWNLOAD_IMAGE: Premature exit.'
    return, 1
endif

;--- compresses the image if it is not compressed
if (strpos(ha,'.gz') le 0) then begin
    spawn, 'gzip -f '+out_image
    out_image = out_image+'.gz'
endif

return, 0

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: download_image.pro,v $
; Revision 3.14  2012/04/26 18:26:59  bernapn1
; Updated the web server for looking for Kanzelhoehe images
;
; Revision 3.13  2010/04/02 19:11:06  bernapn1
; Removed absolute path in call to gzip
;
; Revision 3.12  2010/03/08 21:21:54  bernapn1
; Improved
;
; Revision 3.11  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.10  2010/02/03 19:28:22  bernapn1
; run_analyze: Now if no Ha images found on BBSO server checking KANZELHOEHE
;   server
; download_image: simplified imput. Now have option of getting the Ha image
;   either from BBSO or from KANZELHOENE
; get_ftp_listing: handling in different ways the listing whether using the
;   BBSO or the KANZELHOEHE werver.
; get_ftp_file: a small change to be able to handle either BBSO or KANZ server
;
; Revision 3.9  2008/11/20 16:40:30  bernapn1
; Fixed a small problem with the checking of whether the image already
; exist in the file system
;
; Revision 3.8  2008/11/20 16:20:11  bernapn1
; Added logic that it downloads also files that are not gzipped
;
; Revision 3.7  2005/03/07 22:35:48  pietro
; Fixed a bug in line 162
;
; Revision 3.6  2005/03/07 22:23:15  pietro
; Changed way to find the appropriate Ha image.
;
; Revision 3.5  2004/12/30 20:39:38  pietro
; Fixed a bug related to strdate
;
; Revision 3.4  2004/12/07 20:02:50  pietro
; Changed a bit the criterions on how to find the image to download.
; Changed the algorythms that look for the file to be downloaded.
; now using make_datestring to create the date path for the remote server.
;
; Revision 3.3  2004/11/23 15:12:05  pietro
; Fixed problems form the transition from analyze_web_image
;
; Revision 3.2  2004/11/22 21:46:57  pietro
; Added log
;
; Revision 3.1  2004/11/22 21:45:25  pietro
; Created
;
; Written by Pietro N. Bernasconi JHU/APL: 05/10-13/04
