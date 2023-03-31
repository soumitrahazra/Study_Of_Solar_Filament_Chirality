;******************************************************************************
;***                                                                        ***
        function get_ftp_listing, servernami, pathi, VERBOSE=verbose,$
                                  SHOW_LINUX = show_linux
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    GET_FTP_LISTING
;
; PURPOSE:
;    This function retrieves the list of files contained in a remote ftp
;    directory. The server name and path can be given as the first
;    parameter, or the path can be given in a second input parameter
;
; CALLING SEQUENCE:
;    list = get_ftp_listing(server_name [, path, /VERBOSE, /SHOW_LINUX ])
;
; INPUTS:
;    server_name = string: name of the remote ftp serevr, optionally
;                     followed by the path name. Examples:
;                       ftp.bbso.njit.edu
;                       ftp.bbso.njit.edu/pub/archive/2004/05/05/
;
; OPTIONAL INPUT PARAMETERS:
;    path = string: If the path is not given in the first argument then
;              here give the path. Examples: `pub/archive/2004/05/05/',
;              `/pub/archive/2004/05/05/', `pub/archive/2004/05/05'
;
; KEYWORD PARAMETERS:
;    VERBOSE = flag: If set then some additional information is dispalyed
;                    on the screen
;    SHOW_LINUX = flag: If set then it prints out the linux command to get
;                    the listing from remote server
;
; OUTPUTS:
;    list = strarr(n_files): list of all the files contained in the remote
;              ftp directory. Returns "error" if an error occurred in the
;              remote retrieval, or "empty" if the directory is valid but
;              empty.
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; PROCEDURE:
;    Uses the following GNU linux command to retrieve the listing:
;        wget -nr ftp://remote_server/remote_directory_path/
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 1) then $
  message,'Name of remote ftp server missig (1st parameter)'
if (size(servernami,/type) ne 7) then $
  message,'1st parameter (server_name) must be a string!'

if (n_elements(verbose) le 0) then verbose=0
if (n_elements(show_linux) le 0) then show_linux=0

;--- Removes back-lashes at beginning of server_name:
servernam=servernami
while (strpos(servernam, '/') eq 0) do servernam = strmid(servernam, 1)
;--- Adds a backlash at the end if not there:
if (strmid(servernam, 0, /rev) ne '/') then servernam = servernam+'/'

;--- Builds the first part of the command:
wget_str = 'wget --no-remove-listing '+servernam

;--- Adds an extra path if requested:
if (n_elements(pathi) gt 0) then begin
    if (size(pathi,/type) ne 7) then $
      message,'2nd parameter (path) must be a string!'

    path=pathi

    ;--- Removes back-lashes at beginning of path:
    while (strpos(path, '/') eq 0) do path = strmid(path, 1)
    ;--- Adds a backlash at the end if not there:
    if (strmid(path, 0,/rev) ne '/') then path = path+'/'

    wget_str = wget_str + path
endif

if verbose then print,'    Getting listing from server ...'
if show_linux then print,'      Executing shell command: ',wget_str

;--- Issue the wget command:
wget_str = wget_str + '>& message.tmp'
spawn, wget_str

;--- Checks the message.tmp to make sure that everything went fine
s=''
openr,index,'message.tmp',/get_lun
while (EOF(index) le 0) do begin
    s1 = s
    readf,index,s
endwhile
close,index  &  free_lun,index
file_delete,'message.tmp'
if (s eq '') then s=s1

if ( (strpos(s,'Wrote HTML-ized index to') eq -1) and $
     (strpos(s,'saved') eq -1) ) then begin
    print,'% GET_FTP_LISTING: problem encountered getting listing:'
    print,'      '+s
    return, 'error:  '+s
endif

;--- Get the correct name of the index.html.x and checks if it exists
index_str = strmid(s,strpos(s,'`')+1,strpos(s,"'")-strpos(s,'`')-1)
tmp = findfile(index_str, count=n_files)

;--- Extracts all the file names ----------------------------------------------
max_lines = 50
f_list = strarr(max_lines)
if (strpos( servernam,'ftp') ne -1) then begin
   ;--- For FTP sites: Read the file ".listing"
    openr,index,'.listing',/get_lun
    if (EOF(index) le 0) then begin
        readf,index,s
        nlines = 0
        while (EOF(index) le 0) do begin
            if (nlines eq max_lines) then begin
                tmp = f_list
                max_lines = max_lines + 20
                f_list = strarr(max_lines)
                f_list(0:nlines-1) = tmp
            endif
            
            readf,index,s
            f_list(nlines) = strmid(s, strpos(s, ' ',/reverse_s)+1)
            nlines = nlines +1
        endwhile
    endif else nlines = 0

    close,index  &  free_lun,index
    ;spawn,'/bin/rm .listing'
    file_delete,'.listing'
endif else begin
    ;--- For other sites, typically HTTP: Read the index.html file:
    openr,index,index_str,/get_lun
    nlines = 0
    while (EOF(index) le 0) do begin
        readf,index,s
        endpos = strpos(s,'.fts.gz')
        if (endpos eq -1) then continue

        if (nlines eq max_lines) then begin
            tmp = f_list
            max_lines = max_lines + 50
            f_list = strarr(max_lines)
            f_list(0:nlines-1) = tmp
        endif
        strtpos = strpos(s,'HREF="')+6
        endpos = endpos+7
        f_list(nlines)  = strmid(s,strtpos,endpos-strtpos)
        nlines = nlines+1
    endwhile
    close,index  &  free_lun,index
endelse
;------------------------------------------------------------------------------

;--- Removes the index.htm file if it exists:
;if (n_files ne 0) then spawn,'/bin/rm '+index_str
if (n_files ne 0) then file_delete,index_str

if (nlines eq 0) then begin
    if verbose then print,'    Remote directory empty!'
    return, 'empty'
endif

if verbose then print,'    Total of ',strcompress(string(nlines),/rem),$
  ' files in remote directory.'

return, f_list(0:nlines-1)
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_ftp_listing.pro,v $
; Revision 3.11  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.10  2010/03/08 20:41:00  bernapn1
; Changed from "/usr/bin/wget" to simply "wget"
;
; Revision 3.9  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.8  2010/02/03 19:28:22  bernapn1
; run_analyze: Now if no Ha images found on BBSO server checking KANZELHOEHE
;   server
; download_image: simplified imput. Now have option of getting the Ha image
;   either from BBSO or from KANZELHOENE
; get_ftp_listing: handling in different ways the listing whether using the
;   BBSO or the KANZELHOEHE werver.
; get_ftp_file: a small change to be able to handle either BBSO or KANZ server
;
; Revision 3.7  2008/11/07 14:42:29  bernapn1
; Updated
;
; Revision 3.4  2006/08/29 15:09:34  bernapn1
; updated the command for wget with option: --no-remove-listing
;
; Revision 3.3  2004/11/23 15:24:14  pietro
; Fixed a bug when deleting the index.html file
;
; Revision 3.2  2004/11/23 15:12:27  pietro
; Changed some comments outputs
;
; Revision 3.1  2004/11/22 19:40:05  pietro
; Removed C_PATH keyword and added call fo dir_exist.pro
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
; Written by Pietro N. Bernasconi JHU/APL: 05/06/04
