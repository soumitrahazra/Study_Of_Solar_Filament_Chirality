;******************************************************************************
;***                                                                        ***
      function get_ftp_file, servernami, pathi, OUT_PATH=out_path, $
                             VERBOSE=verbose, SHOW_LINUX = show_linux
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    GET_FTP_FILE
;
; PURPOSE:
;    This routine retrieves a file from an ftp site
;
; CALLING SEQUENCE:
;    status = get_ftp_file(server_name [, path, OUT_PATH=out_path, $
;                          /VERBOSE, /SHOW_LINUX ])
;
; INPUTS:
;    server_name = string: name of the remote ftp serevr, optionally
;                     followed by the file path name. Examples:
;                       ftp.bbso.njit.edu
;                       ftp.bbso.njit.edu/pub/archive/2004/05/05/image.fts
;
; OPTIONAL INPUT PARAMETERS:
;    path = string: If the path and file name are not given in the first
;              argument then here give the path. Examples:
;              `pub/archive/2004/05/05/image.fts',
;              `/pub/archive/2004/05/05/image.fts',
;
; KEYWORD PARAMETERS:
;    OUT_PATH = string: Directory path where the file will be saved.
;                    Default = './'
;    VERBOSE = flag: If set then some additional information is dispalyed
;                    on the screen.
;    SHOW_LINUX = flag: If set then it prints out the linux command to get
;                    the listing from remote server
;
; OUTPUTS:
;    status = string: 'ok' if the download was succesfull,
;                     'error:  '+errmessage if it was NOT!
;
;    The retrieved file is saved in the appropriate local directory
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES CALLED:
;    
;
; PROCEDURE:
;    Uses the following GNU linux command to retrieve the listing:
;        wget -nr ftp://remote_server/remote_directory_path/file.suf
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 1) then $
  message,'Name of remote ftp server missig (1st parameter)'
if (size(servernami,/type) ne 7) then $
  message,'1st parameter (server_name) must be a string!'

if (n_elements(out_path) gt 0) then begin
    if (size(out_path, /type) ne 7) then begin
        print,'% GET_FTP_FILE: OUT_PATH is not a string!'
        print,'                reset to local directory'
        out_path_cmd = ''
    endif else begin
        tmp = file_search(out_path, count = n_dirs)
        if (n_dirs le 0) then begin
            print,'% GET_FTP_FILE: OUT_PATH "',out_path,'" does not exist!'
            print,'                reset to local directory'
            out_path_cmd = ''
        endif else begin
            out_path_cmd = '-P '+out_path
        endelse
    endelse
endif else out_path_cmd = ''

if (n_elements(verbose) le 0) then verbose=0
if (n_elements(show_linux) le 0) then show_linux=0

status = 'ok'

;--- Removes back-lashes at beginning of server_name:
servernam=servernami
while (strpos(servernam, '/') eq 0) do servernam = strmid(servernam, 1)

;--- Builds the first part of the command:
wget_str = 'wget '+out_path_cmd+' '+servernam

;--- Adds an extra path if requested:
if (n_elements(pathi) gt 0) then begin
    if (size(pathi,/type) ne 7) then $
      message,'2nd parameter (path) must be a string!'

    path=pathi

    ;--- Removes back-lashes at beginning of path:
    while (strpos(path, '/') eq 0) do path = strmid(path, 1)

    wget_str = wget_str + '/' +path
endif

if verbose then print,$
  format='("    Downloading file from server ... ",$)'
if show_linux then begin
    if (verbose) then pring
    print,'      Executing shell command: ',wget_str
endif

;--- Issue the wget command:
wget_str = wget_str + '>& message.tmp'
spawn, wget_str
if (verbose) then print,'DONE!'

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
if (strpos(s,'saved') eq -1) then begin
    print,'% GET_FTP_FILE: dowload FAILED!'
    print,'      ',s
    status = 'error:  '+s
endif

return, status
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_ftp_file.pro,v $
; Revision 3.6  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.5  2010/04/02 19:11:06  bernapn1
; Removed absolute path in call to gzip
;
; Revision 3.4  2010/03/08 20:40:22  bernapn1
; changed from "/usr/bin/wget" to simply "wget"
;
; Revision 3.3  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.2  2010/02/03 19:28:22  bernapn1
; run_analyze: Now if no Ha images found on BBSO server checking KANZELHOEHE
;   server
; download_image: simplified imput. Now have option of getting the Ha image
;   either from BBSO or from KANZELHOENE
; get_ftp_listing: handling in different ways the listing whether using the
;   BBSO or the KANZELHOEHE werver.
; get_ftp_file: a small change to be able to handle either BBSO or KANZ server
;
; Revision 3.1  2004/11/22 19:39:27  pietro
; Removed the C_PATH keyword and added a call to file_exist.pro
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.1  2004/07/22 13:46:29  pietro
; MAde some changes ...
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: 05/06/04
