% Script for plotting the mean chirality with latitude and time for manual
% detections of filaments
% Nov 1, 2016
% Sushant S. Mahajan

clear all;
% Load all filenames:
fid=fopen('filenames_manual.txt');

jd2000=juliandate(datetime([2000,01,01,00,00,00 ]));
jds=linspace(jd2000,jd2000+366*11,12);

for fileno=1:115
    % Get the current filename:
    oneline=fgets(fid);

    % Get the date and time and convert it to Julian Date:
    yy=str2num(oneline(2:5));
    mm=str2num(oneline(6:7));
    dd=str2num(oneline(8:9));

    hh=str2num(oneline(11:12));
    mn=str2num(oneline(13:14));
    ss=str2num(oneline(15:16));

    Date=[yy,mm,dd,hh,mn,ss];
    jd(fileno)=juliandate(datetime(Date));

    % Open the current file:
    fid2=fopen(sprintf('%s',oneline(1:20)),'r');

    % Read the first line:
    ln=fgets(fid2);
    if(ln(1:2)=='%2')
        % then get rid of the next line too (which has the labels)
        ln=fgets(fid2);
    end

    % Read the data line:
    ln=fgets(fid2);
    no=1;  % counter for the number of lines in each data file
    
    % Check if the line is non-empty:
    while(ln~=-1)
        c=strsplit(ln);
        if(isempty(str2num(cell2mat(c(1)))))
            c=c(2:end);
        end
        % Loading data:
        xp(fileno,no)=str2num(cell2mat(c(1)));
        yp(fileno,no)=str2num(cell2mat(c(2)));
        barb(fileno,no)=str2num(cell2mat(c(3)));
        nl(fileno,no)=str2num(cell2mat(c(4)));
        nr(fileno,no)=str2num(cell2mat(c(5)));
        chir(fileno,no)=str2num(cell2mat(c(6)));
        occur(fileno,no)=str2num(cell2mat(c(7)));
        no=no+1;

        ln=fgets(fid2);
    end

    % Store the length of data file for this fileno
    data_length(fileno)=no-1;

    fclose(fid2);

end

fclose(fid);


% All data loaded: now, binning it yearly

for i=1:length(jds)-1
   [tmp,filenos]=select1d(jd,jds(i),jds(i+1));
   if(filenos~=0)
       no=0;
       for j=1:length(filenos);
           xp_yr(i,no+1:no+data_length(filenos(j)))=xp(filenos(j)),1:data_length(filenos(j));
           yp_yr(i,no+1:no+data_length(filenos(j)))=yp(filenos(j)),1:data_length(filenos(j));
           barb_yr(i,no+1:no+data_length(filenos(j)))=barb(filenos(j)),1:data_length(filenos(j));
           nl_yr(i,no+1:no+data_length(filenos(j)))=nl(filenos(j)),1:data_length(filenos(j));
           nr_yr(i,no+1:no+data_length(filenos(j)))=nr(filenos(j)),1:data_length(filenos(j));
           chir_yr(i,no+1:no+data_length(filenos(j)))=chir(filenos(j)),1:data_length(filenos(j));
           occur_yr(i,no+1:no+data_length(filenos(j)))=occur(filenos(j)),1:data_length(filenos(j));
           no=no+data_length(filenos(j));
       end
       data_yrlength(i)=no;
   end
end

% Hemispheric mean chirality:

for i=1:115
    pos1=find(yp(i,:)>0);
    pos2=find(yp(i,:)<0);
    chir_hem1(i)=nanmean(chir(i,pos1));
    chir_hem2(i)=nanmean(chir(i,pos2));
    barb_hem1(i)=nanmean(barb(i,pos1));
    barb_hem2(i)=nanmean(barb(i,pos2));
end