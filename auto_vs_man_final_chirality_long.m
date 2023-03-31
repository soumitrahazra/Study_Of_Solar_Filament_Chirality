% Script for comparing automatic and manual results of filaments from
% 2000-2009:

clear all;

afid=fopen('automatic_filenames.txt');
mfid=fopen('manual_filenames.txt');

start_yr=2000;
end_yr=2016;
month=08;

start_jd=juliandate(datetime([start_yr,01,01,00,00,00 ]));
jds=linspace(start_jd, start_jd+365.24*(end_yr-start_yr+1),end_yr-start_yr+2);

for fileno=1:218
    
    % Get the names of the files:
    autofile=fgets(afid);
    manfile=fgets(mfid);
    
    if(strcmp(autofile,manfile(5:end))~=1)
        disp(strcmp(autofile,manfile(5:end)))
        error('The automatic and manual filenames are different!')
    end
    
    yy=str2double(autofile(1:4));
    mm=str2double(autofile(5:6));
    dd=str2double(autofile(7:8));

    hh=str2double(autofile(10:11));
    mn=str2double(autofile(12:13));
    ss=str2double(autofile(14:15));
    
    Date=[yy,mm,dd,hh,mn,ss];
    jd(fileno)=juliandate(datetime(Date));
    
    % Open the current files:
    fid1=fopen(sprintf('%s',autofile(1:end-1)));
    fid2=fopen(sprintf('%s',manfile(1:end-1)));
    
    for i=0:12
        line1=fgets(fid1);
        line2=fgets(fid2);
    end
 
    
    % Check for #barbs >0:
    while(line1~=-1)
        no=str2double(line1(1:4));
        area(fileno,no)=str2double(line1(6:12));
        arat(fileno,no)=str2double(line1(14:18));
        xp(fileno,no)=str2double(line1(19:23));
        yp(fileno,no)=str2double(line1(24:28));
        lat(fileno,no)=str2double(line1(31:36));
        lon(fileno,no)=str2double(line1(37:43));
        ang(fileno,no)=str2double(line1(45:50));
        len(fileno,no)=str2double(line1(52:55));
        nbarbs1(fileno,no)=str2double(line1(57:60));  % Auto
        nbarbs2(fileno,no)=str2double(line2(57:60));  % Man
        nrite1(fileno,no)=str2double(line1(61:64));   % Auto
        nleft1(fileno,no)=str2double(line1(65:68));   % Auto
        %chir1_read(fileno,no)=str2double(line1(69:72));    % Auto
        nleft2(fileno,no)=str2double(line2(61:64));   % Man
        nrite2(fileno,no)=str2double(line2(65:68));   % Man
        %chir2_read(fileno,no)=str2double(line2(69:72));    % Man
        
        % Calculate the Chirality from Auto and Man:
        diffbarbs=nrite1(fileno,no)-nleft1(fileno,no);
        if(abs(diffbarbs)>1)
            if(diffbarbs>1)
                chir1(fileno,no)=1;  % Dextral in Auto
            else
                chir1(fileno,no)=-1; % Sinistral in Auto
            end
        else
            chir1(fileno,no)=0;
        end
        
        % The definitions of barbs in Soumitra's Manual results are
        % flipped:
        diffbarbs=nrite2(fileno,no)-nleft2(fileno,no);
        if(abs(diffbarbs)>1)
            if(diffbarbs>1)
                chir2(fileno,no)=1;  % Dextral in Man
            else
                chir2(fileno,no)=-1; % Sinistral in Man
            end
        else
            chir2(fileno,no)=0;
        end

   % Calculate the Chirality from Auto and Man by t-test:
        diffbarbs=nrite1(fileno,no)-nleft1(fileno,no);
        p=0.5;
        if(abs(diffbarbs)>1)
           ss(fileno,no)= sqrt(nbarbs1(fileno,no)*p*(1-p));
           sg(fileno,no)= nrite1(fileno,no)- p*nbarbs1(fileno,no);
           t_auto = sg(fileno,no)/ss(fileno,no);
           t_thres = (3/(4*sqrt(nbarbs1(fileno,no)))) +0.5;
      
            if (t_auto > t_thres)
                chir1t(fileno,no)= 1;  % Dextral in Auto
            elseif (t_auto < - t_thres)
                chir1t(fileno,no)= -1; % Sinistral in Auto
            else
            chir1t(fileno,no)=0;
          end
         else
         chir1t(fileno,no)=0;
        end
        
        % The definitions of barbs in Soumitra's Manual results are
        % flipped
        p=0.5;
        diffbarbs =nrite2(fileno,no)-nleft2(fileno,no);
        if(abs(diffbarbs)>1)
          ssm(fileno,no)= sqrt(nbarbs2(fileno,no)*p*(1-p));
           sgm(fileno,no)= nrite2(fileno,no)- p*nbarbs2(fileno,no);
           t_man = sgm(fileno,no)/ssm(fileno,no);
           t_thresm = (3/(4*sqrt(nbarbs2(fileno,no)))) +0.5;

            if(t_man > t_thresm)
                chir2t(fileno,no)= 1;  % Dextral in Man
            elseif (t_man < - t_thresm)
                chir2t(fileno,no)= -1; % Sinistral in Man
            else
            chir2t(fileno,no)=0;
           end
        else
       chir2t(fileno,no)=0;
      end
        
  % Get the next lines from the files:
        line1=fgets(fid1);
        line2=fgets(fid2);
    end
    
    nlen(fileno)=no;  % Number of recorded filaments in every file
    fclose(fid1);
    fclose(fid2);
    
end

fclose(afid);
fclose(mfid);

length(chir1t)
length(chir2t)

% All data loaded, now binning it yearly:

for i = 1:length(jds)-1
   [tmp,filenos]=select1d(jd,jds(i),jds(i+1));
   if(filenos~=0)
      no=0;
      for j=1:length(filenos)
       area_yr(i,no+1:no+nlen(filenos(j)))=area(filenos(j),1:nlen(filenos(j))); 
       arat_yr(i,no+1:no+nlen(filenos(j)))=arat(filenos(j),1:nlen(filenos(j))); 
       xp_yr(i,no+1:no+nlen(filenos(j)))=xp(filenos(j),1:nlen(filenos(j))); 
       yp_yr(i,no+1:no+nlen(filenos(j)))=yp(filenos(j),1:nlen(filenos(j))); 
       lat_yr(i,no+1:no+nlen(filenos(j)))=lat(filenos(j),1:nlen(filenos(j))); 
       lon_yr(i,no+1:no+nlen(filenos(j)))=lon(filenos(j),1:nlen(filenos(j))); 
       ang_yr(i,no+1:no+nlen(filenos(j)))=ang(filenos(j),1:nlen(filenos(j))); 
       len_yr(i,no+1:no+nlen(filenos(j)))=len(filenos(j),1:nlen(filenos(j))); 
       nbarbs1_yr(i,no+1:no+nlen(filenos(j)))=nbarbs1(filenos(j),1:nlen(filenos(j))); 
       nrite1_yr(i,no+1:no+nlen(filenos(j)))=nrite1(filenos(j),1:nlen(filenos(j))); 
       nleft1_yr(i,no+1:no+nlen(filenos(j)))=nleft1(filenos(j),1:nlen(filenos(j))); 
       chir1_yr(i,no+1:no+nlen(filenos(j)))= chir1(filenos(j),1:nlen(filenos(j))); 
       nbarbs2_yr(i,no+1:no+nlen(filenos(j)))=nbarbs2(filenos(j),1:nlen(filenos(j))); 
       nrite2_yr(i,no+1:no+nlen(filenos(j)))=nrite2(filenos(j),1:nlen(filenos(j))); 
       nleft2_yr(i,no+1:no+nlen(filenos(j)))=nleft2(filenos(j),1:nlen(filenos(j))); 
       chir2_yr(i,no+1:no+nlen(filenos(j)))=chir2(filenos(j),1:nlen(filenos(j))); 
       chir1t_yr(i,no+1:no+nlen(filenos(j)))=chir1t(filenos(j),1:nlen(filenos(j)));
       chir2t_yr(i,no+1:no+nlen(filenos(j)))=chir2t(filenos(j),1:nlen(filenos(j)));
       no=no+nlen(filenos(j));
      end
      ndata_yr(i)=no;
   end
end

% Calculating Hemispheric Chirality
for i=1:length(jds)-1
    pos1=find(xp_yr(i,:)>0);  % western Hemisphere
    pos2=find(xp_yr(i,:)<0);  % eastern Hemisphere
    chir1_hem1(i)=mean(chir1_yr(i,pos1));
    chir1_hem2(i)=mean(chir1_yr(i,pos2));
    chir2_hem1(i)=mean(chir2_yr(i,pos1));
    chir2_hem2(i)=mean(chir2_yr(i,pos2));
end

% Calculating Hemispheric Chirality
for i=1:length(jds)-1
 
     
      for j=1:length(yp_yr)

    if (yp_yr(i,j)<0)  % nortern Hemisphere
     chir1n_yr(i,j)= chir1_yr(i,j);
     chir2n_yr(i,j)= chir2_yr(i,j);
     chir1tn_yr(i,j) = chir1t_yr(i,j);
     chir2tn_yr(i,j)= chir2t_yr(i,j);
   else
  chir1n_yr(i,j)= 0;
  chir2n_yr(i,j)=0;
  chir1tn_yr(i,j)=0;
  chir2tn_yr(i,j)=0;
 end 
 
end 
end


% Calculating Matches/mismatches excluding undetermined:

chirm1=chir1_yr(find(chir1_yr~=0 | chir2_yr~=0));
chirm2=chir2_yr(find(chir1_yr~=0 | chir2_yr~=0));
chirm3=chir1_yr(find(chir1_yr==-1 & chir2_yr==1));
chirm4=chir1_yr(find(chir1_yr==1 & chir2_yr==-1));
disp(sprintf('Matching results= %f percent',100*length(chirm3)/length(chirm1)))
disp(sprintf('Matching results= %f percent',100*length(find(chirm1==chirm2))/length(chirm1)))
disp(sprintf('Filaments which are opposite= %f percent',100*length(find(chir1_yr.*chir2_yr<0))/length(chirm1)))
disp(sprintf('Filaments which changes 0 to 1= %f percent',100*length(find(chirm1.*chirm2==0))/length(chirm1)))
% Calculating how many filaments match including undetermined:
matches=0;
for i=1:length(jds)-2
    matches=matches+length(find(chir1_yr(i,1:ndata_yr(i))==chir2_yr(i,1:ndata_yr(i))));
end
disp(sprintf('Complete Matches(including 0s)= %f percent',100*matches/sum(ndata_yr)))

% Plotting chirality of filaments with latitude and time:

colors='rwb';

figure
hold on
for i=1:length(jds)-1
    for j=1:ndata_yr(i)
        if(chir1_yr(i,j)~=0) % Filtering out undetermined filaments
            plot((jds(i)-jds(1))/365.24+8/12+2000,xp_yr(i,j),sprintf('o%s',colors(chir1_yr(i,j)+2)),'linewidth',2);
        end
    end
end

% Binning in Yp:
binedges=linspace(-800, 800,10);

for i=1:length(jds)-1
    for k=1:length(binedges)-1
        [data,pos]=select1d(xp_yr(i,1:ndata_yr(i)),binedges(k),binedges(k+1));
        if(pos~=0)
            chirality1(i,k)=sum(chir1n_yr(i,pos));
            chirality2(i,k)=sum(chir2n_yr(i,pos));
            chirality1t(i,k)=sum(chir1tn_yr(i,pos));
            chirality2t(i,k)=sum(chir2tn_yr(i,pos));
            dexstr1(i,k) = length(find(chir1_yr(i,pos)==1));
            sinstr1(i,k) = length(find(chir1_yr(i,pos)==-1));
            strength1(i,k) = dexstr1(i,k)-sinstr1(i,k);
            dexstr2(i,k) = length(find(chir2_yr(i,pos)==1));
            sinstr2(i,k) = length(find(chir2_yr(i,pos)==-1));
            strength2(i,k) = dexstr2(i,k)-sinstr2(i,k);
            dexstr1t(i,k) = length(find(chir1t_yr(i,pos)==1));
            sinstr1t(i,k) = length(find(chir1t_yr(i,pos)==-1));
            strength1t(i,k) = dexstr1t(i,k)-sinstr1t(i,k);
            dexstr2t(i,k) = length(find(chir2t_yr(i,pos)==1));
            sinstr2t(i,k) = length(find(chir2t_yr(i,pos)==-1));
            strength2t(i,k) = dexstr2t(i,k)-sinstr2t(i,k);
            determ_auto(i,k) = length(find(chir1_yr(i,pos)~=0));
            determ_autot(i,k) = length(find(chir1t_yr(i,pos)~=0));
            determ_manu(i,k) = length(find(chir2_yr(i,pos)~=0));
            determ_manut(i,k) = length(find(chir2t_yr(i,pos)~=0));
            undeterm_auto(i,k) = length(find(chir1_yr(i,pos)==0));
            undeterm_autot(i,k) = length(find(chir1t_yr(i,pos)==0));
            undeterm_manu(i,k) = length(find(chir2_yr(i,pos)==0));
            undeterm_manut(i,k) = length(find(chir2t_yr(i,pos)==0));
            du_auto(i,k)= determ_auto(i,k)-undeterm_auto(i,k);
            du_autot(i,k)= determ_autot(i,k)- undeterm_autot(i,k);
            du_manu(i,k)= determ_manu(i,k)- undeterm_manu(i,k);
            du_manut(i,k)= determ_manut(i,k)- undeterm_manut(i,k);
        else
            chirality1(i,k)=0;
            chirality2(i,k)=0;
            chirality1t(i,k)=0;
            chirality2t(i,k)=0;
            dexstr1(i,k) = 0;
            sinstr1(i,k) = 0;
            strength1(i,k) = 0;
            dexstr2(i,k) = 0;
            sinstr2(i,k) = 0;
            strength2(i,k) = 0;
            dexstr1t(i,k) = 0;
            sinstr1t(i,k) = 0;
            strength1t(i,k) = 0;
            dexstr2t(i,k) = 0;
            sinstr2t(i,k) = 0;
            strength2t(i,k) = 0;
            determ_auto(i,k) = 0;
            determ_autot(i,k) = 0;
            determ_manu(i,k) = 0;
            determ_manut(i,k) = 0;
            undeterm_auto(i,k) = 0;
            undeterm_autot(i,k) = 0;
            undeterm_manu(i,k) = 0;
            undeterm_manut(i,k) = 0;
            du_auto(i,k)=0;
            du_autot(i,k) = 0;
            du_manu(i,k) = 0;
            du_manut(i,k) = 0;
        end
    end
end

figure
hold on
subplot(4,1,1)
surf((binedges(1:end-1)+100)*(90)/918, (jds(1:end-1)-jds(1))/365.24+2000+8/12, chirality1);
xlabel('Longitude (deg)','FontSize',15)
xlim([-70 70])
ylabel('Time (years)','FontSize',15)
ylim([2000.6 2016.8])
[cmap]=cbrewer('div','PRGn',100);
%colormap(magma(256));
colormap(jet)
%colorbar;
title('(a)')
caxis([-1 1])
view(90,270)
subplot(4,1,2)
surf((binedges(1:end-1)+100)*90/918,(jds(1:end-1)-jds(1))/365.24+2000+8/12,chirality2);
xlabel('Longitude (deg)','FontSize',15)
xlim([-70 70])
ylabel('Time (years)','FontSize',15)
ylim([2000.6 2016.8])
[cmap]=cbrewer('div','PRGn',100);
%colormap(magma(256));
colormap(jet)
title('(b)')
caxis([-1 1])
%colorbar;
view(90,270)
subplot(4,1,3)
surf((binedges(1:end-1)+100)*(90)/918, (jds(1:end-1)-jds(1))/365.24+2000+8/12, chirality1t);
xlabel('Longitude (deg)','FontSize',15)
xlim([-70 70])
ylabel('Time (years)','FontSize',15)
ylim([2000.6 2016.8])
[cmap]=cbrewer('div','PRGn',100);
%colormap(magma(256));
colormap(jet)
%colorbar;
title('(c)')
caxis([-1 1])
view(90,270)
subplot(4,1,4)
surf((binedges(1:end-1)+100)*90/918,(jds(1:end-1)-jds(1))/365.24+2000+8/12,chirality2t);
xlabel('Longitude (deg)','FontSize',15)
xlim([-70 70])
ylabel('Time (years)','FontSize',15)
ylim([2000.6 2016.8])
[cmap]=cbrewer('div','PRGn',100);
%colormap(magma(256));
colormap(jet)
%colorbar;
title('(d)')
caxis([-1 1])
view(90,270)

% Calculate the accuracy of the Automatic detection algorithm (better calculation now, after understanding the dataset)
no=1;
for i=1:length(jds)-1
    for j=1:ndata_yr(i)
       if(nbarbs1_yr(i,j)>1 && nbarbs2_yr(i,j)>1) % Filtering out filaments for which chirality rule is not applicable
           chir_auto(no)=chir1_yr(i,j);
           chir_man(no)=chir2_yr(i,j);
           no=no+1;
       end
    end
end

matches(1)=length(find(chir_auto==1 & chir_man==1));  % 1 to 1
matches(1)=matches(1)+length(find(chir_auto==-1 & chir_man==-1));  % -1 to -1
matches(2)=length(find(chir_auto==0 & abs(chir_man)==1));  % 0 to 1 & 0 to -1
matches(3)=length(find(abs(chir_auto)==1 & chir_man==0));  % 1 to 0 & -1 to 0
matches(4)=length(find((chir_auto.*chir_man)==-1));  % 1 to -1 & -1 to 1
matches(5)=length(find(chir_auto==0 & chir_man==0));  % 0 to 0

matches=matches/length(chir_auto);
