x = [0.14 0.21 0.21 0.10 0.34];


h = pie(x);

hText = findobj(h,'Type','text'); % text object handles
percentValues = get(hText,'String'); % percent values
txt = {'Same Chirality excluding U: ';'D(A)<->U(V): ';'U(A)<->D(V):';'Opposite Chirality:'; 'U<->U:'}; % strings
combinedtxt = strcat(txt,percentValues); % strings and percent values

oldExtents_cell = get(hText,'Extent'); % cell array

hText(1).String = combinedtxt(1);
hText(2).String = combinedtxt(2);
hText(3).String = combinedtxt(3);
hText(4).String = combinedtxt(4);
hText(5).String = combinedtxt(5);

oldExtents = cell2mat(oldExtents_cell); % numeric array

newExtents_cell = get(hText,'Extent'); % cell array

newExtents = cell2mat(newExtents_cell); % numeric array 
width_change = newExtents(:,3)-oldExtents(:,3);

signValues = sign(oldExtents(:,1));
offset = signValues.*(width_change/2);

textPositions_cell = get(hText,{'Position'}); % cell array
textPositions = cell2mat(textPositions_cell); % numeric array
textPositions(:,1) = textPositions(:,1) + offset; % add offset 

hText(1).Position = textPositions(1,:);
hText(2).Position = textPositions(2,:);
hText(3).Position = textPositions(3,:);
hText(4).Position = textPositions(4,:);
hText(5).Position = textPositions(5,:);
